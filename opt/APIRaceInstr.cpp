#define DEBUG_TYPE "apirace"
#include "llvm/Pass.h"
#include "llvm/Function.h"
#include "llvm/IRBuilder.h"
#include "llvm/Metadata.h"
#include "llvm/Module.h"
#include "llvm/Type.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/DataLayout.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/SmallString.h"               // class SmallString
#include "llvm/ADT/SmallSet.h"                  // class SmallSet
#include "llvm/ADT/StringExtras.h"              // itostr
#include "llvm/Transforms/Utils/ModuleUtils.h"  // appendToGlobalCtors
#include <map>
using namespace llvm;

namespace {

struct APIRace : public FunctionPass {
    static char ID;
    APIRace();
    const char *getPassName() const;
    virtual bool runOnFunction(Function &F);
    virtual bool doInitialization(Module &M);

  private:
    DataLayout *TD;
    NamedMDNode *MD;

    // user-defined annotations
    typedef std::map<std::string, std::string> AnnotationMap;
    AnnotationMap AT;  // [ FunName : Annotation ]
    bool getFuncAnnotations(Module &M);

    // utility for instrumentation
    void groupAllUsedMembers( StringRef &TargetClassName,
        SmallVectorImpl<Instruction*> &LocalGetElementPtrs, SmallSet<Value*, 8> &AllUsedMembers );
    void groupLoadedAndStoredMembers( StringRef &TargetClassName, 
        SmallVectorImpl<Instruction*> &LocalLoadAndStores,
        std::set<Value*> &AllLoadedMembers, std::set<Value*> &AllStoredMembers );
    bool instrumentLoadedAndStoredMemberInFunction( Function &F, 
        std::set<Value*> &AllLoadedMembers, std::set<Value*> &AllStoredMembers );


    // TSAN: original tsan rtl functions
    Function *TsanFuncEntry;
    Function *TsanFuncExit;
    static const size_t kNumberOfAccessSizes = 5;   // Accesses sizes are powers of two: 1, 2, 4, 8, 16
    Function *TsanRead[kNumberOfAccessSizes];
    Function *TsanWrite[kNumberOfAccessSizes];
    Function *TsanAtomicLoad[kNumberOfAccessSizes];
    Function *TsanAtomicStore[kNumberOfAccessSizes];
    Function *TsanVptrUpdate;

};

}

/**
 * Extract the class name from the given PointerType. 
 * OUTPUT: if is a class, return its name in format of "class.XXXX"; 
 *         otherwise, return "";
 *
 * NOTE: given a class with name A, in llvm, a StructType with name "class.A" is 
 * declared to represent class A, like
 *                
 * %class.A = type { ... , ... }
 *
 */
static StringRef getClassName(Type *PT) {
    if (!isa<PointerType>(PT)) return StringRef("");
    Type * baseTy = cast<PointerType>(PT)->getElementType();
    if (!isa<StructType>(baseTy)) return StringRef("");
    StructType * ST = cast<StructType>(baseTy);
    StringRef className = ST->getName();
    static StringRef prefix = StringRef("class.");
    if (!className.startswith(prefix)) return StringRef("");
    return className;
}

static StringRef getClassNameOfMemberFunc(Function &F) {
    Function::arg_iterator argIt = F.arg_begin();
    Value * firstArg = argIt;                                       // get 1st argument
    Type * typeOfFirstArg = firstArg->getType();                    //     its type
    return getClassName(typeOfFirstArg);
}

static bool isSyncCall(Instruction *I) {
    static const char * syncCallList[] = {
        "pthread_join",
        "pthread_barrier_wait",
        // TODO: add other synchronization functions in Pthreads API
    };
    static int numOfSyncCalls = sizeof(syncCallList)/sizeof(syncCallList[0]);

    CallInst *CI = cast<CallInst>(I);
    Function *F = CI->getCalledFunction();
    StringRef funcName = F->getName();
    
    for (int i=0; i<numOfSyncCalls; ++i) {
        if (funcName == syncCallList[i]) {
            errs() << "isSyncCall: found a call to \'" << funcName << "\' in instruction \"" << *I << "\"\n";
            return true;
        }
    }

    return false;
}




char APIRace::ID = 0;
static RegisterPass<APIRace> X("apirace",
    "APIRace: detect api data races.",
    false, false );

const char *APIRace::getPassName() const {
    return "APIRace";
}

APIRace::APIRace() 
    : FunctionPass(ID), TD(NULL) {
}

/** read the llvm.global.annotations
 *  'llvm.global.annotations' is a GlobalVariable containing all the function level
 *  annotations. Its operand is an array of type {i8*, i8*, i8*, i32}.
 *  i8*  : a constant expression, the GV being annotated
 *  i8*  : a constant string of anotation
 *  i8*  : a constatn string containing the name of the translation unit
 *  i32  : the line number in the file
 */

bool APIRace::getFuncAnnotations(Module &M) {
    GlobalVariable *GV;
    if ( (GV = M.getGlobalVariable( "llvm.global.annotations" )) == NULL ) 
        return false; 
    //errs() << "GET: " << *GV  << '\n';

    ConstantArray *CA = dyn_cast<ConstantArray>(GV->getOperand(0));
    int numAnnotations = CA->getNumOperands();
    //errs() << "CA: " << numAnnotations << '\n'; 

    // loop each Annotation entry (ConstantStruct) and store info to AT
    for (int i=0; i<numAnnotations; ++i) {
        ConstantStruct *entry = dyn_cast<ConstantStruct>(CA->getOperand(i));
        assert( entry->getNumOperands() == 4 );
        
        Value *fn   = entry->getOperand(0)->getOperand(0);
        Constant *str = dyn_cast<Constant> (entry->getOperand(1)->getOperand(0));
        ConstantDataSequential *anno = dyn_cast<ConstantDataSequential> (str->getOperand(0));
        //errs() << i << ": " << fn->getName() << " , " << anno->getAsCString() << '\n';
        AT[fn->getName()] = anno->getAsCString();
    }

    return true;
}

static Function *checkInterfaceFunction(Constant *FuncOrBitcast) {
    if (Function *F = dyn_cast<Function>(FuncOrBitcast))
        return F;
    FuncOrBitcast->dump();
    report_fatal_error("ThreadSanitizer interface function redefined");
}

bool APIRace::doInitialization(Module &M) {
    // get user-defined annotation, if any
    if (!getFuncAnnotations(M)) return false;

    // TSAN: Always insert a call to __tsan_init into the module's CTORs. 
    IRBuilder<> IRB(M.getContext());
    Value *TsanInit = M.getOrInsertFunction("__tsan_init",
        IRB.getVoidTy(), NULL);
    appendToGlobalCtors(M, cast<Function>(TsanInit), 0);

    // TSAN: Initialize the tsan rtl functions
    TsanFuncEntry = checkInterfaceFunction(M.getOrInsertFunction(
        "__tsan_func_entry", IRB.getVoidTy(), IRB.getInt8PtrTy(), NULL));
    TsanFuncExit = checkInterfaceFunction(M.getOrInsertFunction(
        "__tsan_func_exit", IRB.getVoidTy(), NULL));
    IntegerType *OrdTy = IRB.getInt32Ty();
    for (size_t i = 0; i < kNumberOfAccessSizes; ++i) {
        const size_t ByteSize = 1 << i;
        const size_t BitSize = ByteSize * 8;
        SmallString<32> ReadName("__tsan_read" + itostr(ByteSize));
        TsanRead[i] = checkInterfaceFunction(M.getOrInsertFunction(
            ReadName, IRB.getVoidTy(), IRB.getInt8PtrTy(), NULL));

        SmallString<32> WriteName("__tsan_write" + itostr(ByteSize));
        TsanWrite[i] = checkInterfaceFunction(M.getOrInsertFunction(
            WriteName, IRB.getVoidTy(), IRB.getInt8PtrTy(), NULL));

        Type *Ty = Type::getIntNTy(M.getContext(), BitSize);
        Type *PtrTy = Ty->getPointerTo();
        SmallString<32> AtomicLoadName("__tsan_atomic" + itostr(BitSize) + "_load");
        TsanAtomicLoad[i] = checkInterfaceFunction(M.getOrInsertFunction(
            AtomicLoadName, Ty, PtrTy, OrdTy, NULL));
        SmallString<32> AtomicStoreName("__tsan_atomic" + itostr(BitSize) + "_store");
        TsanAtomicStore[i] = checkInterfaceFunction(M.getOrInsertFunction(
            AtomicStoreName, IRB.getVoidTy(), PtrTy, Ty, OrdTy, NULL));
    }
    TsanVptrUpdate = checkInterfaceFunction(M.getOrInsertFunction(
        "__tsan_vptr_update", IRB.getVoidTy(), IRB.getInt8PtrTy(),
        IRB.getInt8PtrTy(), NULL));
     
    return true;
}

void APIRace::groupAllUsedMembers( StringRef &TargetClassName, 
    SmallVectorImpl<Instruction*> &LocalGetElementPtrs, SmallSet<Value*, 8> &AllUsedMembers ) {
    
    for ( SmallVectorImpl<Instruction*>::reverse_iterator It = LocalGetElementPtrs.rbegin(),
        End = LocalGetElementPtrs.rend(); It != End; ++It ) {

        GetElementPtrInst *GEP = cast<GetElementPtrInst>(*It);
        Value *base = GEP->getPointerOperand();
        Type *baseTy = base->getType();
        StringRef className = getClassName(baseTy);
        if (className.size() == 0) continue;
        // FIXME: this is not working for friend function...
        if (className != TargetClassName) continue;

        AllUsedMembers.insert(GEP);
        errs() << GEP->getName() << " : " << className << "\n"; 
    }
}

static bool isMemberOfClass( StringRef &TargetClassName, Value *Member ) {
    GetElementPtrInst *GEP = dyn_cast<GetElementPtrInst>(Member);
    if ( GEP ) {
        Value *base = GEP->getPointerOperand();
        Type *baseTy = base->getType();
        StringRef className = getClassName(baseTy);
        // FIXME: this is not working for friend function...
        if (className == TargetClassName) return true;
    }
    return false;
}

void APIRace::groupLoadedAndStoredMembers( StringRef &TargetClassName,
    SmallVectorImpl<Instruction*> &LocalLoadAndStores,
    std::set<Value*> &AllLoadedMembers, std::set<Value*> &AllStoredMembers ) {
    
    for ( SmallVectorImpl<Instruction*>::reverse_iterator It = LocalLoadAndStores.rbegin(),
        End = LocalLoadAndStores.rend(); It != End; ++It ) {
        
        if (StoreInst *SI = dyn_cast<StoreInst>(*It)) {
            Value *PointerOperand = SI->getPointerOperand();
            if ( isMemberOfClass(TargetClassName, PointerOperand) ) 
                AllStoredMembers.insert(PointerOperand);
        } 
    }

    for ( SmallVectorImpl<Instruction*>::reverse_iterator It = LocalLoadAndStores.rbegin(),
        End = LocalLoadAndStores.rend(); It != End; ++It ) {

        if (LoadInst *LI = dyn_cast<LoadInst>(*It)) {
            Value *PointerOperand = LI->getPointerOperand();
            // we will write it, so ignore the read
            if ( AllStoredMembers.count(PointerOperand) ) continue;   
            // TODO: in TSAN, addrPointsToConstantData
            if ( isMemberOfClass(TargetClassName, PointerOperand) )
                AllLoadedMembers.insert(PointerOperand);
        }
    }
    LocalLoadAndStores.clear();
}

bool APIRace::runOnFunction(Function &F) {
    // print out user-defined annotation
    // TODO: we could use annotation string to specify special treatment to certain member, or
    //      different checking granularity
    AnnotationMap::iterator ann = AT.find(F.getName());
    errs() << F.getName() << " : " << ( (ann == AT.end()) ? "No Annotation" : ann->second )<< '\n';

    // check whether F is a non-static member function for a class
    // if not, do nothing and return false
    StringRef className = getClassNameOfMemberFunc(F);
    if (className.size()==0) return false;
    errs() << F.getName() << " is a member function of " << className <<'\n';  

    // Traverse all instructions in this function:
    // (1) collect loads/stores/returns, involving a class member
    // (2) check for calls to pthread_*
    //
    // TODO: actually, we need to do 2 runs on functions
    // 1. first run to check whether its contains a call, which (directly or indirectly via 
    // calls inside) potentially calls sync barrier.
    // 2. for correctness, we can only aggregate loads and stores on members until reaching
    // any sync call.

    SmallVector<Instruction*, 8> LocalLoadsAndStores;
    std::set<Value*> AllLoadedMembers, AllStoredMembers;
    bool Res = false;
    bool HasSyncCall = false;

    for (Function::iterator FI = F.begin(), FE = F.end(); FI != FE; ++FI) {
        BasicBlock &BB = *FI;
        for (BasicBlock::iterator BI = BB.begin(), BE = BB.end(); BI != BE; ++BI) {
            if (isa<LoadInst>(BI) || isa<StoreInst>(BI)) {
                LocalLoadsAndStores.push_back(BI); 
            } else if (isa<CallInst>(BI) || isa<InvokeInst>(BI)) {
                if ( (HasSyncCall = isSyncCall(BI)) ) break;
            }
        }
        if (HasSyncCall) break;
        groupLoadedAndStoredMembers( className, LocalLoadsAndStores,
                                    AllLoadedMembers, AllStoredMembers );
    }

    if (HasSyncCall) {  // slow path, instrument every load/store
       // Res |= instrumentEveryLoadAndStore(F); 
    } 
    else { // instrument this function
        Res |= instrumentLoadedAndStoredMemberInFunction( F, AllLoadedMembers, AllStoredMembers );
    }
    return Res;
}

bool APIRace::instrumentLoadedAndStoredMemberInFunction( Function &F, 
    std::set<Value*> &AllLoadedMembers, std::set<Value*> &AllStoredMembers ) {

    for (std::set<Value*>::reverse_iterator It = AllLoadedMembers.rbegin(), 
        End = AllLoadedMembers.rend(); It != End; ++It ) {

        Value *LoadedMember = *It;
        errs() << ">>> Load " << LoadedMember->getName() << "\n";
    }

    for (std::set<Value*>::reverse_iterator It = AllStoredMembers.rbegin(),
        End = AllStoredMembers.rend(); It != End; ++It ) {

        Value *StoredMember = *It;
        errs() << ">>> Store " << StoredMember->getName() << "\n";
    }

    return true;
}
