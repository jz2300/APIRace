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
#include "llvm/ADT/SmallString.h"               // SmallString
#include "llvm/ADT/StringExtras.h"              // itostr
#include "llvm/Transforms/Utils/ModuleUtils.h"  // appendToGlobalCtors
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

bool APIRace::runOnFunction(Function &F) {
    // print out user-defined annotation
    errs() << "APIRace: ";
    AnnotationMap::iterator ann = AT.find(F.getName());
    errs() << F.getName() << " : " << ( (ann == AT.end()) ? "No Annotation" : ann->second )<< '\n';

    // Traverse all instructions in this function:
    // (1) collect loads/stores/returns, involving a class member
    // (2) check for calls to pthread_*

    SmallVector<Instruction*, 8> LocalLoadsAndStores;
    SmallVector<Value*, 8> AllMembers;
    bool Res = false;
    bool HasSync = false;

    for (Function::iterator FI = F.begin(), FE = F.end(); FI != FE; ++FI) {
        BasicBlock &BB = *FI;
        for (BasicBlock::iterator BI = BB.begin(), BE = BB.end(); BI != BE; ++BI) {
            if (isa<LoadInst>(BI) || isa<StoreInst>(BI)) {
                LocalLoadsAndStores.push_back(BI); 
            } else if (isa<CallInst>(BI) || isa<InvokeInst>(BI)) {
                if (HasSync = checkSyncCall(BI)) break;
            }
        }
        analyzeLoadAndStoreOperands( LocalLoadsAndStores, AllMembers );
    }

    if (HasSync) {  // slow path, instrument every load/store
        Res |= instrumentEveryLoadAndStore(
        
    } 
    else { // instrument this function
        Res |= instrumentMemberLoadAndStore(F, AllMembers);
    }
    return Res;
}

