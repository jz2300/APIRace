#define DEBUG_TYPE "apirace"
#include "llvm/Pass.h"
#include "llvm/Function.h"
#include "llvm/IRBuilder.h"
#include "llvm/Module.h"
#include "llvm/Type.h"
#include "llvm/Intrinsics.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/DataLayout.h"                    // for TD, get size. 
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/SmallString.h"               // class SmallString
#include "llvm/ADT/SmallSet.h"                  // class SmallSet
#include "llvm/ADT/StringExtras.h"              // itostr
#include "llvm/Transforms/Utils/ModuleUtils.h"  // appendToGlobalCtors
#include <map>
using namespace llvm;

static cl::opt<bool>  ClInstrumentFuncEntryExit(
    "apirace-instrument-func-entry-exit", cl::init(true),
    cl::desc("Instrument function entry and exit"), cl::Hidden);
static cl::opt<bool>  CLInstrumentAtMultipleReturn(
    "apirace-insrtument-at-multiple-return", cl::init(false),
    cl::desc("Instrument at multiple return, otherwise at the begin"), cl::Hidden);


STATISTIC(NumInstrumentedReads, "Number of instrumented reads");
STATISTIC(NumInstrumentedWrites, "Number of instrumented writes");
STATISTIC(NumOmittedReadsBeforeWrite,
          "Number of reads ignored due to following writes");
STATISTIC(NumAccessesWithBadSize, "Number of accesses with bad size");


namespace {

struct APIRace : public FunctionPass {
    static char ID;
    APIRace();
    const char *getPassName() const;
    virtual bool runOnFunction(Function &F);
    virtual bool doInitialization(Module &M);

  private:
    DataLayout *TD;

    // user-defined annotations
    typedef std::map<std::string, std::string> AnnotationMap;
    AnnotationMap AT;  // [ FunName : Annotation ]
    bool getFuncAnnotations(Module &M);

    // compute lvl of sync calls
    typedef std::map<std::string, int> FunctionSyncMap;
    FunctionSyncMap FSM;
    int isSyncCall(Instruction *I);
    int hasSyncCallInFunc(Function *F);

    // utility for instrumentation
    void groupAllMemberGEPs( StringRef &TargetClassName,
        SmallVectorImpl<Instruction*> &LocalGetElementPtrs, std::set<Instruction*> &AllMemberGEPs );
    void groupLoadedAndStoredMembers( StringRef &TargetClassName, 
        SmallVectorImpl<Instruction*> &LocalLoadAndStores,
        std::set<Value*> &AllLoadedMembers, std::set<Value*> &AllStoredMembers );
    bool instrumentLoadedAndStoredMemberInFunction( Instruction *I, 
        std::set<Value*> &AllLoadedMembers, std::set<Value*> &AllStoredMembers );
    int getMemoryAccessFuncIndex(Value *Addr);
    Instruction *findInstructionAfterThis1(Function &F, StringRef &TargetClassName);
    void selectLoadAndStoreInstructions( StringRef &TargetClassName,
        SmallVectorImpl<Instruction*> &LocalLoadAndStores,
        SmallVectorImpl<Instruction*> &MemberLoadAndStores );
    bool instrumentExactLoadOrStore(Instruction *I);

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
 * OUTPUT: if it is a class pointer, return its class name, like "class.A" ; 
 *         otherwise, return "".
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

/**
 * Extract the class name for the given function 'F'.
 * Return: if it is a member function, return its class name, like "class.A" ;
 *         otherwise, return "".
 */
static StringRef getClassNameOfMemberFunc(Function &F) {
    Function::arg_iterator argIt = F.arg_begin();
    Value * firstArg = argIt;                                       // get 1st argument
    Type * typeOfFirstArg = firstArg->getType();                    //     its type
    return getClassName(typeOfFirstArg);
}

/**
 * Check if the given 'Member' is a non-static member of a class named 'TargetClassName'.
 */
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

/**
 * Check whether the given CallInst calls to synchronization function, like
 * pthread_join, pthread_barrier_wait ...
 */
int APIRace::isSyncCall(Instruction *I) {
    static const char * syncCallList[] = {
        "pthread_cancel",
        "pthread_join",
        "pthread_barrier_wait",
        "pthread_cond_wait",
        "pthread_cond_signal",
        "pthread_mutex_lock",
        "pthread_mutex_unlock",
        // TODO: add other synchronization functions in Pthreads API
    };
    static int numOfSyncCalls = sizeof(syncCallList)/sizeof(syncCallList[0]);

    CallInst *CI = dyn_cast<CallInst>(I);
    if (!CI) return false;
    Function *F = CI->getCalledFunction();
    if (!F) return false;
    if (!F->hasName()) return false;
    StringRef funcName = F->getName();
    
    for (int i=0; i<numOfSyncCalls; ++i) {
        if (funcName == syncCallList[i]) {
            errs() << "isSyncCall: found a call to \'" << funcName 
                   << "\' in instruction \"" << *I << "\"\n";
            return 0;
        }
    }

    // lazy evaluation + cache
    return hasSyncCallInFunc(F);
}

int APIRace::hasSyncCallInFunc(Function *F) {
    if (!F) return false;
    
    // check whether we cache the result
    FunctionSyncMap::iterator it = FSM.find(F->getName());
    if (it!=FSM.end()) return it->second;

    // default not a sync -1
    int lvl = -1, new_lvl;
    // DFS
    for (Function::iterator FI = F->begin(), FE = F->end(); FI != FE; ++FI) {
        BasicBlock &BB = *FI;
        for (BasicBlock::iterator BI = BB.begin(), BE = BB.end(); BI != BE; ++BI) {
            if (isa<CallInst>(BI)) {
                new_lvl = isSyncCall(BI) + 1;
                if (new_lvl > 0) {
                    if (lvl < 0 || lvl > new_lvl) lvl = new_lvl;
                }
            } 
        }
    }
    
    return FSM[F->getName()] = lvl;
}

bool APIRace::doInitialization(Module &M) {
    TD = getAnalysisIfAvailable<DataLayout>();
    if (!TD) return false;

    // get user-defined annotation, if any
    getFuncAnnotations(M);

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

void APIRace::groupAllMemberGEPs( StringRef &TargetClassName, 
    SmallVectorImpl<Instruction*> &LocalGetElementPtrs, std::set<Instruction*> &AllMemberGEPs ) {
    
    for ( SmallVectorImpl<Instruction*>::reverse_iterator It = LocalGetElementPtrs.rbegin(),
        End = LocalGetElementPtrs.rend(); It != End; ++It ) {

        if ( isMemberOfClass(TargetClassName, *It) ) {
            AllMemberGEPs.insert(*It);
        }
    }
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
            if ( AllStoredMembers.count(PointerOperand) ) {
                // we will write it, so ignore the read
                NumOmittedReadsBeforeWrite++;
                continue;   
            }
            // TODO: in TSAN, addrPointsToConstantData
            if ( isMemberOfClass(TargetClassName, PointerOperand) )
                AllLoadedMembers.insert(PointerOperand);
        }
    }
    LocalLoadAndStores.clear();
}

void APIRace::selectLoadAndStoreInstructions( StringRef &TargetClassName,
    SmallVectorImpl<Instruction*> &LocalLoadAndStores,
    SmallVectorImpl<Instruction*> &MemberLoadAndStores ) {

    SmallSet<Value*, 8> WriteTargets;    
    for ( SmallVectorImpl<Instruction*>::reverse_iterator It = LocalLoadAndStores.rbegin(),
        End = LocalLoadAndStores.rend(); It != End; ++It ) {
        
        Instruction *I = *It;
        if (StoreInst *SI = dyn_cast<StoreInst>(I)) {
            Value *PointerOperand = SI->getPointerOperand();
            if ( !isMemberOfClass( TargetClassName, PointerOperand ) ) continue;
            WriteTargets.insert(PointerOperand);
        } else {
            LoadInst *LI = cast<LoadInst>(I);
            Value *PointerOperand = LI->getPointerOperand();
            if ( !isMemberOfClass( TargetClassName, PointerOperand ) ) continue;
            if (WriteTargets.count(PointerOperand)) {
                NumOmittedReadsBeforeWrite++;
                continue;
            }
        }
        MemberLoadAndStores.push_back(I);
    }
    LocalLoadAndStores.clear();
}

bool APIRace::runOnFunction(Function &F) {
    if (!TD) return false;

    // print out user-defined annotation
    // TODO: we could use annotation string to specify special treatment to certain member, or
    //      different checking granularity
    AnnotationMap::iterator ann = AT.find(F.getName());

    // check whether F is a non-static member function for a class
    // if not, do nothing and return false
    StringRef className = getClassNameOfMemberFunc(F); 
    if (className.size()==0) {
        className = ".....";
        // FIXME: At this moment, we still need to instrument entry/exit of EVERY function 
        // thus we can NOT return here.
        //return false;
    }


    // Traverse all instructions in this function:
    // (1) collect loads/stores/returns, involving a class member
    // (2) check for sync calls to pthread_*
    // 
    // NOTE: isSyncCall is recursive lazy evaluation on whether the call may contain sync 

    SmallVector<Instruction*, 8> RetVec;
    SmallVector<Instruction*, 8> LocalLoadsAndStores;
    SmallVector<Instruction*, 8> MemberLoadsAndStores;
    std::set<Value*> AllLoadedMembers, AllStoredMembers;
    std::set<Instruction*> AllMemberGEPs;
    bool Res = false;
    int SyncCallLvl = hasSyncCallInFunc(&F);

    errs() << ">>>>>> Start Function " << F.getName() << " <<<<<<\n";
    errs() << "# Annotation : " << ( (ann == AT.end()) ? "NONE" : ann->second )<< '\n';
    errs() << "# Class Name : " << className <<'\n';  
    errs() << "# Sync Level : " << SyncCallLvl << '\n';

    for (Function::iterator FI = F.begin(), FE = F.end(); FI != FE; ++FI) {
        BasicBlock &BB = *FI;
        int BBSyncCallLvl = -1;

        for (BasicBlock::iterator BI = BB.begin(), BE = BB.end(); BI != BE; ++BI) {
            //errs() << *BI << '\n';
            if (isa<LoadInst>(BI) || isa<StoreInst>(BI)) {
                LocalLoadsAndStores.push_back(BI); 
            } else if (isa<GetElementPtrInst>(BI)) {
                if ( isMemberOfClass( className, BI ) ) {
                    // collect all member GEPs
                    AllMemberGEPs.insert(BI);
                }
            } else if (isa<ReturnInst>(BI)) {
                RetVec.push_back(BI);
                if (CLInstrumentAtMultipleReturn) {
                    //FIXME : the following is NOT working, because clang treat return 
                    // as (1) set retval and (2) jump to RETURN.
                    // i.e. only one ret exists in a function!!!
                    if (BBSyncCallLvl < 0) {  // if no sync in this BB
                        groupLoadedAndStoredMembers( className, LocalLoadsAndStores,
                                    AllLoadedMembers, AllStoredMembers );
                    }
                    Res |= instrumentLoadedAndStoredMemberInFunction( BI,
                                    AllLoadedMembers, AllStoredMembers );
                }
            } else if (isa<CallInst>(BI) || isa<InvokeInst>(BI)) {
                if ( BBSyncCallLvl < 0 )
                    BBSyncCallLvl = isSyncCall(BI);
            }
        }
        // if this BB has sync call, we treat all loads and stores exactly.
        if (BBSyncCallLvl >= 0) {
            selectLoadAndStoreInstructions( className, LocalLoadsAndStores,
                                     MemberLoadsAndStores);
        } else {
            groupLoadedAndStoredMembers( className, LocalLoadsAndStores,
                                   AllLoadedMembers, AllStoredMembers );
        }
    }
    
    // Instrument those needed to be exact location
    for (size_t i=0, n=MemberLoadsAndStores.size(); i<n; ++i) {
        Res |= instrumentExactLoadOrStore(MemberLoadsAndStores[i]);
    }

    // Move all GEPs for used member just after 'this1'
    if (AllMemberGEPs.size() > 0) {
        Instruction *MovePos = findInstructionAfterThis1(F, className);
        if (MovePos == NULL) {
            errs() << "Cannot location \'this\' for GEP! BUG!!!!!!!!!!!!!!!!! \n";
        }
        for (std::set<Instruction*>::reverse_iterator It = AllMemberGEPs.rbegin(),
            End = AllMemberGEPs.rend(); It != End; ++It) {
            (*It)->moveBefore(MovePos);
        }
        if (!CLInstrumentAtMultipleReturn) {
            Res |= instrumentLoadedAndStoredMemberInFunction( MovePos,
                                AllLoadedMembers, AllStoredMembers );
        }
        //errs() << '\n' << F << '\n';
    }

    // Instrument function entry/exit
    if ((Res || SyncCallLvl ) && ClInstrumentFuncEntryExit) {
        IRBuilder<> IRB(F.getEntryBlock().getFirstNonPHI());
        Value *ReturnAddress = IRB.CreateCall(
            Intrinsic::getDeclaration(F.getParent(), Intrinsic::returnaddress),
            IRB.getInt32(0));
        IRB.CreateCall(TsanFuncEntry, ReturnAddress);
        for (size_t i = 0, n = RetVec.size(); i < n; ++i) {
            IRBuilder<> IRBRet(RetVec[i]);
            IRBRet.CreateCall(TsanFuncExit);
        }
        Res = true;
    }

    errs() << "-------------------------------\n";
    return Res;
}

Instruction *APIRace::findInstructionAfterThis1(Function &F, StringRef &TargetClassName) {
    BasicBlock &EntryBlock = F.getEntryBlock();
    BasicBlock::iterator BI, BE;
    for (BI = EntryBlock.begin(), BE = EntryBlock.end();
        BI != BE; ++BI ) {
        if (isa<LoadInst>(BI)) {
            if (BI->getName() == "this1") break;
        }
    }
    // If not found "this1", something wrong!!!
    if (BI == BE) return NULL;
    // skip member GEPs
    while ( ++BI != BE && isMemberOfClass(TargetClassName, BI) );
    if (BI!=BE) return BI;
    // reaching end of entry BB, get first Instruction of 2nd BB
    Function::iterator FI = F.begin();
    FI++;
    return FI->getFirstNonPHI();
}

bool APIRace::instrumentLoadedAndStoredMemberInFunction( Instruction *I, 
    std::set<Value*> &AllLoadedMembers, std::set<Value*> &AllStoredMembers ) {

    IRBuilder<> IRB(I);

    bool Res = false;
    for (std::set<Value*>::reverse_iterator It = AllLoadedMembers.rbegin(), 
        End = AllLoadedMembers.rend(); It != End; ++It ) {

        Value *LoadedMember = *It;

        int Idx = getMemoryAccessFuncIndex(LoadedMember);
        if (Idx < 0) continue;
        errs() << "# INSTR Load : " << LoadedMember->getName() << "\n";
        IRB.CreateCall(TsanRead[Idx], IRB.CreatePointerCast(LoadedMember, 
                                                    IRB.getInt8PtrTy()));
        NumInstrumentedReads++;
        Res |= true;
    }

    for (std::set<Value*>::reverse_iterator It = AllStoredMembers.rbegin(),
        End = AllStoredMembers.rend(); It != End; ++It ) {

        Value *StoredMember = *It;
        int Idx = getMemoryAccessFuncIndex(StoredMember);
        if (Idx < 0) continue;
        errs() << "# INSTR Store : " << StoredMember->getName() << "\n";
        IRB.CreateCall(TsanWrite[Idx], IRB.CreatePointerCast(StoredMember, 
                                                    IRB.getInt8PtrTy()));
        NumInstrumentedWrites++;
        Res |= true;
    }

    return Res;
}

bool APIRace::instrumentExactLoadOrStore(Instruction *I) {
    IRBuilder<> IRB(I);
    bool IsWrite = isa<StoreInst>(*I);
    Value *Addr = IsWrite
        ? cast<StoreInst>(I)->getPointerOperand()
        : cast<LoadInst>(I)->getPointerOperand();
    int Idx = getMemoryAccessFuncIndex(Addr);
    if (Idx < 0) return false;
    Value *OnAccessFunc = IsWrite ? TsanWrite[Idx] : TsanRead[Idx];
    IRB.CreateCall(OnAccessFunc, IRB.CreatePointerCast(Addr, IRB.getInt8PtrTy()));
    if (IsWrite) NumInstrumentedWrites++;
    else         NumInstrumentedReads++;
    return true;
}

int APIRace::getMemoryAccessFuncIndex(Value *Addr) {
    Type *OrigPtrTy = Addr->getType();
    Type *OrigTy = cast<PointerType>(OrigPtrTy)->getElementType();
    assert(OrigTy->isSized());
    uint32_t TypeSize = TD->getTypeStoreSizeInBits(OrigTy);
    if (TypeSize != 8  && TypeSize != 16 &&
        TypeSize != 32 && TypeSize != 64 && TypeSize != 128) {
        NumAccessesWithBadSize++;
        return -1;
    }

    size_t Idx = CountTrailingZeros_32(TypeSize / 8);
    assert(Idx < kNumberOfAccessSizes);
    return Idx;
}
