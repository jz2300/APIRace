/**
 * this is the code of APIRace project, for Reliable Software 2012. 
 * author: Jing Zhang, jz2300@columbia.edu
 */

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

static cl::opt<bool>  ClDisableAll(
    "apirace-disable-all-instruments", cl::init(false),
    cl::desc("Do NO instrument"), cl::Hidden);
static cl::opt<bool>  ClInstrumentFuncEntryExit(
    "apirace-instrument-func-entry-exit", cl::init(true),
    cl::desc("Instrument function entry and exit"), cl::Hidden);
static cl::opt<bool>  ClInstrumentMemberLoadAndStore(
    "apirace-instrument-member-load-and-store", cl::init(true),
    cl::desc("Instrument loads and stores of class members"), cl::Hidden);
static cl::opt<bool> ClInsertTempStoreExact(
    "apirace-insert-temp-store-exactly", cl::init(false),
    cl::desc("Insert storeInst of Temp exactly before load/store"), cl::Hidden);
static cl::opt<bool> ClDebugVerbose(
    "apirace-debug-verbose", cl::init(true),
    cl::desc("show verbose debug info"), cl::Hidden);
static cl::opt<bool> ClDebugTrack(
    "apirace-debug-trace", cl::init(false),
    cl::desc("show pass tracking"), cl::Hidden);
static cl::opt<bool> ClShowStatistic(
    "show statistics", cl::init(true),
    cl::desc("show statistics on intrumentation"), cl::Hidden); 


STATISTIC(NumInstrumentedReads, "Number of instrumented reads");
STATISTIC(NumInstrumentedWrites, "Number of instrumented writes");
STATISTIC(NumOmittedReadsBeforeWrite,
          "Number of reads ignored due to following writes");
STATISTIC(NumAccessesWithBadSize, "Number of accesses with bad size");
STATISTIC(NumOmittedTemps, "Number of temp reduced due to duplicated members");
STATISTIC(NumSyncCalls, "Number of synchronization calls");
STATISTIC(NumSyncCallsTreated, "Number of synchronization calls treated");
STATISTIC(NumInstrumentedReadsExact, "Number of instrumented reads at exact postion");
STATISTIC(NumInstrumentedWritesExact, "Number of instrumented writes at exact position");

// auxiliary functions
static int checkSizeValidity(unsigned size);
static StringRef getClassName(Type *PT);
static Type* getClassTypeOfMemberFunc(Function &F);
static bool isMemberOfClass( StringRef &TargetClassName, Value *Member );
static bool isMemberOfAnyClass( Value *Member );
static Function *checkInterfaceFunction(Constant *FuncOrBitcast);
static int selectGEP( GetElementPtrInst *GEP );

// declaration of APIRace Pass
namespace {

struct APIRace : public FunctionPass {
    static char ID;
    APIRace();
    const char *getPassName() const;
    virtual bool runOnFunction(Function &F);
    virtual bool doInitialization(Module &M);
    virtual bool doFinalization(Module &M);

  private:
    DataLayout *TD;

    // user-defined annotations
    typedef std::map<std::string, std::string> AnnotationMap;
    AnnotationMap AT;  // [ FunName : Annotation ]
    bool getFuncAnnotations(Module &M);

    // compute lvl of sync calls
    typedef std::map<std::string, int> FunctionSyncMap;
    FunctionSyncMap FSM;
    int isSyncCall(Instruction *I, int level_left = 5);
    int hasSyncCallInFunc(Function *F, int level_left = 5);

    // collect member GEP
    typedef std::map<GetElementPtrInst*, std::pair<Value*, Value*> > MemberGEPToTempMap;
    void groupAllClassMemberGEPsAndInsertTemps( Instruction *InsertPoint,
        SmallVectorImpl<Instruction*> &LocalGetElementPtrs,
        MemberGEPToTempMap &UniqueMemberGEPs, MemberGEPToTempMap &DupMemberGEPs );
    std::pair<Value*,Value*> findTempforGEP( GetElementPtrInst *GEP, 
        MemberGEPToTempMap &UniqueGEPs, MemberGEPToTempMap &DupGEPs );
    MemberGEPToTempMap::iterator findDuplicateGEP( GetElementPtrInst* GEP,
        MemberGEPToTempMap &UniqueMemberGEPs );
    void nullAllTemps( Instruction *IAfter, MemberGEPToTempMap &UniqueMemberGEPs );
    void insertStoreInstOfTemp( Instruction *GEP, Value* Temp);

    // class info:
    // A map ( class name :-> offset of each member from base ) 
    // used as 2nd index argument in GEP
    typedef std::map<std::string, std::vector<unsigned> > ClassInfoMap;
    ClassInfoMap CIM;
    void extraceClassInfo( Type* classType );

    // utility for instrumentation
    void groupLoadedAndStoredMembers( SmallVectorImpl<Instruction*> &LocalLoadAndStores,
        MemberGEPToTempMap &UniqueGEPs, MemberGEPToTempMap &DupGEPs,
        std::set<Value*> &AllMemberLoads,
        std::set<Value*> &AllMemberStores );
    bool instrumentLoadedAndStoredMemberInFunction( Instruction *Insert,
        std::set<Value*> &AllMemberLoads,
        std::set<Value*> &AllMemberStores,
        MemberGEPToTempMap &UniqueMemberGEPs, MemberGEPToTempMap &DupMemberGEPs );

    int getMemoryAccessFuncIndex(Value *Addr);
    int getMemoryAccessFuncIndexFromType(Type *OrigPtrTy);
    //void insertStoreInstOfTempBefore( Instruction *I, Value* Temp);
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
int APIRace::isSyncCall(Instruction *I, int level_left) {
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
    if (!CI) return -1;
    Function *F = CI->getCalledFunction();
    if (!F) return -1;
    if (!F->hasName()) return -1;
    StringRef funcName = F->getName();
    
    for (int i=0; i<numOfSyncCalls; ++i) {
        if (funcName == syncCallList[i]) {
            if (ClDebugVerbose) {
                errs() << "isSyncCall: found a call to \'" << funcName 
                   << "\' in instruction \"" << *I << "\"\n";
            }
            return 0;
        }
    }
    if (level_left == 0) return -1;

    // lazy evaluation + cache
    return hasSyncCallInFunc(F, level_left);
}


// NOTE: if the calling graph has loop, the DFS could goes to infinite loop
// without loop detection. Now, simply set max depth = 5. 
int APIRace::hasSyncCallInFunc(Function *F, int level_left) {
    if (ClDebugTrack) errs() << "APIRace::hasSyncCallInFunc: " << F->getName()  << ' ' << level_left << '\n';
    if (!F) return -1;
    if (!F->hasName()) return -1;
    
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
                new_lvl = isSyncCall(BI, level_left-1) + 1;
                if (new_lvl > 0) {
                    if (lvl < 0 || lvl > new_lvl) lvl = new_lvl;
                }
            } 
        }
    }
    
    return FSM[F->getName()] = lvl;
}

bool APIRace::doInitialization(Module &M) {
    if (ClDebugTrack) errs() << "APIRace::doInitialization\n";
    // enable statistics
    EnableStatistics();
    bool Res = false;
    TD = getAnalysisIfAvailable<DataLayout>();
    if (!TD) return Res;

    // get user-defined annotation, if any
    getFuncAnnotations(M);

    if (!ClDisableAll) {
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
     
        Res = true;
    }

    return Res;
}

bool APIRace::runOnFunction(Function &F) {
    if (ClDebugTrack) errs() << "APIRace::runOnFunction\n";
    if (ClDisableAll) return false;
    if (!TD) return false;

    // print out user-defined annotation
    // TODO: we could use annotation string to specify special treatment to certain member, or
    //      different checking granularity
    AnnotationMap::iterator ann = AT.find(F.getName());

    // check whether F is a non-static member function for a class
    // if not, do nothing and return false
    Type *classType = getClassTypeOfMemberFunc(F);
    StringRef className = getClassName(classType);

    if (className.size()==0) {
        // className = ".....";
        // FIXME: At this moment, we still need to instrument entry/exit of EVERY function 
        // thus we can NOT return here.
        // return false;
    }

    if (ClDebugVerbose) {
        errs() << ">>>>>> Start Function " << F.getName() << " <<<<<<\n";
        errs() << "# Annotation : " << ( (ann == AT.end()) ? "NONE" : ann->second )<< '\n';
        errs() << "# Class Name : " << className <<'\n';  
    }

    // Traverse all instructions in this function:
    // (1) collect loads/stores/returns, involving a class member
    // (2) check for sync calls to pthread_*
    // 
    // NOTE: isSyncCall is recursive lazy evaluation on whether the call may contain sync 

    SmallVector<Instruction*, 8> RetVec;
    SmallVector<Instruction*, 8> LocalLoadsAndStores;
    SmallVector<Instruction*, 8> LocalGEPs;
    std::set<Value*> AllMemberLoads, AllMemberStores;
    MemberGEPToTempMap UniqueMemberGEPs, DupMemberGEPs;
    bool Res = false;
    int SyncCallLvl = hasSyncCallInFunc(&F);
    Instruction *Ihead = F.getEntryBlock().getFirstNonPHI();

    if (ClDebugVerbose) {
        errs() << "# Sync Level : " << SyncCallLvl << '\n';
    }

    for (Function::iterator FI = F.begin(), FE = F.end(); FI != FE; ++FI) {
        BasicBlock &BB = *FI;

        for (BasicBlock::iterator BI = BB.begin(), BE = BB.end(); BI != BE; ++BI) {
            if (isa<LoadInst>(BI) || isa<StoreInst>(BI)) {
                LocalLoadsAndStores.push_back(BI); 
            } else if (isa<GetElementPtrInst>(BI)) {
                LocalGEPs.push_back(BI);
            } else if (isa<ReturnInst>(BI)) {
                RetVec.push_back(BI);
            } else if (isa<CallInst>(BI) || isa<InvokeInst>(BI)) {
                int synclvl = isSyncCall(BI);
                if (synclvl >= 0) NumSyncCalls++;
                if (synclvl >= 0) {     // use your heuristics
                    NumSyncCallsTreated++;
                    // if BI is a potential syncall, not necessarily be,
                    // (1) instrument all load and store before call
                    groupAllClassMemberGEPsAndInsertTemps( Ihead, LocalGEPs, 
                        UniqueMemberGEPs, DupMemberGEPs );
                    groupLoadedAndStoredMembers( LocalLoadsAndStores, UniqueMemberGEPs,
                        DupMemberGEPs, AllMemberLoads, AllMemberStores );
                    instrumentLoadedAndStoredMemberInFunction( BI,
                        AllMemberLoads, AllMemberStores,
                        UniqueMemberGEPs, DupMemberGEPs);
                    
                    // (2) clear local temp after call
                    nullAllTemps( BI, UniqueMemberGEPs );
                }
            }
        }
        groupAllClassMemberGEPsAndInsertTemps( Ihead, LocalGEPs, UniqueMemberGEPs,
            DupMemberGEPs );
        groupLoadedAndStoredMembers( LocalLoadsAndStores, UniqueMemberGEPs,
            DupMemberGEPs, AllMemberLoads, AllMemberStores );
    }
    
    // Instrument member loads and stores before returns
    for (size_t i = 0, n = RetVec.size(); i < n; ++i) {
        Res |= instrumentLoadedAndStoredMemberInFunction( RetVec[i],
               AllMemberLoads, AllMemberStores, UniqueMemberGEPs, DupMemberGEPs);
    }

    // Instrument function entry/exit
    if ( ClInstrumentFuncEntryExit && !ClDisableAll ) {
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

    if (ClDebugVerbose) {

        errs() << "\n### After instrumentation: " << F << '\n';
        errs() << "-------------------------------\n";
    }
    return Res;
}

bool APIRace::doFinalization(Module& M) {
    //PrintStatistics(errs());
    return false;
}

/**
 * find whether the `GEP' is in `UniqueMemberGEPs'. if yes, return associated iterator,
 * else, return end()
 */
APIRace::MemberGEPToTempMap::iterator APIRace::findDuplicateGEP( GetElementPtrInst* GEP,
    MemberGEPToTempMap &UniqueMemberGEPs ) {

    Value *GEPBase = GEP->getPointerOperand();
    unsigned NumOps = GEP->getNumOperands();

    MemberGEPToTempMap::iterator it, end;
    for ( it = UniqueMemberGEPs.begin(), end = UniqueMemberGEPs.end(); 
        it != end; ++it ) {
        
        GetElementPtrInst* Mem = it->first;
        Value *MemBase = Mem->getPointerOperand();
        unsigned NumOps2 = Mem->getNumOperands();
        if (GEPBase != MemBase || NumOps != NumOps2) continue;

        int flag = 0;
        for (unsigned i=1; i< NumOps; ++i) {
            ConstantInt *GEPARG = dyn_cast<ConstantInt>(GEP->getOperand(i));
            ConstantInt *MemARG = dyn_cast<ConstantInt>(Mem->getOperand(i));
            if (!GEPARG || !MemARG) {
                flag = 1; break;
            }
            if (!APInt::isSameValue( GEPARG->getValue(), MemARG->getValue() )) {
                flag = 1; break;
            }
        }
        if (!flag) {
            errs() << "DUP GEPS: " << *GEP << " || " << *Mem << '\n';
            return it;
        }       
    }

    return it;
}

//NOTE:: this is a walkaround for false positive of "heap-use-after-free"
// example:
//
//   template<T> class A {
//        Pointer<T> m_data;
//        ....
//        void bugfunc() {
//
//           delete[] m_data;      // trigger heap-use-after-free
//
//           // __tsan_read(m_data)
//        }
//   
// because 
// (1) there exists GEP to get just m_data, instead of &m_data
// (2) we treat all GEPs with name "class.xxxx" as a class member
// (3) thus, we instrument a read or write after the ptr already deleted!!!
//
// !!!!only consider GEP hasName() is class member!!!!

static int selectGEP( GetElementPtrInst *GEP ){
    //unsigned NumOps = GEP->getNumOperands();
    //if (NumOps != 3) return -1;
    if (!GEP->hasName()) return -1;
    return 0;
}

/**
 * group unique and duplicate GEPs for the given `LocalGetElementPtrs',
 * and insert alloca for temp ptrs.
 */
void APIRace::groupAllClassMemberGEPsAndInsertTemps( Instruction *InsertPoint,
    SmallVectorImpl<Instruction*> &LocalGetElementPtrs,
    MemberGEPToTempMap &UniqueMemberGEPs, MemberGEPToTempMap &DupMemberGEPs ) {
    
    for ( SmallVectorImpl<Instruction*>::reverse_iterator It = LocalGetElementPtrs.rbegin(),
        End = LocalGetElementPtrs.rend(); It != End; ++It ) {
        
        Instruction *I = *It;
        if (!isMemberOfAnyClass(I)) continue;
        if (!isa<GetElementPtrInst>(I)) continue;
        GetElementPtrInst *GEP = cast<GetElementPtrInst>(I);
        if (selectGEP(GEP)) continue;
        MemberGEPToTempMap::iterator found = findDuplicateGEP( GEP, UniqueMemberGEPs );
        if (found == UniqueMemberGEPs.end()) {
            std::pair<Value*, Value*> Temp; 
            IRBuilder<> IRB(InsertPoint);
            Temp.first = IRB.CreateAlloca(GEP->getType());
            IRB.CreateStore(Constant::getNullValue(GEP->getType()), Temp.first);
            Temp.second = IRB.CreateAlloca(GEP->getType());
            IRB.CreateStore(Constant::getNullValue(GEP->getType()), Temp.second);
            
            UniqueMemberGEPs[GEP] = Temp;
        }
        else {
            DupMemberGEPs[GEP] = found->second;
            NumOmittedTemps++;
        }
    }
    LocalGetElementPtrs.clear();
}

/**
 * insert StoreInst to null all temp ptrs.
 */
void APIRace::nullAllTemps( Instruction *IAfter, MemberGEPToTempMap &UniqueMemberGEPs ) {
    
    for ( MemberGEPToTempMap::iterator It = UniqueMemberGEPs.begin(),
        End = UniqueMemberGEPs.end(); It != End; ++It ) {
        
        Value *Temp1 = (*It).second.first,
              *Temp2 = (*It).second.second;

        Instruction *DUM = new AllocaInst(Type::getInt8PtrTy(IAfter->getContext()));
        DUM->insertAfter(IAfter);
        IRBuilder<> IRB(DUM);
        Type *OrigPtrTy = Temp1->getType();
        Type *OrigTy = cast<PointerType>(OrigPtrTy)->getElementType();
        IRB.CreateStore(Constant::getNullValue(OrigTy), Temp1);
        IRB.CreateStore(Constant::getNullValue(OrigTy), Temp2);        
        DUM->eraseFromParent();
    }

}

/**
 * for given GEP, try to find <GEP,TEMP> pair from maps.
 */
std::pair<Value*, Value*> APIRace::findTempforGEP( GetElementPtrInst *GEP, MemberGEPToTempMap &UniqueGEPs, MemberGEPToTempMap &DupGEPs ) {
    std::pair<Value*, Value*> none_pair = std::pair<Value*, Value*>( NULL, NULL );
    MemberGEPToTempMap::iterator found;
    found = UniqueGEPs.find(GEP);
    if ( found != UniqueGEPs.end() ) {
        return found->second;
    }
    found = DupGEPs.find(GEP);
    if ( found != DupGEPs.end() ) {
        return found->second;
    }
    return none_pair;
}

/**
 * find member loads and member stores from `LocalLoadAndStors'.
 */
void APIRace::groupLoadedAndStoredMembers( SmallVectorImpl<Instruction*> &LocalLoadAndStores,
    MemberGEPToTempMap &UniqueGEPs, MemberGEPToTempMap &DupGEPs,
    std::set<Value*> &AllMemberLoads, 
    std::set<Value*> &AllMemberStores ) {

    SmallSet<Value*, 8> WriteTargets;
    for ( SmallVectorImpl<Instruction*>::reverse_iterator It = LocalLoadAndStores.rbegin(),
        End = LocalLoadAndStores.rend(); It != End; ++It ) {

        if (StoreInst *SI = dyn_cast<StoreInst>(*It)) {
            Value *PointerOperand = SI->getPointerOperand();
            if (!isa<GetElementPtrInst>(PointerOperand)) continue;
            GetElementPtrInst *GEP = cast<GetElementPtrInst>(PointerOperand);
            Value *Temp = findTempforGEP( GEP, UniqueGEPs, DupGEPs ).first;
            if ( Temp != NULL ) {
                if (ClInsertTempStoreExact) {
                    IRBuilder<> IRB(SI);
                    IRB.CreateStore( GEP, Temp );
                } else {
                    if(!AllMemberStores.count(Temp)) {
                        insertStoreInstOfTemp( GEP, Temp );
                    }
                }
                AllMemberStores.insert( Temp );
                WriteTargets.insert(GEP);                
            }
        }
    }

    for ( SmallVectorImpl<Instruction*>::reverse_iterator It = LocalLoadAndStores.rbegin(),
        End = LocalLoadAndStores.rend(); It != End; ++It ) {

        if (LoadInst *LI = dyn_cast<LoadInst>(*It)) {
            Value *PointerOperand = LI->getPointerOperand();
            if (!isa<GetElementPtrInst>(PointerOperand)) continue;
            GetElementPtrInst *GEP = cast<GetElementPtrInst>(PointerOperand);
            if ( WriteTargets.count(GEP) ) {
                // we will write it, so ignore the read
                NumOmittedReadsBeforeWrite++;
                continue;
            }
            Value *Temp = findTempforGEP( GEP, UniqueGEPs, DupGEPs ).second;
            if ( Temp != NULL ) {
                if(ClInsertTempStoreExact) {
                    IRBuilder<> IRB(LI);
                    IRB.CreateStore( GEP, Temp );
                } else {
                    if (!AllMemberLoads.count(Temp)) {
                        insertStoreInstOfTemp( GEP, Temp );
                    }
                }
                AllMemberLoads.insert(Temp);
            }
        }
    }
    LocalLoadAndStores.clear();
}

/**
 * deprecated method.
 */
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

/**
 * deprecated method
 */
Instruction *APIRace::findInstructionAfterThis1(Function &F, StringRef &TargetClassName) {
    BasicBlock &EntryBlock = F.getEntryBlock();
    BasicBlock::iterator BI, BE;
    for (BI = EntryBlock.begin(), BE = EntryBlock.end();
        BI != BE; ++BI ) {
        if (isa<LoadInst>(BI)) {
            if (BI->getName() == "this1") break;
        }
    }
    // If not found "this1", 
    // this happens when compiling code with -O1 or above. 
    // 
    // Assume that the code does not need this1, that is, the code will use this to 
    // do GEP.
    if (BI == BE) {
        return EntryBlock.getFirstNonPHI();
    }

    // skip member GEPs
    while ( ++BI != BE && isMemberOfClass(TargetClassName, BI) );
    if (BI!=BE) return BI;
    // reaching end of entry BB, get first Instruction of 2nd BB
    Function::iterator FI = F.begin();
    FI++;
    return FI->getFirstNonPHI();
}

void APIRace::insertStoreInstOfTemp( Instruction *GEP, Value* Temp) {
    Instruction *DUM = new AllocaInst(Type::getInt8PtrTy(GEP->getContext()));
    DUM->insertAfter(GEP);
    IRBuilder<> IRB(DUM);
    IRB.CreateStore( GEP, Temp );
    DUM->eraseFromParent();
    return;
}

bool APIRace::instrumentLoadedAndStoredMemberInFunction( Instruction *Insert,
    std::set<Value*> &AllMemberLoads,  std::set<Value*> &AllMemberStores,
    MemberGEPToTempMap &UniqueMemberGEPs, MemberGEPToTempMap &DupMemberGEPs ) {

    if (!ClInstrumentMemberLoadAndStore || ClDisableAll) return false;

    bool Res = false;
    IRBuilder<> IRBtail(Insert);

    for (std::set<Value*>::reverse_iterator It = AllMemberLoads.rbegin(),
        End = AllMemberLoads.rend(); It != End; ++It ) {

        // insert __tsan_read
        PointerType *PT = cast<PointerType>((*It)->getType());
        int Idx = getMemoryAccessFuncIndexFromType(PT->getElementType());
        if (Idx < 0) continue;
        IRBtail.CreateCall(TsanRead[Idx], IRBtail.CreatePointerCast(
                IRBtail.CreateLoad(*It), IRBtail.getInt8PtrTy() ));
        NumInstrumentedReads++;
    }

    for (std::set<Value*>::reverse_iterator It = AllMemberStores.rbegin(),
        End = AllMemberStores.rend(); It != End; ++It ) {
        
        // insert __tsan_write
        PointerType *PT = cast<PointerType>((*It)->getType());
        int Idx = getMemoryAccessFuncIndexFromType(PT->getElementType());
        if (Idx < 0) continue;
        IRBtail.CreateCall(TsanWrite[Idx], IRBtail.CreatePointerCast(
                IRBtail.CreateLoad(*It), IRBtail.getInt8PtrTy() ));
        NumInstrumentedWrites++;
    }
    return Res;
}

/**
 * deprecated method.
 */
bool APIRace::instrumentExactLoadOrStore(Instruction *I) {
    if (!ClInstrumentMemberLoadAndStore || ClDisableAll) return false;

    IRBuilder<> IRB(I);
    bool IsWrite = isa<StoreInst>(*I);
    Value *Addr = IsWrite
        ? cast<StoreInst>(I)->getPointerOperand()
        : cast<LoadInst>(I)->getPointerOperand();
    int Idx = getMemoryAccessFuncIndex(Addr);
    if (Idx < 0) return false;
    Value *OnAccessFunc = IsWrite ? TsanWrite[Idx] : TsanRead[Idx];
    IRB.CreateCall(OnAccessFunc, IRB.CreatePointerCast(Addr, IRB.getInt8PtrTy()));
    if (IsWrite) NumInstrumentedWritesExact++;
    else         NumInstrumentedReadsExact++;
    return true;
}

int APIRace::getMemoryAccessFuncIndex(Value *Addr) {
    if (Addr==NULL) return -1;
    Type *OrigPtrTy = Addr->getType();
    if (!isa<PointerType>(OrigPtrTy)) return -1;
    Type *OrigTy = cast<PointerType>(OrigPtrTy)->getElementType();
    assert(OrigTy->isSized());
    uint32_t TypeSize = TD->getTypeStoreSizeInBits(OrigTy);
    errs() << *Addr << ' ' << TypeSize << '\n';
    if (checkSizeValidity(TypeSize)) return -1;

    size_t Idx = CountTrailingZeros_32(TypeSize / 8);
    assert(Idx < kNumberOfAccessSizes);
    return Idx;
}

int APIRace::getMemoryAccessFuncIndexFromType(Type *OrigPtrTy) {
    if (!isa<PointerType>(OrigPtrTy)) return -1;
    Type *OrigTy = cast<PointerType>(OrigPtrTy)->getElementType();
    assert(OrigTy->isSized());
    uint32_t TypeSize = TD->getTypeStoreSizeInBits(OrigTy);
    if (checkSizeValidity(TypeSize)) return -1;

    size_t Idx = CountTrailingZeros_32(TypeSize / 8);
    assert(Idx < kNumberOfAccessSizes);
    return Idx;
}

void APIRace::extraceClassInfo( Type* classType ) {
    if (classType == NULL) return;
    //errs() << *classType << '\n';
    StringRef className = getClassName(classType);
    if (className.size() == 0) return;
    if (!isa<PointerType>(classType)) return;
    Type *CT = cast<PointerType>(classType)->getElementType();
    if (!isa<StructType>(CT)) return;
    StructType* ST = cast<StructType>(CT);
    if (!ST) return;
    
    unsigned numElements = ST->getNumElements();
    std::vector<unsigned> offsets(numElements, 0);
    unsigned off = 0;
    for (unsigned i=0; i<numElements; ++i) {
        offsets[i] = off;
        Type *Arg = ST->getTypeAtIndex(i);
        //errs() << *Arg << ' ' << off << '\n';
        unsigned size = TD->getTypeStoreSizeInBits(Arg);
        if (checkSizeValidity(size)) {
            errs() << "bad size of class declaration!!!!!! Program continue, BUT can get wrong result\n";
        }
        off += size;
    }
    unsigned sizeOfClass = TD->getTypeStoreSizeInBits(CT);
    //errs() << *CT << ' ' << sizeOfClass << '\n'; 
    assert( sizeOfClass >= off );   // if pading exists
    
    
    CIM[className] = offsets;
    return;
}


// auxiliary function bodies
static int checkSizeValidity(unsigned size) {
    if ( size != 8 && size != 16 && size != 32 && size != 64
        && size != 128 ) {
        NumAccessesWithBadSize++;
        errs() << " bad size of memory access or class declaration !!!!!!!!!!!!!!!!!!!!!!!!!!\n";
        return -1;
    }
    return 0;
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
    if (PT==NULL) return StringRef("");
    if (!isa<PointerType>(PT)) return StringRef("");
    Type * baseTy = cast<PointerType>(PT)->getElementType();
    if (!isa<StructType>(baseTy)) return StringRef("");
    StructType * ST = cast<StructType>(baseTy);
    if (!ST->hasName()) return StringRef("");
    StringRef className = ST->getName();
    static StringRef prefix = StringRef("class.");
    if (!className.startswith(prefix)) return StringRef("");
    return className;
}

/**
 * Extract the class type for a given funciton 'F'.
 * Return: if 'F' is a member funciton, return its class type;
 *         otherwise, return NULL.
 */
static Type* getClassTypeOfMemberFunc(Function &F) {
    Function::arg_iterator argIt = F.arg_begin();
    Value * firstArg = argIt;
    Type * typeOfFirstArg = firstArg->getType();
    if (getClassName(typeOfFirstArg).size() != 0) return typeOfFirstArg;
    return NULL; 
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
        // FIXME:: access of B's public member in A's member function
        if (className == TargetClassName) return true;
        // this should work for static member funciton
        if (TargetClassName.size() == 0) {  // match any class name
            if (className.startswith(StringRef("class."))) return true;
        }
    }
    return false;
}

static bool isMemberOfAnyClass( Value *Member ) {
    if (!isa<GetElementPtrInst>(Member)) return false;
    GetElementPtrInst *GEP = cast<GetElementPtrInst>(Member);
    Value *base = GEP->getPointerOperand();
    Type *baseTy = base->getType();
    StringRef className = getClassName(baseTy);
    if (className.startswith(StringRef("class."))) return true;
    return false;
}
