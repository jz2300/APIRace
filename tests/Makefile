PASS=APIRaceInstr
LIBDIR = ../lib
CXXFLAG =
FLAG=-apirace

CXXFLAGS=-Wall -fPIE -fno-builtin 
#OPTFLAGS=-O2 -fno-inline-functions
OPTFLAGS=-O1 
RTL=-lpthread -ldl /home/jing/src/APIRace/lib/libtsan.a
RTLMOD=-lpthread -ldl /home/jing/src/APIRace/lib/libtsan-mod.a

tasks = $(patsubst %.cc,%.task,$(wildcard *.cc))

all: $(tasks)

%.task: %.cc
	clang++ $(CXXFLAGS) $(OPTFLAGS) -pie $< -o $*.x -lpthread
	../bin/clang-apirace.sh $(OPTFLAGS) -c $<
	../bin/clang-apirace.sh $(OPTFLAGS) $*.o -o $*.api.x
	clang++ $(CXXFLAGS) $(OPTFLAGS) -emit-llvm -c $*.cc.instr.bc -o $*.cc.instr.o1.bc
	llvm-dis $*.cc.instr.bc
	llvm-dis $*.cc.instr.o1.bc
	clang++ -fsanitize=thread $(CXXFLAGS) $(OPTFLAGS) -c $< -emit-llvm -o $*.cc.tsan.bc
	clang++ $(CXXFLAGS) $(OPTFLAGS) -emit-llvm -c $*.cc.tsan.bc -o $*.cc.tsan.o1.bc
	llvm-dis $*.cc.tsan.bc
	llvm-dis $*.cc.tsan.o1.bc
	clang++ -pie $(CXXFLAGS) $(OPTFLAGS) $*.cc.tsan.bc -o $*.tsan.x $(RTL)
	clang++ -pie $(CXXFLAGS) $(OPTFLAGS) $*.cc.tsan.bc -o $*.tsan.mod.x $(RTLMOD)

clean:
	rm -rf *.x *.bc *.ll *.o *.stat
