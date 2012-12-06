PASS=APIRaceInstr
LIBDIR = ../lib
CXXFLAG = 
FLAG=-apirace

CXX_OPT_FLAG=-O2 -Wall -fno-builtin -fno-inline-functions

cctests = $(patsubst %.cc,%.cc.bc,$(wildcard *.cc))

exes = $(patsubst %.bc,%.instr.x,$(cctests))

instrlls = $(patsubst %.bc,%.instr.ll,$(cctests))

test: $(instrlls) $(exes)

%.cc.bc: %.cc
	clang++ $(CXX_OPT_FLAG) -emit-llvm -c $< -o $@

%.instr.bc: %.bc
	opt -load $(LIBDIR)/$(PASS).so $(FLAG) < $< > $@

%.instr.ll: %.instr.bc
	llvm-dis $<

%.instr.o: %.instr.bc
	clang++ $(CXXFLAG) -fPIE $(CXX_OPT_FLAG) -c $<

%.instr.x: %.instr.o
	clang++ $(CXXFLAG) -pie -o $@ $< -lpthread -ldl $(LIBDIR)/libtsan.a

clean:
	rm -rf *.bc *.x *.o *.ll
