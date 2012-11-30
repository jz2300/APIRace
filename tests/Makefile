PASS=APIRaceInstr
LIBDIR = ../lib
FLAG=-apirace

CXXFLAG=-O1 -g -Wall -fno-builtin

cctests = $(patsubst %.cc,%.cc.bc,$(wildcard *.cc))

exes = $(patsubst %.bc,%.instr.x,$(cctests))

instrlls = $(patsubst %.bc,%.instr.ll,$(cctests))

test: $(instrlls) $(exes)

%.cc.bc: %.cc
	clang++ -emit-llvm -c $< -o $@

%.instr.bc: %.bc
	opt -load $(LIBDIR)/$(PASS).so $(FLAG) < $< > $@

%.instr.ll: %.instr.bc
	llvm-dis $<

%.instr.o: %.instr.bc
	clang++ -fPIE $(CXXFLAG) -c $<

%.instr.x: %.instr.o
	clang++ -pie -o $@ $< -lpthread -ldl $(LIBDIR)/libtsan.a

clean:
	rm -rf *.bc *.x *.o *.ll
