PASS=APIRaceInstr
LIBDIR = ../lib
FLAG=-apirace

ctests = $(patsubst %.c,%.c.bc,$(wildcard *.c))
cctests = $(patsubst %.cc,%.cc.bc,$(wildcard *.cc))

alltests = $(patsubst %.bc,%.instr.bc, $(ctests) $(cctests))

test: $(ctests) $(cctests) $(alltests)

%.c.bc: %.c
	clang -emit-llvm -c $< -o $@

%.cc.bc: %.cc
	clang++ -emit-llvm -c $< -o $@

%.instr.bc: %.bc
	opt -load $(LIBDIR)/$(PASS).so $(FLAG) < $< > $@

clean:
	rm -r *.bc *.ll
