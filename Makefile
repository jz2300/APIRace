SH=bash
MAKE=make

LIBDIR=./lib

libs: rtl instr

rtl: $(LIBDIR)/libtsan.a

$(LIBDIR)/libtsan.a:
	cd tsan-rtl; $(SH) rtl.sh

instr:
	$(MAKE) -C opt -f Makefile

test:
	$(MAKE) -C tests -f Makefile

clean:
	rm -f $(LIBDIR)/*.so $(LIBDIR)/libtsan.a

