SH=bash
MAKE=make

LIBDIR=./lib

libs: $(LIBDIR)/libtsan.a $(LIBDIR)/APIRaceInstr.so

$(LIBDIR)/libtsan.a:
	cd tsan-rtl; $(SH) rtl.sh

$(LIBDIR)/APIRaceInstr.so:
	$(MAKE) -C opt -f Makefile

clean:
	rm -f lib/*.so libtsan.a

