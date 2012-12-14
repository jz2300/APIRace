MAKE = make
MAKEFILE_DIR = makefiles
PATCH_DIR = patch

all: pbzip2-apirace pbzip2-tsan

pbzip2-1.1.6:
	(cd tarball; tar xvf pbzip2-1.1.6.tar.gz; mv pbzip2-1.1.6 ../)
	cp $(PATCH_DIR)/pbzip2.cpp.patch pbzip2-1.1.6/
	(cd pbzip2-1.1.6; patch -p0 < pbzip2.cpp.patch)	

pbzip2-apirace: pbzip2-1.1.6
	cp $(MAKEFILE_DIR)/Makefile.pbzip2.apirace pbzip2-1.1.6
	$(MAKE) -C pbzip2-1.1.6 -f Makefile.pbzip2.apirace

pbzip2-tsan: pbzip2-1.1.6
	cp $(MAKEFILE_DIR)/Makefile.pbzip2.tsan pbzip2-1.1.6
	$(MAKE) -C pbzip2-1.1.6 -f Makefile.pbzip2.tsan	


pbzip2-clean:
	$(MAKE) -C pbzip2-1.1.6 -f Makefile.pbzip2.apirace clean
	$(MAKE) -C pbzip2-1.1.6 -f Makefile.pbzip2.tsan clean
	rm -f pbzip2-1.1.6/Makefile.pbzip2.apirace pbzip2-1.1.6/Makefile.pbzip2.tsan

clean: pbzip2-clean
