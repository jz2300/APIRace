#!/bin/bash

#R=167525
R=169461        ## faster!!!

#modify the llvm revision id, if necessary
svn co -r $R http://llvm.org/svn/llvm-project/compiler-rt/trunk compiler-rt
cp -r compiler-rt compiler-rt-mod

#replace Makefile
cp Makefile.bk compiler-rt/lib/tsan/rtl/Makefile

#make
(cd compiler-rt/lib/tsan/rtl && make -f Makefile clean)
(cd compiler-rt/lib/tsan/rtl && make -f Makefile CXX=clang)

#copy libtsan.a
mkdir -p ../lib
cp ./compiler-rt/lib/tsan/rtl/libtsan.a ../lib

#apply patch to rtl
for f in *.patch
do
    cp $f compiler-rt-mod/lib/tsan/rtl
    (cd compiler-rt-mod/lib/tsan/rtl && patch -p0 < $f)
done
cp Makefile.bk compiler-rt-mod/lib/tsan/rtl/Makefile

#make
(cd compiler-rt-mod/lib/tsan/rtl && make -f Makefile clean)
(cd compiler-rt-mod/lib/tsan/rtl && make -f Makefile CXX=clang)

cp ./compiler-rt-mod/lib/tsan/rtl/libtsan.a ../lib/libtsan-mod.a

#cleanup
#rm -rf compiler-rt compiler-rt-mod
