#!/bin/bash

# 167525
R=167525

#modify the llvm revision id, if necessary
svn co -r $R http://llvm.org/svn/llvm-project/compiler-rt/trunk compiler-rt

#replace the '-Werror', which causes compilation error
sed -i 's/-Werror/-Wall/g' ./compiler-rt/lib/tsan/rtl/Makefile.old 

#make
(cd compiler-rt/lib/tsan && make -f Makefile.old libtsan CXX=clang++)

#copy libtsan.a
mkdir -p ../lib
cp ./compiler-rt/lib/tsan/rtl/libtsan.a ../lib

#cleanup
#rm -rf compiler-rt
