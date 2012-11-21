#!/bin/bash

#modify the llvm revision id, if necessary
svn co -r 167525 http://llvm.org/svn/llvm-project/compiler-rt/trunk compiler-rt

#replace the '-Werror', which causes compilation error
sed -i 's/-Werror/-Wall/g' ./compiler-rt/lib/tsan/rtl/Makefile.old 

#make
(cd compiler-rt/lib/tsan && make -f Makefile.old libtsan)

#copy libtsan.a
cp ./compiler-rt/lib/tsan/rtl/libtsan.a ../lib

#cleanup
rm -rf compiler-rt
