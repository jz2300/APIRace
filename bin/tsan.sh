#!/bin/sh
clang -fsanitize=thread -emit-llvm -c $1 -o $1.bc
llvm-dis $1.bc
clang -fPIE -O1 -g -Wall -fno-builtin -c $1.bc
clang -pie -o $1.x $1.o -lpthread -ldl ../lib/libtsan.a
