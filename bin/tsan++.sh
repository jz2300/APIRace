#!/bin/sh
clang++ -std=c++11 -fsanitize=thread -emit-llvm -c $1 -o $1.bc
llvm-dis $1.bc
clang++ -std=c++11 -fPIE -O1 -g -Wall -fno-builtin -c $1.bc
clang++ -std=c++11 -pie -o $1.x $1.o -lpthread -ldl ../lib/libtsan.a
