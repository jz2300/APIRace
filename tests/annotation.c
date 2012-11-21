// RUN: %clang_tsan -O1 %s -o %t && %t 2>&1 | FileCheck %s
#include <pthread.h>
#include <stdio.h>

int Global;

void * __attribute__((annotate("self-write"))) Thread1(void *x) {
  Global = 42;
  return NULL;
}

void * __attribute__((annotate("self-write"))) Thread2(void *x __attribute__((annotate("foo"))) ) {
  Global = 43;
  return NULL;
}

int main() {
  pthread_t t[2];
  pthread_create(&t[0], NULL, Thread1, NULL);
  pthread_create(&t[1], NULL, Thread2, NULL);
  pthread_join(t[0], NULL);
  pthread_join(t[1], NULL);
  return 0;
}

/*
 * @llvm.global.annotations = appending global [2 x { i8*, i8*, i8*, i32 }] [{ i8*, i8*, i8*, i32 } { i8* bitcast (i8* (i8*)* @Thread1 to i8*), i8* getelementptr inbounds ([11 x i8]* @.str, i32 0, i32 0), i8* getelementptr inbounds ([14 x i8]* @.str1, i32 0, i32 0), i32 7 }, { i8*, i8*, i8*, i32 } { i8* bitcast (i8* (i8*)* @Thread2 to i8*), i8* getelementptr inbounds ([11 x i8]* @.str, i32 0, i32 0), i8* getelementptr inbounds ([14 x i8]* @.str1, i32 0, i32 0), i32 12 }], section "llvm.metadata"
 *
 * array of {i8*, i8*, i8*, i32 }
 * i8*  : a constant expression, the GV being annotated
 * i8*  : a constant string of anotation
 * i8*  : a constatn string containing the name of the translation unit
 * i32  : the line number in the file
 *
 */ 

// CHECK: WARNING: ThreadSanitizer: data race
