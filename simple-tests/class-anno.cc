// RUN: %clangxx_tsan -O1 %s -o %t && %t 2>&1 | FileCheck %s
#include <pthread.h>
#include <stdio.h>

class A {
    static int Global;

  public:
    static void * __attribute__((annotate("self-write"))) Thread1(void *x) {
        Global++;
        return NULL;
    }

    static void *__attribute__((annotate("self-write")))  Thread2(void *x) {
        Global--;
        return NULL;
    }
};

int A::Global = 0;

int main() {
  pthread_t t[2];
  pthread_create(&t[0], NULL, A::Thread1, NULL);
  pthread_create(&t[1], NULL, A::Thread2, NULL);
  pthread_join(t[0], NULL);
  pthread_join(t[1], NULL);
}

// CHECK: WARNING: ThreadSanitizer: data race
