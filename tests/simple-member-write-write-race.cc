#include <pthread.h>
#include <cstdio>

void * call_back1(void* A_ptr);
void * call_back2(void* A_ptr);

class A {
    int member;
    int res;

  public:
    void * __attribute__((annotate("self-write"))) Thread1(void *x) {
        member++;
        return this;
    }
    void * __attribute__((annotate("self-write")))  Thread2(void *x) {
        member = res;
        return this;
    }

    friend void* call_back1(void* A_ptr);
    friend void* call_back2(void* A_ptr);

    void create_thread1(pthread_t* t) {
        pthread_create(t, NULL, call_back1, this);
    }
    void create_thread2(pthread_t* t) {
        pthread_create(t, NULL, call_back2, this);
    }
};

void * call_back1(void* A_ptr) {
    return static_cast<A*>(A_ptr)->Thread1(NULL);
}

void * call_back2(void* A_ptr) {
    return static_cast<A*>(A_ptr)->Thread2(NULL);
}

int main() {
    pthread_t t[2];
    A aa;
    aa.create_thread1(&t[0]);
    aa.create_thread2(&t[1]);
    pthread_join(t[0], NULL);
    pthread_join(t[1], NULL);
}

