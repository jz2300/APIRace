#include <pthread.h>

void * call_back1(void* A_ptr);

class A {
    int dummy;
    int member;

  public:
    void * __attribute__((annotate("self-write"))) Thread1(void *x) {
        dummy = member;
        return NULL;
    }

    friend void* call_back1(void* A_ptr);

    void create_thread1(pthread_t* t) {
        pthread_create(t, NULL, call_back1, this);
    }
};

void * call_back1(void* A_ptr) {
    static_cast<A*>(A_ptr)->Thread1(NULL);
    return NULL;
}

int main() {
    pthread_t tt;
    A aa;
    aa.create_thread1(&tt);
}

