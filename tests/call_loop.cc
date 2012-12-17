#include <pthread.h>

void * call_back1(void* A_ptr);
void * call_back2(void* A_ptr);

class A {
    char flag;
    char flag2;
    int mem[10];
    int member;
    double xyz;

  public:
    A() : flag(1), flag2(0) {}
    void * __attribute__((annotate("self-write"))) Thread1(void *x) {
        return NULL;
    }
    void *__attribute__((annotate("self-write")))  Thread2(void *x) {
        f(100);
        return NULL;
    }

    void f(int x) {
        if (x>0)
        g(x-1);
    }

    void g(int x) {
        if (x>0)
        f(x-1);
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
    static_cast<A*>(A_ptr)->Thread1(NULL);
    return NULL;
}

void * call_back2(void* A_ptr) {
    static_cast<A*>(A_ptr)->Thread2(NULL);
    return NULL;
}

int main() {
    pthread_t t[2];
    A aa;
    aa.create_thread1(&t[0]);
    aa.create_thread2(&t[1]);
    pthread_join(t[0], NULL);
    pthread_join(t[1], NULL);
}

