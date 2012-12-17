#include <pthread.h>

void * call_back1(void* A_ptr);
void * call_back2(void* A_ptr);

class A {
    char flag;
    int member;
    double xyz;

  public:
    A() : flag(0) {}
    void * __attribute__((annotate("self-write"))) Thread1(void *x) {
        if (flag) {
            member++;
        } else {
            xyz++;
        }          
        return NULL;
    }
    void *__attribute__((annotate("self-write")))  Thread2(void *x) {
        xyz = 0;
        return NULL;
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

