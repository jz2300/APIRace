#include <pthread.h>

void * call_back1(void* A_ptr);
void * call_back2(void* A_ptr);

class A {
    pthread_barrier_t B;
    int dummy;
    int member;
    int xyz;

  public:
    A() { pthread_barrier_init(&B, 0, 2); }
    ~A() { pthread_barrier_destroy(&B); }

    void * __attribute__((annotate("self-write"))) Thread1(void *x) {
        member++;
        pthread_barrier_wait(&B);
        dummy = xyz;
        pthread_barrier_wait(&B);
        //member--;
        return NULL;
    }
    void *__attribute__((annotate("self-write")))  Thread2(void *x) {
        xyz++;
        pthread_barrier_wait(&B);
        member++;
        pthread_barrier_wait(&B);
        dummy++;
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

