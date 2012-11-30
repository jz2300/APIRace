#include <pthread.h>

void * call_back1(void* A_ptr);
void * call_back2(void* A_ptr);

class A {
    pthread_mutex_t lock;
    pthread_cond_t cond;
    int dummy;
    double member;
    int xyz;

  public:
    A() {
        pthread_mutex_init(&lock, NULL); 
        pthread_cond_init(&cond, NULL); 
    }
    ~A() { 
        pthread_mutex_destroy(&lock);
        pthread_cond_destroy(&cond); 
    }

    void * __attribute__((annotate("self-write"))) Thread1(void *x) {
        member++;
        pthread_mutex_lock(&lock);
        pthread_cond_wait(&cond,&lock);
        member++;
        pthread_mutex_unlock(&lock);
        xyz++;
        return NULL;
    }
    void *__attribute__((annotate("self-write")))  Thread2(void *x) {
        xyz++;
        pthread_mutex_lock(&lock);
        dummy++;
        pthread_cond_signal(&cond);
        pthread_mutex_unlock(&lock);
        member++;
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

