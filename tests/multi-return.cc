#include <pthread.h>

void * call_back1(void* A_ptr);
void * call_back2(void* A_ptr);

class A {
    char flag;
    char flag2;
    int member1;
    int member2;
    double xyz;

  public:
    A() : flag(1), flag2(0) {}
    void * __attribute__((annotate("self-write"))) Thread1(void *x) {
        return NULL;
        if (flag) {
            member1++;
            return NULL;
        } else if (flag2) {
            return NULL;
        }
        member2++;
        return NULL;
    }
    void *__attribute__((annotate("self-write")))  Thread2(void *x) {
        if (flag) {
            //member2++;
            return NULL;
        }
        member1=1;
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

