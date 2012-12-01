#include <pthread.h>

void * call_back1(void* A_ptr);
void * call_back2(void* A_ptr);

class A {
    pthread_barrier_t B;
    char dummy;
    int member;
    double xyz;
    float abc;

    int flag;

  public:
    A() { pthread_barrier_init(&B, 0, 2); flag = 1;}
    ~A() { pthread_barrier_destroy(&B); }

    void * __attribute__((annotate("self-write"))) Thread1(void *x) {
        void * retval = NULL;
        // __tsan_entry_func()        

        // 1 :: instr(abc,xyz);
        // [1] == AllLoadedMembers
        // [2] == AllStoredMembers
        // [3] == MemberLoadsAndStores
        //
        // [1] [2] [3] empty
        abc++;

        // [1] [2] {abc} 
        if (flag) {
            dummy++; // Exact :: instr(dummy);
            internal1();
            abc++; // Exact :: instr(abc);
            retval = &flag; // Exact :: instr(flag);
            goto RETURN;
            // 2 :: instr(abc); 
            // [3] { load dummy ; load abc; load flag; store ... }
        }
        else {
            xyz++; 
            retval =  &abc;
            goto RETURN;
            // [1] [2] { abc, xyz }
            // 2 :: instr( xyz, abc );
        }
        member++;
        // [1] [2] { abc, xyz, member };
        // 2 :: instr( abc, xyz, member );
        
    RETURN:
        // instr ( abc, xyz, member } ===> BEGIN
        return retval;

        // instrument [3] { ... }
        // Move member GEP just after 'this1' in 1st BB
        // instrument entry/exit
        //
    }
    void *__attribute__((annotate("self-write")))  Thread2(void *x) {
        internal2();
        return NULL;
    }


    void internal1() {
        abc++;
        if (flag) {
            member++;
            pthread_barrier_wait(&B);
            xyz++;
        }
    }

    void internal2() {
        xyz++;
        if (flag) pthread_barrier_wait(&B);
        dummy++;
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

