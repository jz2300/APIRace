#include <pthread.h>
#include <unistd.h>

class Thread {
    private:
        pthread_t thread;
    protected:
        static void* callback( void * thread_ptr );
    public:
        Thread();
        ~Thread();
        virtual void * run() = 0;
        void join();
        void start();
        void stop();
};

Thread::Thread() {
}

Thread::~Thread() {
}

void* Thread::callback( void * thread_ptr ) {
    Thread * tptr = reinterpret_cast<Thread*>(thread_ptr);
    void * res = tptr->run();
    return res;
}

void Thread::start() {
    pthread_create(&thread, NULL, Thread::callback, this);
}

void Thread::stop() {
    pthread_cancel(thread);
    join();
}

void Thread::join() {
    pthread_join (thread, NULL);
}
