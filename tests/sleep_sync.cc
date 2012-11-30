#include "Thread.hpp"
#include <unistd.h>

static void MySleep() {
    usleep(50*1000);
}

class foo : public Thread {
    private:
        int X;
    public:
        foo() : X(0) {}
        ~foo() {}
        void setX(int t) { X = t; }
        void *run() {
            MySleep();
            X = 42;
            return NULL;
        }
};

int main() {
    foo my_foo;

    my_foo.start();
    my_foo.setX(43);
    my_foo.join();
    return 0;
}
