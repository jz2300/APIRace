#include "Thread.hpp"
#include <unistd.h>

class foo : public Thread {
    public:
        void *run() { return 0; }
};

int main() {
    foo my_foo;
    my_foo.start();
    usleep(50000);
    //my_foo.join();
    return 0;

}
