#include "Thread.hpp"
#include <iostream>

class foo : public Thread {
    public:
        foo() {}
        ~foo() {}
        void *run() { return 0; }
};

int main() {
    foo my_foo;
    my_foo.start();
    my_foo.join();
    std::cout<< "PASS\n";
    return 0;

}
