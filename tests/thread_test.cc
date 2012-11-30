#include "Thread.hpp"
#include <iostream>

class A : public Thread {
    private:
        int xyz;

    public:
        A(): xyz(0) {}
        ~A() {}
        void * run() { xyz++; std::cerr << xyz << "\n"; return NULL;}

};


int main () {
    A aa;
    for (int i=0; i< 10; ++i) {
        aa.start();
        aa.join();
    } 
    return 0;
}
