class A {
    public:
        int val;
        A(int n) : val(n) {}

};

class B {
    private:
        int abc;
    public:
        B(int n) : abc(n) {}
        void add( A &a ) { abc += a.val; }
        int get() {return abc;}
};

int main() {
    A  a(1);
    B  b(2);
    b.add(a);
    return b.get();
}
