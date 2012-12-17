class A {
    private:
        int val;

    public:
        A(): val(0) {}
        A(int n) : val(n) {}
        void add( A &a ) { val += a.val; }
        int get() {return val;}

};

int main() {
    A  a(1), b(2);
    a.add(b);
    return a.get();
}
