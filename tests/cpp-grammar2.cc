class A {
    public:
        int val;
        A(): val(0) {}
        A(int n) : val(n) {}
        int get() {return val;}

};

static void add (A &a, A &b) { a.val += b.val; }

int main() {
    A  a(1), b(2);
    add( a, b );
    return a.get();
}
