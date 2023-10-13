int __attribute__((noinline)) func2() {
    return 2;
}

int __attribute__((noinline)) func1() {
    return func2();
}

int main(int argc) {
    return func1();
}