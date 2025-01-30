int x;

__attribute((noinline)) int g(int a, int b) {
    return a + b;
}

__attribute((noinline)) int f(int a) {
    return g(a, a) + 1;
}

int main() {
    x = f(x);
}
