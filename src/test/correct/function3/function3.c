int x;

int g(int a, int b) {
    return a + b;
}

int f(int a) {
    return g(a, a);
}

int main() {
    return f(x);
}
