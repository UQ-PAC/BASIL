long x;

__attribute((noinline)) long g(long a, long c) {
    if (c >= 0) {
        return a + 10;
    } else {
        return a - 10;
    }
}

__attribute((noinline)) long h(long a, long b) {
    return g(a + b, a) + 1;
}

int main(int argc, char **argv) {
    x = h(x, x);

    return x;
}
