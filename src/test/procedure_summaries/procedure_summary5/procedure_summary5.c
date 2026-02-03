long x;

long h(long a, long b);

__attribute((noinline)) long g(long a) {
    return h(0, a - 1);
}

__attribute((noinline)) long h(long a, long b) {
    return g(a + b) + 1;
}

__attribute((noinline)) long f(long z) {
    return h(z, z * z);
}

int main(int argc, char **argv) {
    x = f(x);

    return x;
}

