long x;

__attribute((noinline)) long bound(long a) {
    while (a < 100) {
        a++;
    }
    return a;
}

int main(int argc, char **argv) {
    x = bound(0);
    return x;
}
