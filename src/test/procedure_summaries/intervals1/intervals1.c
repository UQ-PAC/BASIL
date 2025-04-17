long x;

__attribute((noinline)) long bound(int c) {
    if (c) {
        return 100;
    } else {
        return -100;
    }
}

int main(int argc, char **argv) {
    x = bound(x);
    return x;
}
