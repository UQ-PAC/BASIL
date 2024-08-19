// noinline + -O2 to avoid using the stack (while still having a seperate procedure for f)

int x;

int c;

__attribute((noinline)) int f(int a, int b) {
    if (c) {
        return a;
    } else {
        return b;
    }
}

int main() {
    x = f(x, 1);

    return 0;
}
