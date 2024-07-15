int x;

int c;

int f(int a, int b) {
    if (c) {
        return a;
    } else {
        return b;
    }
}

int main() {
    c = 0;
    x = f(x, 1);

    return 0;
}
