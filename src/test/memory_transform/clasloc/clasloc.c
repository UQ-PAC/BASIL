#define LO 0
#define HI 1

int z;

int source() {
    z = LO;
    return 42;
}

int main() {
    return source();
}