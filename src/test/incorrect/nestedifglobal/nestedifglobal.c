int x;
int z;

int main(int a) {
    x = a;
    int y = 0;
    if (x == 0) { // Insecure
        y = 3;
    }

     if (x == 1) {
        y = 5;
    }

     if (y == 3) {
        y = z; // Insecure
    }
    return z;
}
