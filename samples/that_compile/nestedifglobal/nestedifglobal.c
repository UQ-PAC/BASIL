int x;
int z;

int main() {
    int y = 0;
    if (x == 0) { // Insecure
        y = 3;
    } else if (x == 1) {
        y = 5;
    } else if (x == 2) {
        y = z; // Insecure
    }
}
