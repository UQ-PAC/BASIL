#include <stdio.h>

void modifyValue(int *a) {
    *a = *a + 10;
}

int main() {
    int number = 5;
    int xnumber = 25;
    modifyValue(&number);  // Passing address of the stack variable
    modifyValue(&xnumber);  // Passing address of the stack variable
    return 0;
}
