#include <stdio.h>

// Function declarations
int addNumbers(int a, int b);

int callAddFromAnotherFunction(int x, int y) {
    return addNumbers(x, y);
}

int callFromFun2(int x, int y) {
    return addNumbers(x, y);
}

int addNumbers(int a, int b) {
    return a + b;
}

int main() {
    int resultFromMain = addNumbers(10, 5);
    int resultFromOtherFunc = callAddFromAnotherFunction(20, 15);
    int resultFromFun2 = callFromFun2(30, 25);
    return 0;
}
