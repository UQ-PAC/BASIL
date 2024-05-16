#include <stdlib.h>
int* callee(int* arg) {
    int* ret = arg + sizeof(int); // this is wrong
    return ret;
}

int main() {
    int *bar = malloc(5 * sizeof(int));
    *bar = 12;
    int* foo = bar + sizeof(int);
    *foo = 13;
    int* bat = callee(foo);
    *bat = 14;
}
