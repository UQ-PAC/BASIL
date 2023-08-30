#include <stdlib.h>

void main() {
    int *foo = malloc(1 * sizeof(int));
    // foo -> alloc-1

    *foo = 1;
    // foo -> alloc-1

    free(foo);
    // foo -> nothing?
}