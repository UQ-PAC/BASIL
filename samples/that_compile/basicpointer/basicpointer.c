#include<stdlib.h>

void main() {
    int *foo;
        // foo -> ?
    
    int *bar = malloc(1 * sizeof(int));
        // foo -> ?; bar -> alloc-1

    foo = &bar;
        // foo -> bar -> alloc-1

    foo = bar;
        // foo -> alloc-1; bar -> alloc-1

    *foo = bar;
        // foo -> alloc-1; bar -> alloc-1; alloc-1 -> alloc-1;

    bar = *foo;
        // foo -> bar -> bar; malloc-1
}