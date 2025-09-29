#include <stdbool.h>
#include <stdlib.h>

typedef struct {
    long int a;
    long int b;
    long int c;
} str;

void __attribute__ ((noinline)) set_fields(str *ext) {
    ext->a = 1;
    ext->c = 2;
}

int main()
{
    str push; // to get variable stack isnt the first thing on the stack
    push.a = 4;
    str stack;
    stack.a = 3;
    stack.c = 10;
    set_fields(&stack);
}