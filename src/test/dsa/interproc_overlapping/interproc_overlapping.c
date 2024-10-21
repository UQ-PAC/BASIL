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

long int test(bool b, str *heap) {
    str stack;
    stack.a = 3;
    stack.c = 10;
    str* ptr = b ? heap : &stack;
    set_fields(ptr);
    return stack.c;
}

int main()
{

    test(false, malloc(sizeof(str)));

}