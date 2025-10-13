#include <stdbool.h>
#include <stdlib.h>

typedef struct {
    long int a;
    long int b;
    long int c;
} str;


str global;

void __attribute__ ((noinline)) set_fields(str *ext) {

    str tmp = {1,2,3};
    *ext = tmp;

}

int main()
{
    global.a = 3;
    set_fields(&global);
}