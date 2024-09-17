#include <stdio.h>

void greet() {
    printf("Hello, world!\n");
}

int main() {
    void (*funcPtr)() = greet;  // Function pointer to greet function

    printf("Calling greet() indirectly:\n");
    (*funcPtr)();  // Indirect function call using function pointer

    return 0;
}

