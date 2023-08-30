#include <stdio.h>
#include <stdlib.h>

int main() {
    // Allocate memory for a char using malloc
    char* myChar = (char*) malloc(sizeof(char));

    int firstVar = 11;
    // Allocate memory for an int using malloc
    int* myInt = (int*) malloc(sizeof(int));

    // Declare a local variable
    int myLocalVar = 10;

    int* myInt2 = (int*) malloc(sizeof(int));

    int myLast = 9;

    // Assign a value to the char
    *myChar = 'A';

    // Assign a value to the int
    *myInt = 42;

    // Print the values of the char, int, and local variable
    printf("The char is: %c\n", *myChar);
    printf("The int is: %d\n", *myInt);
    printf("The local variable is: %d\n", myLocalVar);

    // Free the memory allocated using malloc
    free(myChar);
    free(myInt);

    return 0;
}

