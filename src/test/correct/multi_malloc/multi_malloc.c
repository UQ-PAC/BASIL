#include <stdio.h>
#include <stdlib.h>

int main() {
    // Allocate memory for a char using malloc
    char* myChar = (char*) malloc(sizeof(char));

    // Allocate memory for an int using malloc
    int* myInt = (int*) malloc(sizeof(int));

    // Assign a value to the char
    *myChar = 'A';

    // Assign a value to the int
    *myInt = 42;

    // Print the values of the char and int
    printf("The char is: %c\n", *myChar);
    printf("The int is: %d\n", *myInt);

    // Free the memory allocated using malloc
    free(myChar);
    free(myInt);

    return 0;
}

