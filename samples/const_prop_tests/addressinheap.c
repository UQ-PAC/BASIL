#include <stdlib.h>

int main() {
	int x = 5;
	
	int* ptr1 = malloc(sizeof(int));
	int** ptr2 = malloc(sizeof(int*));
	int*** ptr3 = malloc(sizeof(int**));
	
	ptr2 = &ptr1;
	ptr3 = &ptr2;

	*ptr1 = x;

	int y = *(*(*ptr3));
	
	return 0;
}
