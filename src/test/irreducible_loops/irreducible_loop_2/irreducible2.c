#include <stdio.h>
//
//	+---+
//  | S |---------\ 
//  +---+         |
//    |	          |
//   \ /         \ /
//  +---+       +---+
//  | A |       | G |
//  +---+       +---+
//    |	          |  
//   \ /          |
//  +---+         |
//  | B |<--\     |
//  +---+   |     |
//    |	    |     |
//   \ /    |     |
//  +---+   |     |
//  | C |   |     |
//  +---+   |     |
//    |	    |     | 
//   \ /    |     |
//  +---+   |     |
//  | D |<--+-----/
//  +---+   |
//    |	    | 
//   \ /    |
//  +---+   |
//  | F |---/
//  +---+
//    |	      
//   \ /
//  +---+
//  | E |
//  +---+

int main(int argc, char** argv)
{
	int i,x;
	i = 0;
	x = 0;

S:
	if (x % 2 == 0 || i % 2 == 0)
		goto A;
	else
		goto G;

A:
	printf("A-%d\n", i);
	i++;
	goto B;
B:
	printf("B-%d\n", i);
	i++;
	goto C;
C:
	printf("C-%d\n", i);
	i++;
	goto D;
D:
	printf("D-%d\n", i);
	goto F;
F:
	printf("F-%d\n", i);
	if (i % 2 != 0)
		goto B;
	else
		goto E;
G:
	printf("G-%d\n", i);
	goto D;

E:
	printf("We have exited!\n");
	return 0;
}
