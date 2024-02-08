#include <stdio.h>
//
//	      +---+
//    /---| H |---\
//    |	  +---+   |
//   \ /         \ /
//  +---+ <---- +---+
//  | A |       | B |
//  +---+ ----> +---+
//  		      |
//
//

int main(int argc, char** argv)
{
	int i,x;
	i = 0;
	x = 0;
H:
	if (x % 2 == 0)
		goto B;
	else
		goto A;	
A:
	printf("A-%d\n", i);
	i++;
	goto B;
B:
	printf("B-%d\n", i);
	if (i == 5) {
		goto E;
	} else {
		goto A;
	}

E:
	printf("We have exited!\n");
	return 0;
}
