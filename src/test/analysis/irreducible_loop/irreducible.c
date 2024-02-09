// Should verify with irreducible loop resolution and loop unrolling
// run -o basil-out.bpl -a src/test/analysis/irreducible_loop/irreducible.adt -r src/test/analysis/irreducible_loop/irreducible.relf -s src/test/analysis/irreducible_loop/irreducible.spec --analyse
// boogie basil-out.bpl /useArrayAxioms /loopUnroll 15

#include <stdio.h>
//#include <assert.h>
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

int i,x;

int main(int argc, char** argv)
{
	i = 0;
H:
	if (x % 2 == 0)
		goto B;
	else
		goto A;	
A:
	puts("i");
//	printf("A-i=%d\n", i);
	i++;
	goto B;
B:
	puts("i");
//	printf("B-i=%d\n", i);
	if (i == 5) {
		goto E;
	} else {
		goto A;
	}

E:
	puts("exited\n");
//	assert(i == 5);
//	printf("We have exited!\n");
	return 0;
}
