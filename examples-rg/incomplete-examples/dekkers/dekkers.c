// a slight modification on Dekker's algorithm - the first known correct mutual-exclusion algorithm
// verification depends on knowing that the exclusive regions cannot be accessed be either thread at the same time

int x;
int y;
int z;

int main() {
	return 0;
}

// requires x == 0 && y == 0 && z == 0
// ensures true
void T1(int secret) {
	int r1 = 0;
	x = 1;
	r1 = y;
	if (r1 == 0) {
		// exclusive region
		z = secret;
		z = 0;
	}
}

// requires x == 0 && y == 0 && z == 0
// ensures true
int T2() {
	int r = 0;
	int r2 = 0;
	y = 1;
	r2 = x;
	if (r2 == 0) {
		// exclusive region
		r = z;
	}
	return r;
}

------------------------------------------

#include <stdbool.h>

bool a1;
bool a2;
bool c1;
bool c2;
int z;

// L(z) = !(a1 && c1)
// L(everything else) = true

// requires !c1 && !c2
// ensures true
// Rely: (!(c1 && c2) && (c1 ==> a1) && (c2 ==> a2) ==> !(c1' && c2') && (c1' ==> a1') && (c2' ==> a2'))
//			&& (a1 == a1') && (c1 == c1')
// Guar: (!(c1 && c2) && (c1 ==> a1) && (c2 ==> a2) ==> !(c1' && c2') && (c1' ==> a1') && (c2' ==> a2'))
//			&& (a2 == a2') && (c2 == c2')
void T1(int secret) {
	a1 = 1;
	c1 = !a2;
	if (c1) {
		// exclusive region
		z = secret;
		z = 0;
	}
}

// requires !c1 && !c2
// ensures true
int T2() {
	a2 = 1;
	c2 = !a1;
	if (c2) {
		// exclusive region
		r = z;
	}
	return r;
}