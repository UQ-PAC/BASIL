/*
This program should fail to verify.
*/

// Requires true
// Ensures true
void T1(int secret) {
	x = &secret;
}

// Requires true
// Ensures true
void T2() {
	int r = 5;
	int x = &r;
	y = *x
}