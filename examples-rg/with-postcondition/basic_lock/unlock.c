int x;
int z;

// Requires z == 1
// Ensures true
void unlock() {
	x = 1;
	z = 0;
}