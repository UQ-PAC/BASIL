int x;
int z;

// Requires z != 0
// Ensures true
void write(int secret) {
	z = 1;
	x = secret;
	x = 0;
	z = 0;
}
