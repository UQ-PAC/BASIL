int x;
int z;

// Requires x == 0 && z == 0
// Ensures true
void sync_write(int secret) {
	z = z + 1;
	x = secret;
	x = 0;
	z = z + 1;
}