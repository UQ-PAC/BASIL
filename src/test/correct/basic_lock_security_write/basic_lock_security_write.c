int x;
int z;

// Requires z != 0 && Gamma_secret == false
// Ensures true
int main(int secret) {
	z = 1;
	x = secret;
	x = 0;
	z = 0;
}
