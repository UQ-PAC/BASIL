
int x;

// ens: true
void something(int secret) {
	if (z == 1) {
		x = secret;
		y = 1;
	} else if (z == 2) {
		x = 0;
		y = 0;
	}
	z = 0;
}

// ens: true
void otherThing() {
	int r = 0;
	if (z == 0 && y == 0) {
		r = x;
	}
	return r;
}