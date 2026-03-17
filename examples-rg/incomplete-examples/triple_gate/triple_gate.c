
void enter(int secret) {
	x = 0;
	if (x == 0) {
		x = 1;
		if (y == 0) {
			z = secret;
			z = 0;
		}
	}
}

void other() {
	if (x == 1) {
		y = 1;
	}
}