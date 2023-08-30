// a very interesting and tricky rely to generate

// L(product) -> true
int product;

// Requires true
// Ensures true
void producer() {
	int i = 0;
	while (i < 100) {
		product++;
	}
}

// Requires true
// Ensures consumed <= 100
void consumer() {
	int consumed = 0;
	while (product >= 0) {
		product--;
		consumed++;
	}
	return consumed;
}