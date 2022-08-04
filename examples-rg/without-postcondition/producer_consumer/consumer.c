int product;

// Requires product >= 0
// Ensures true
void consumer(int secret) {
	int r = 0;
	while (product > 0) 
		// invariant 0 <= r <= product
	{
		product--;
		r = product;
	}
	if (r == 0) {
		return r;
	} else {
		return r + secret;
	}
}