int x;

// Requires x == 0
// Ensures x == 10 || x == 20 || x == 21
void loop() {
	while (x < 10)
		// invariant x <= 10 || x == 20 || x == 21
	{
		x = x + 1;
	}
}
