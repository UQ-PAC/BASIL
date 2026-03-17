/*
This program can only be verified using a technique which considers a "mutual postcondition" - one that holds when *both* threads have returned.
It can otherwise be proven by changing the thread-specific postconditions to:
T1: x == 1 && (y == 0 || y == 1)
T2: (x == 0 || x == 1) && y == 1
And deriving the mutual postcondition as their conjunction:
x == 1 && y == 1.
*/

int x;
int y;

// req: x == 0 && y == 0
// ens: x == 1 && y == 1
void inc_x() {
	x++;
}

// req: x == 0 && y == 0
// ens: x == 1 && y == 1
void inc_y() {
	y++;
}