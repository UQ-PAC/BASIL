#include <stdlib.h>

void main() {
	int i = 1;
	int new = 0;
	switch (i) {
		case 3: new = 3;
		case 1: new = 1; break;
		default: new = 5;
	}
}
