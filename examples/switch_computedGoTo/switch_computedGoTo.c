#include <stdlib.h>

void main(int argc, char** argv) {
	int new = 0;
	switch (argc) {
		case 3: new = r();
		case 1: new = 1; break;
        case 2: new = 2;
        case 4: new = 3; break;
        case 5: new = 4;
		default: new = 5;
	}

    int r() {
        return 1;
    }
}
