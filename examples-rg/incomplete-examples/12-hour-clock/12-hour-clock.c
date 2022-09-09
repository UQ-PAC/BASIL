int hours;
int morning;
int evening;

// req: hours == 0 && morning == 1 && evening == 0
void T1() {
	while (1) {
		while (morning != 1);
		while (morning == 1 && hours < 12) {
			hours++;
		}
		hours = 0;
		morning = 0;
		evening = 0;
		// ens: hours <= 12 && morning == 0 && evening == 1 || hours == 0 && morning == 1 && evening == 0
	}
}

void T2() {
	while (1) {
		while (evening != 1);
		while (evening == 1 && hours < 12) {
			hours++;
		}
		hours = 0;
		morning = 1;
		evening = 0;
		// ens: hours <= 12 && morning == 1 && evening == 0 || hours == 0 && morning == 0 && evening == 1
	}
}