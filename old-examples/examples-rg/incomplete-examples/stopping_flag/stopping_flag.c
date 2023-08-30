int pending_io;
int stopping_flag;
int stopping_event;
int stopped;

// req: pending_io == 1 && stopping_flag == 0 && stopping_event == 0 && stopped == 0
void T1() {
	stopping_flag = 1;
	pending_io--;
	pending_t1 = pending_io;
	if (pending_t1 == 0) {
		stopping_event = 1;
	}
	assume stopping_event;
	stopped = 1;
}

// todo
void T2() {
	if (stopping_flag) {
		status := -1;
	} else {
		pending_io := pending_io + 1;
		status := 0;
	}
	if (status == 0) {
		assert (!stopped);
		stopped = 0;
	}
	pending_io := pending_io - 1;
	pending_t2 := pending_io;
	if (pending_t2 == 0) {
		stopping_event = 1;
	}
}