#include <pthread.h>

int x;

// Requires x == 0
// Ensures x == 5 || x == 6
void* assign(void* arg) {
	x = 5;
  pthread_exit(NULL);
}

// Requires x == 0
// Ensures x == 1 || x == 5 || x == 6
void* increment(void* arg) {
	x = x + 1;
  pthread_exit(NULL);
}

int main() {
  pthread_t assign_thread, increment_thread;

  pthread_create(&assign_thread, NULL, assign, NULL);
  pthread_create(&increment_thread, NULL, increment, NULL);

  pthread_join(increment_thread, NULL);
  pthread_join(assign_thread, NULL);
  return 0;
}