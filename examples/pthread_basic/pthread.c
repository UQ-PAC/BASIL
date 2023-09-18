#include <pthread.h>
#include <unistd.h>

void *perform_work(void *arguments){
        int index = *((int *)arguments);
        sleep(index);
        return NULL;
}

int main(void) {
        pthread_t thread;
        int thread_args = 5;

        pthread_create(&thread, NULL, perform_work, &thread_args);

        pthread_join(thread, NULL);

        return 0;
}