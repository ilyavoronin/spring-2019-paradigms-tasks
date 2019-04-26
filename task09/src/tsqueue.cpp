#include "tsqueue.h"

void threadsafe_queue_init(ThreadsafeQueue *q) {
    pthread_mutex_init(&(q->m), NULL);
    pthread_cond_init(&(q->c), NULL);
    pthread_mutex_lock(&(q->m));
    queue_init(&(q->q));
    pthread_mutex_unlock(&(q->m));
}

void threadsafe_queue_destroy(ThreadsafeQueue *q) {
    pthread_mutex_lock(&(q->m));
    queue_destroy(&(q->q));
    pthread_mutex_unlock(&(q->m));
    pthread_mutex_destroy(&(q->m));
    pthread_cond_destroy(&(q->c));
}

void threadsafe_queue_push(ThreadsafeQueue *q, void *data) {
    pthread_mutex_lock(&(q->m));
    queue_push(&(q->q), data);
    pthread_cond_signal(&(q->c));
    pthread_mutex_unlock(&(q->m));
}

void *threadsafe_queue_wait_and_pop(ThreadsafeQueue *q) {
    pthread_mutex_lock(&(q->m));
    while (queue_empty(&(q->q))) {
        pthread_cond_wait(&(q->c), &(q->m));
    }
    void *first_elem = queue_pop(&(q->q));
    pthread_mutex_unlock(&(q->m));
    return first_elem;
}
