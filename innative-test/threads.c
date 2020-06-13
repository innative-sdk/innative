#include <threads.h>
//#include <atomic>
#define NUM_THREADS     5

__attribute__((visibility("default"))) static int lock;
static int result;

int PrintHello(void* threadid)
{
  int tid = (long)threadid;
  result |= tid << (tid * 4);
  thrd_exit(tid * 4);
}

__attribute__((visibility("default"))) int begin()
{
  lock = 0;
  result = 0;
  thrd_t threads[NUM_THREADS];
  int rc;
  long t;
  for(t=0; t<NUM_THREADS; t++){
     rc = thrd_create(&threads[t], PrintHello, (void *)t);
     if(rc)
        return 0;
  }

  result = 1;
  /* Wait for threads */
  for(t=0; t<NUM_THREADS; t++)
    thrd_join(threads[t], &rc);
  
  return result;
}
