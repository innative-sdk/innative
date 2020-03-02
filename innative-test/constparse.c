#include <stdarg.h>

long long add(long long n, ...)
{
  char strtest[]="ABC";
  
  long long r = 0;
  va_list args;
  va_start(args, n);
  
  for(long long i = 0; i < n; ++i)
    r += va_arg(args, long long);
  
   va_end(args);
   return r;
}

__attribute__((visibility("default"))) long long test(long long a)
{
  int cints[] = {1,2,3};
  
  const long long b = -9218868437227405313;
  return add(cints[1], a, b);
}
