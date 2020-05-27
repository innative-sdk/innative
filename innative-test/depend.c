#include "innative/innative.h"

extern int test(int a, int b);

IN_COMPILER_DLLEXPORT int call_test(int a, int b, int c)
{
  return test(a, b + c);
}
