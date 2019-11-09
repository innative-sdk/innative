#include "innative/innative.h"

IN_COMPILER_DLLEXPORT extern int my_factorial(int i)
{
  if(i <= 1)
    return 1;
  return i * my_factorial(i - 1);
}
