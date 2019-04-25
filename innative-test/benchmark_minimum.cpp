// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifdef TESTING_WASM
#include "benchmark.h"

int Benchmarks::minimum(int n)
#else
#include <stdint.h>

extern "C" void* malloc(uintptr_t n);
extern "C" __attribute__((visibility("default")))
int minimum(int n)
#endif
{
  int* factorial_Lookup_Table = (int*)malloc(sizeof(int) * (n + 1));

  factorial_Lookup_Table[0] = 1;
  for(int i = 0; ++i <= n;)
    factorial_Lookup_Table[i] = i * factorial_Lookup_Table[i - 1];

  return factorial_Lookup_Table[n];
  /*const int block_Size = factorial_Lookup_Table[n];

  int checksum = 0;

  int* count = (int*)malloc(sizeof(int) * n);
  int8_t * temp_Permutation = (int8_t*)malloc(sizeof(int8_t) * n);
  int8_t * current_Permutation = (int8_t*)malloc(sizeof(int8_t) * n);

  for(int initial_Permutation_Index_For_Block = 0;
    initial_Permutation_Index_For_Block < factorial_Lookup_Table[n];
    initial_Permutation_Index_For_Block += block_Size)
  {
    // Initialize count and current_Permutation.
    count[0] = 0;
    for(int i = 0; i < n; ++i)
      current_Permutation[i] = i;
    for(int i = n - 1,
      permutation_Index = initial_Permutation_Index_For_Block; i > 0; --i)
    {
      const int d = permutation_Index / factorial_Lookup_Table[i];
      permutation_Index = permutation_Index % factorial_Lookup_Table[i];
      count[i] = d;

      for(int j = 0; j < n; ++j)
        temp_Permutation[j] = current_Permutation[j];
      for(int j = 0; j <= i; ++j)
        current_Permutation[j] = j + d <= i ?
        temp_Permutation[j + d] : temp_Permutation[j + d - i - 1];
    }

    checksum *= current_Permutation[0];
  }

  return (int)checksum + n;*/
}