// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef __FLAGS_H__IN__
#define __FLAGS_H__IN__

#ifdef  __cplusplus
extern "C" {
#endif

enum WASM_ENVIRONMENT_FLAGS
{
  ENV_DEBUG = (1 << 0), // Enables debug information
  ENV_LIBRARY = (1 << 1), // Builds a dynamic library instead of an executable.
  ENV_WHITELIST = (1 << 2), // Enables the C import whitelist (if the whitelist is empty, no C imports will be allowed)
  ENV_ENABLE_WAT = (1 << 3), // Enables compiling .wat and .wast files
  ENV_MULTITHREADED = (1 << 4), // Compiles each module in parallel (experimental)
  ENV_EMIT_LLVM = (1 << 5), // Emits intermediate LLVM IR files for debugging
  ENV_HOMOGENIZE_FUNCTIONS = (1 << 6), // Converts all exported functions to i64 types for testing
  ENV_NO_INIT = (1 << 7), // Disables automatic initialization in DLLs, requiring you to manually call IN_INIT_FUNCTION and IN_EXIT_FUNCTION
  ENV_CHECK_STACK_OVERFLOW = (1 << 10), // Ensures a stack probe exists for large stack allocations even on systems where it is optional (linux)
  ENV_CHECK_FLOAT_TRUNC = (1 << 11),
  ENV_CHECK_MEMORY_ACCESS = (1 << 12), // Put in bounds checks around all memory accesses
  ENV_CHECK_INDIRECT_CALL = (1 << 13), // Verifies the function call signature of all indirect calls
  ENV_CHECK_INT_DIVISION = (1 << 14), 
  ENV_DISABLE_TAIL_CALL = (1 << 15), // Disables tail calls (demanded by current webassembly standard)

  // Strictly adheres to the standard, provided the optimization level does not exceed ENV_OPTIMIZE_STRICT
  ENV_STRICT = ENV_CHECK_STACK_OVERFLOW | ENV_CHECK_FLOAT_TRUNC | ENV_CHECK_MEMORY_ACCESS | ENV_CHECK_INDIRECT_CALL | ENV_DISABLE_TAIL_CALL | ENV_CHECK_INT_DIVISION | ENV_WHITELIST,
  // Only inserts checks required to maintain a sandbox
  ENV_SANDBOX = ENV_CHECK_STACK_OVERFLOW | ENV_CHECK_MEMORY_ACCESS | ENV_CHECK_INDIRECT_CALL | ENV_WHITELIST,
};

enum WASM_OPTIMIZE_FLAGS
{
  ENV_OPTIMIZE_FAST_MATH_REASSOCIATE = (1 << 0),
  ENV_OPTIMIZE_FAST_MATH_NO_NAN = (1 << 1), // Assume no NaN
  ENV_OPTIMIZE_FAST_MATH_NO_INF = (1 << 2), // Assume no Inf
  ENV_OPTIMIZE_FAST_MATH_NO_SIGNED_ZERO = (1 << 3),
  ENV_OPTIMIZE_FAST_MATH_ALLOW_RECIPROCAL = (1 << 4),
  ENV_OPTIMIZE_FAST_MATH_CONTRACT = (1 << 5),
  ENV_OPTIMIZE_FAST_MATH_ALLOW_APPROXIMATE_FUNCTIONS = (1 << 6),
  ENV_OPTIMIZE_FAST_MATH = ENV_OPTIMIZE_FAST_MATH_REASSOCIATE | ENV_OPTIMIZE_FAST_MATH_NO_NAN | ENV_OPTIMIZE_FAST_MATH_NO_INF | ENV_OPTIMIZE_FAST_MATH_NO_SIGNED_ZERO | ENV_OPTIMIZE_FAST_MATH_ALLOW_RECIPROCAL | ENV_OPTIMIZE_FAST_MATH_CONTRACT | ENV_OPTIMIZE_FAST_MATH_ALLOW_APPROXIMATE_FUNCTIONS,
  ENV_OPTIMIZE_O0 = 0, // no optimization
  ENV_OPTIMIZE_O1 = (1 << 8), // some optimization
  ENV_OPTIMIZE_O2 = (2 << 8), // more optimization
  ENV_OPTIMIZE_O3 = (3 << 8), // all the optimization
  ENV_OPTIMIZE_Os = (4 << 8), // minimize code size
  ENV_OPTIMIZE_OMASK = (7 << 8),
  ENV_OPTIMIZE_STRICT = ENV_OPTIMIZE_O3, // Only performs optimizations that cannot invalidate the standard
  ENV_OPTIMIZE_ALL = ENV_OPTIMIZE_O3 | ENV_OPTIMIZE_FAST_MATH, // Performs all optimizations, but will never compromise the sandbox.
};

enum WASM_FEATURE_FLAGS
{
  ENV_FEATURE_MUTABLE_GLOBALS = (1 << 0),
  ENV_FEATURE_ALL = ~0,
};

#ifdef  __cplusplus
}
#endif

#endif