// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__FLAGS_H
#define IN__FLAGS_H

#ifdef __cplusplus
extern "C" {
#endif

enum WASM_ENVIRONMENT_FLAGS
{
  // Adds debugging information to the webassembly module, either as a PDB file or as a DWARF file, depending on platform.
  ENV_DEBUG       = (3 << 0),
  ENV_DEBUG_PDB   = (2 << 0), // Forces PDB generation
  ENV_DEBUG_DWARF = (1 << 0), // Forces DWARF generation

  // Specifies that the result should be a dynamic library instead of an executable. Any start function will be ignored, and
  // the resulting DLL or `.so` file will export all symbols that are exported from all the modules being compiled. These
  // symbol names will be mangled, but can be accessed from C if you know the resulting mangled name. This does not produce
  // a static library because the dynamic library performs initialization and cleanup of global variables when loaded and
  // unloaded. If you know what you're doing, you can perform this initialization yourself by specifying ENV_NOINIT.
  ENV_LIBRARY = (1 << 2),

  // Enables the C function whitelist.If the whitelist is not enabled, the webassembly module will be able to call any C
  // function that it is linked to, which includes all kernel functions on windows. Enabling the whitelist without providing
  // a list of functions will effectively prevent the webassembly module from calling any external C function.
  ENV_WHITELIST = (1 << 3),

  // Enables compiling .wat and .wast files
  ENV_ENABLE_WAT = (1 << 4),

  // Attempts to compile the modules in parallel as much as possible (Experimental).
  ENV_MULTITHREADED = (1 << 5),

  // Outputs a `.llvm` file in the target output directory for each module being compiled that outputs the optimized, final
  // LLVM IR that is compiled to machine code. This can be used to investigate compiler bugs or unexpected behavior.
  ENV_EMIT_LLVM = (1 << 6),

  // When testing, due to C++ not being able to dynamically generate calling conventions, it is useful to "homogenize" all
  // functions to always return `i64` and transform every single parameter into an `i64` parameter. This makes it easier to
  // generate function pointers for the test harness.This may be useful for game scripting APIs in certain contexts, but in
  // most cases is unnecessary and simply adds overhead.
  ENV_HOMOGENIZE_FUNCTIONS = (1 << 7),

  // Normally, inNative will create a dynamic library that automatically calls it's initialization function when it is
  // loaded into memory, using the operating system's global initialization handles, and free it's global resources when it
  // is unloaded. However, a game may want more precise control over when a webassembly module is actually instantiated or
  // freed, and may want the option of freeing and re-instantiating the module without having to unload it from memory. To
  // prevent the initialization and cleanup functions from being automatically called, use this flag, but be sure you call
  // them from your code correctly.
  ENV_NO_INIT = (1 << 8),

  // Some platforms, like windows, always require a stack probe if there is any possibility of skipping the stack guard
  // page. This option ensures that a stack probe is always done, even on linux, if a large stack space is requested. This
  // is critical for sandboxing, because otherwise the stack overflow can be used to break out of the program memory space.
  ENV_CHECK_STACK_OVERFLOW = (1 << 10),

  // This forces a trap if any floating point truncation to an integer would overflow or underflow the integer due to
  // precision issues. For example, a 64-bit float can accurate hold 52-bit integers with no precision loss, but if you
  // truncate this 64-bit float into a 32-bit integer, the result could potentially overflow the 32-bit integer.
  ENV_CHECK_FLOAT_TRUNC = (1 << 11),

  // This inserts memory bounds checks on all load and store operations, which is critical for sandboxing applications, but
  // comes at a significant performance cost. Some of these checks can be simplified for certain platforms - 32-bit linear
  // memories on a 64-bit machine only require a single instruction plus a jump, whereas a 64-bit linear memory on a 64-bit
  // platform requires five instructions and a jump.
  ENV_CHECK_MEMORY_ACCESS = (1 << 12),

  // This inserts a check that verifies the expected type of the indirect function call matches the actual type of the
  // function that is about to be called. This is required for sandboxing because a function call mismatch can smash the
  // stack, but comes at a small performance cost.
  ENV_CHECK_INDIRECT_CALL = (1 << 13),

  // Integer division is not actually guaranteed to throw a trap on most hardware, and is considered undefined behavior by
  // LLVM, which can affect optimizations. The webassembly standard requires that division by zero always throws a trap, so
  // this inserts checks for both division by zero and MAX_INT overflow edge cases.
  ENV_CHECK_INT_DIVISION = (1 << 14),

  // The webassembly standard currently does not allow tail calls to prevent stack overflows from turning into endless loops
  // that lock up a web browser. This option is provided purely for compatibility with the standard.
  ENV_DISABLE_TAIL_CALL = (1 << 15),

  // DWARF's "is_stmt" flag marks which assembly lines are actually source code statements, but it is not always reliable.
  ENV_DEBUG_DETECT_IS_STMT = 0, // By default, we check if there are is_stmt flags anywhere and if they exist we use them.
  ENV_DEBUG_USE_IS_STMT    = (1 << 20), // ONLY generates debug information for lines marked with is_stmt, no matter what.
  ENV_DEBUG_IGNORE_IS_STMT = (2 << 20), // ALWAYS generates debug information for all assembly lines, ignoring is_stmt.

  // Strictly adheres to the standard, provided the optimization level does not exceed ENV_OPTIMIZE_STRICT.
  ENV_STRICT = ENV_CHECK_STACK_OVERFLOW | ENV_CHECK_FLOAT_TRUNC | ENV_CHECK_MEMORY_ACCESS | ENV_CHECK_INDIRECT_CALL |
               ENV_DISABLE_TAIL_CALL | ENV_CHECK_INT_DIVISION | ENV_WHITELIST,

  // Only inserts checks required to safely isolate a webassembly module in a sandbox.
  ENV_SANDBOX = ENV_CHECK_STACK_OVERFLOW | ENV_CHECK_MEMORY_ACCESS | ENV_CHECK_INDIRECT_CALL | ENV_WHITELIST,
};

enum WASM_OPTIMIZE_FLAGS
{
  ENV_OPTIMIZE_FAST_MATH_REASSOCIATE                 = (1 << 0),
  ENV_OPTIMIZE_FAST_MATH_NO_NAN                      = (1 << 1), // Assume no NaN
  ENV_OPTIMIZE_FAST_MATH_NO_INF                      = (1 << 2), // Assume no Inf
  ENV_OPTIMIZE_FAST_MATH_NO_SIGNED_ZERO              = (1 << 3),
  ENV_OPTIMIZE_FAST_MATH_ALLOW_RECIPROCAL            = (1 << 4),
  ENV_OPTIMIZE_FAST_MATH_CONTRACT                    = (1 << 5),
  ENV_OPTIMIZE_FAST_MATH_ALLOW_APPROXIMATE_FUNCTIONS = (1 << 6),
  ENV_OPTIMIZE_FAST_MATH                             = ENV_OPTIMIZE_FAST_MATH_REASSOCIATE | ENV_OPTIMIZE_FAST_MATH_NO_NAN |
                           ENV_OPTIMIZE_FAST_MATH_NO_INF | ENV_OPTIMIZE_FAST_MATH_NO_SIGNED_ZERO |
                           ENV_OPTIMIZE_FAST_MATH_ALLOW_RECIPROCAL | ENV_OPTIMIZE_FAST_MATH_CONTRACT |
                           ENV_OPTIMIZE_FAST_MATH_ALLOW_APPROXIMATE_FUNCTIONS,
  ENV_OPTIMIZE_O0     = 0,        // no optimization
  ENV_OPTIMIZE_O1     = (1 << 8), // some optimization
  ENV_OPTIMIZE_O2     = (2 << 8), // more optimization
  ENV_OPTIMIZE_O3     = (3 << 8), // all the optimization
  ENV_OPTIMIZE_Os     = (4 << 8), // minimize code size
  ENV_OPTIMIZE_OMASK  = (7 << 8),
  ENV_OPTIMIZE_STRICT = ENV_OPTIMIZE_O3, // Only performs optimizations that cannot invalidate the standard
  ENV_OPTIMIZE_ALL    = ENV_OPTIMIZE_O3 |
                     ENV_OPTIMIZE_FAST_MATH, // Performs all optimizations, but will never compromise the sandbox.
};

enum WASM_FEATURE_FLAGS
{
  ENV_FEATURE_MUTABLE_GLOBALS = (1 << 0), // https://github.com/WebAssembly/mutable-global
  ENV_FEATURE_ALL             = ~0,
};

#ifdef __cplusplus
}
#endif

#endif