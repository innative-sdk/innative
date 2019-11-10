// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__COMPILE_H
#define IN__COMPILE_H

#include "innative/schema.h"
#include <vector>
#include <string>

namespace innative {
  enum class LLD_FORMAT : unsigned char
  {
    COFF,
    ELF,
    WASM,
  };

  enum class ABI : unsigned char
  {
    Win32,
    Win64,
    POSIX,
  };

  #ifdef IN_PLATFORM_WIN32
  #ifdef IN_CPU_x86
  static constexpr ABI CURRENT_ABI = ABI::Win32;
  #else
  static constexpr ABI CURRENT_ABI = ABI::Win64;
  #endif
  #else
  static constexpr ABI CURRENT_ABI = ABI::POSIX;
  #endif

  IN_ERROR CompileEnvironment(const Environment* env, const char* file);
  void DeleteCache(const Environment& env, Module& m);
  void DeleteContext(Environment& env, bool shutdown = false);
  std::vector<std::string> GetSymbols(const char* file, FILE* log, LLD_FORMAT format);
  void AppendIntrinsics(Environment& env);
  std::string ABIMangle(const std::string& src, ABI abi, int convention, int bytes);
  int GetCallingConvention(const Import& imp);
  int GetParameterBytes(const IN_WASM_MODULE& m, const Import& imp);
}

#endif
