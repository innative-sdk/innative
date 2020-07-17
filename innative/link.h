// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__LINK_H
#define IN__LINK_H

#include "constants.h"
#include "filesys.h"
#include <vector>
#include <string>

namespace innative {
  IN_ERROR LinkEnvironment(const Environment* env, const path& file);
  void DeleteCache(const Environment& env, Module& m);
  void DeleteContext(Environment& env, bool shutdown);
  std::vector<std::string> GetSymbols(const char* file, size_t size, FILE* log, LLD_FORMAT format);
  void AppendIntrinsics(Environment& env);
  IN_COMPILER_DLLEXPORT size_t ABIMangleBuffer(const char* src, char* buffer, size_t count, ABI abi, int convention,
                                               int bytes);
  inline std::string ABIMangle(const std::string& src, ABI abi, int convention, int bytes)
  {
    std::string buf;
    buf.resize(ABIMangleBuffer(src.c_str(), 0, 0, abi, convention, bytes) + 1);
    buf.resize(ABIMangleBuffer(src.c_str(), buf.data(), buf.size(), abi, convention, bytes));
    return buf;
  }
  int GetParameterBytes(const IN_WASM_MODULE& m, const Import& imp);
  IN_ERROR GenerateLinkerObjects(const Environment& env, std::vector<std::string>& cache);
  int CallLinker(const Environment* env, std::vector<const char*>& linkargs, LLD_FORMAT format);
  path GetLinkerObjectPath(const Environment& env, Module& m, const path& outfile);
  IN_ERROR CompileEnvironment(Environment* env, const char* file);
  IN_ERROR CompileEnvironmentJIT(Environment* env, bool expose_process);
  int GetCallingConvention(const Import& imp);
  IN_ERROR OutputObjectFile(Compiler& context, const path& out);
  void* LoadJITFunction(void* env, const char* s);
}

#endif
