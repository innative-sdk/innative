// Copyright (c)2021 Fundament Software
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__TEST_H
#define IN__TEST_H

#include "innative/export.h"
#include <utility>
#include <stdint.h>
#include <stdio.h>
#include <vector>
#include <string.h>
#include <functional>
#include "../innative/filesys.h"

#ifdef IN_PLATFORM_WIN32
  #define IN_STATIC_EXTENSION  ".lib"
  #define IN_LIBRARY_EXTENSION ".dll"
#else
  #define IN_STATIC_EXTENSION  ".a"
  #define IN_LIBRARY_EXTENSION ".so"
#endif

class TestHarness
{
public:
  TestHarness(const INExports& exports, const char* arg0, int loglevel, FILE* out, const path& folder);
  ~TestHarness();
  size_t Run(FILE* out);
  void test_allocator();
  void test_atomic_waitnotify();
  void test_environment();
  void test_debug();
  void test_queue();
  void test_stack();
  void test_stream();
  void test_util();
  void test_manual();
  void test_assemblyscript();
  void test_parallel_parsing();
  void test_serializer();
  void test_whitelist();
  void test_jit();
  void test_malloc();
  void test_embedding();
  void test_variadic();
  void test_errors();
  void test_funcreplace();
  void test_wasi();
  int CompileWASM(const path& file, int (TestHarness::*fn)(void*), const char* system = nullptr,
                  std::function<int(Environment*)> preprocess = std::function<int(Environment*)>(), const char* name = nullptr, const char* out = nullptr);
  void Clean();
  int do_debug(void* assembly);
  int do_debug_2(void* assembly);
  int do_funcreplace(void* assembly);
  int do_embedding(void* assembly);
  int do_embedding2(void* assembly);
  int do_variadic(void* assembly);
  int do_wasi(void* assembly);

  inline std::pair<uint32_t, uint32_t> Results()
  {
    auto r    = _testdata;
    _testdata = { 0, 0 };
    return r;
  }

  void* LoadAssembly(const path& file);
  static int Log(const Environment* env, const char* f, ...);

#ifdef IN_DEBUG
  static const bool Debug = true;
#else
  static const bool Debug = false;
#endif

  protected:
  inline void DoTest(bool test, const char* text, const char* file, int line)
  {
    ++_testdata.second;

    const char* f = strrchr(file, '/');
    if(!f)
      f = strrchr(file, '\\');
    if(f)
      file = f + 1;

    if(test)
      ++_testdata.first;
    else
      fprintf(_target, "%s[%i]: %s\n", file, line, text);
  }

  void DoTestError(int test, const char* text, int result, const char* file, int line);

  std::pair<uint32_t, uint32_t> _testdata;
  FILE* _target;
  const INExports& _exports;
  const char* _arg0;
  int _loglevel;
  std::vector<path> _garbage;
  path _folder;
  path _out;
};

#define TEST(x) DoTest(x, "" #x, __FILE__, __LINE__)
#define TESTERR(x, err) DoTestError(x, "" #x " == " #err, err, __FILE__, __LINE__)
#endif