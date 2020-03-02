// Copyright (c)2020 Black Sphere Studios
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

class TestHarness
{
public:
  TestHarness(const INExports& exports, const char* arg0, int loglevel, FILE* out, const path& folder);
  ~TestHarness();
  size_t Run(FILE* out);
  void test_allocator();
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
  void test_malloc();
  void test_embedding();
  void test_variadic();
  void test_errors();
  void test_funcreplace();
  int CompileWASM(const path& file, int (TestHarness::*fn)(void*), const char* system = nullptr,
                  std::function<int(Environment*)> preprocess = std::function<int(Environment*)>());
  int do_debug(void* assembly);
  int do_debug_2(void* assembly);
  int do_funcreplace(void* assembly);
  int do_embedding(void* assembly);
  int do_variadic(void* assembly);

  inline std::pair<uint32_t, uint32_t> Results()
  {
    auto r    = _testdata;
    _testdata = { 0, 0 };
    return r;
  }

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

  std::pair<uint32_t, uint32_t> _testdata;
  FILE* _target;
  const INExports& _exports;
  const char* _arg0;
  int _loglevel;
  std::vector<path> _garbage;
  path _folder;
};

#define TEST(x) DoTest(x, "" #x, __FILE__, __LINE__)

#endif