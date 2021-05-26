// Copyright (c)2021 Fundament Software
// For conditions of distribution and use, see copyright notice in innative.h

#include "test.h"
#include <stdio.h>
#include <stdarg.h>
#include "../innative/utility.h"
#include "../innative/constants.h"

TestHarness::TestHarness(const INExports& exports, const char* arg0, int loglevel, FILE* out, const path& folder) :
  _exports(exports), _arg0(arg0), _loglevel(loglevel), _target(out), _folder(folder), _testdata(0, 0)
{
  assert(_target);
}

TestHarness::~TestHarness()
{
  // Clean up all the files we just produced
  Clean();
}

void TestHarness::Clean()
{
  for(auto f : _garbage)
    remove(f);
}

int TestHarness::Log(const Environment* env, const char* f, ...)
{
  va_list args;
  va_start(args, f);
  int len = VPRINTF(f, args);
  va_end(args);
  return len;
}

size_t TestHarness::Run(FILE* out)
{
  std::pair<const char*, void (TestHarness::*)()> tests[] = { { "wasm_malloc.c", &TestHarness::test_malloc },
                                                              { "assemblyscript", &TestHarness::test_assemblyscript },
                                                              { "whitelist", &TestHarness::test_whitelist },
                                                              { "debugging.cpp", &TestHarness::test_debug },
                                                              { "embedding", &TestHarness::test_embedding },
                                                              { "JIT", &TestHarness::test_jit },
                                                              { "funcreplace.c", &TestHarness::test_funcreplace },
                                                              { "internal.c", &TestHarness::test_environment },
                                                              { "queue.h", &TestHarness::test_queue },
                                                              { "stack.h", &TestHarness::test_stack },
                                                              { "stream.h", &TestHarness::test_stream },
                                                              { "utility.h", &TestHarness::test_util },
                                                              { "manual", &TestHarness::test_manual },
                                                              { "allocator", &TestHarness::test_allocator },
                                                              { "parallel parsing", &TestHarness::test_parallel_parsing },
                                                              { "serializer", &TestHarness::test_serializer },
                                                              { "errors", &TestHarness::test_errors },
                                                              { "atomic_waitnotify",
                                                                &TestHarness::test_atomic_waitnotify } };

  static const size_t NUMTESTS    = sizeof(tests) / sizeof(decltype(tests[0]));
  static constexpr int COLUMNS[3] = { 24, 11, 8 };

  FPRINTF(out, "%-*s %-*s %-*s\n", COLUMNS[0], "Internal Tests", COLUMNS[1], "Subtests", COLUMNS[2], "Pass/Fail");
  FPRINTF(out, "%-*s %-*s %-*s\n", COLUMNS[0], "--------------", COLUMNS[1], "--------", COLUMNS[2], "---------");

  size_t failures = 0;
  for(size_t i = 0; i < NUMTESTS; ++i)
  {
    (this->*tests[i].second)();
    auto results = Results();
    failures += results.second - results.first;

    char buf[COLUMNS[1] + 1] = { 0 };
    snprintf(buf, COLUMNS[1] + 1, "%u/%u", results.first, results.second);
    FPRINTF(out, "%-*s %-*s %-*s\n", COLUMNS[0], tests[i].first, COLUMNS[1], buf, COLUMNS[2],
            (results.first == results.second) ? "PASS" : "FAIL");
  }

  {
    TEST(CompileWASM("../scripts/test-h.wat", nullptr) == ERR_SUCCESS);
#ifdef IN_PLATFORM_WIN32
  #ifdef IN_32BIT
    TEST(CompileWASM("../scripts/test-win32-cref.wat", nullptr) == ERR_SUCCESS);
  #else
    TEST(CompileWASM("../scripts/test-win64.wat", nullptr) == ERR_SUCCESS);
    TEST(CompileWASM("../scripts/test-win64-cref.wat", nullptr) == ERR_SUCCESS);
  #endif
#endif

    char buf[COLUMNS[1] + 1] = { 0 };
    auto results             = Results();
    snprintf(buf, COLUMNS[1] + 1, "%u/%u", results.first, results.second);
    FPRINTF(out, "%-*s %-*s %-*s\n", COLUMNS[0], "aux tests", COLUMNS[1], buf, COLUMNS[2],
            (results.first == results.second) ? "PASS" : "FAIL");
  }

  FPRINTF(out, "\n");
  return failures;
}

void TestHarness::DoTestError(int test, const char* text, int result, const char* file, int line)
{
  DoTest(test == result, text, file, line);
  if(test != result)
    FPRINTF(_target, "  Return Value: %i\n", test);
  // innative::utility::EnumToString(innative::utility::ERR_ENUM_MAP, test, buf, 32)); // Useful for debugging
}

void* TestHarness::LoadAssembly(const path& file)
{
  auto s  = file.u8string();
  void* m = (*_exports.LoadAssembly)(s.c_str());
  if(!m)
  {
    char* e = (*_exports.LoadAssemblyError)();
    if(e && _target)
    {
      FPRINTF(_target, "  Failed to load %s: %s\n", s.c_str(), e);
      (*_exports.LoadAssemblyErrorFree)(e);
    }
  }
  return m;
}

int TestHarness::CompileWASM(const path& file, int (TestHarness::*fn)(void*), const char* system,
                             std::function<int(Environment*)> preprocess, const char* name, const char* out)
{
  Environment* env = (*_exports.CreateEnvironment)(1, 0, 0);
  if(!env)
    return ERR_UNKNOWN_ENVIRONMENT_ERROR;
  env->flags    = ENV_ENABLE_WAT | ENV_LIBRARY;
  env->optimize = ENV_OPTIMIZE_O3;
  env->features = ENV_FEATURE_ALL;
  env->loghook  = &TestHarness::Log;
  env->loglevel = _loglevel;
  if(system)
    env->system = system;

#ifdef IN_DEBUG
  env->flags |= ENV_DEBUG;
  env->optimize = ENV_OPTIMIZE_O0;
#endif

  if(preprocess)
    if(int err = preprocess(env); err < 0)
      return err;

  auto stem = file.stem().u8string();
  int err   = (*_exports.AddEmbedding)(env, 0, (void*)(*_exports.GetDefaultEmbedding)(TestHarness::Debug), 0, 0);
  if(err >= 0)
    (*_exports.AddModule)(env, file.u8string().c_str(), 0, (!name ? stem.c_str() : name), &err);

  if(err >= 0)
    (*_exports.FinalizeEnvironment)(env);

  path base = _folder / (!out ? file.stem() : out);
  _out      = base;
#ifdef IN_PLATFORM_WIN32
  base.replace_extension(".lib");
  remove(base);
#endif
  _out.replace_extension(IN_LIBRARY_EXTENSION);
  remove(_out);

  if(err >= 0)
    err = (*_exports.Compile)(env, _out.u8string().c_str());

  while(env->errors != nullptr)
  {
    fputs("  ", stdout);

    if(env->errors->m >= 0)
    {
      fputs(env->modules[env->errors->m].name.str(), stdout);
      fputs(": ", stdout);
    }

    fputs(env->errors->error, stdout);
    fputc('\n', stdout);
    env->errors = env->errors->next;
  }

  (*_exports.DestroyEnvironment)(env);

  if(err < 0)
    return err;

  _garbage.push_back(_out);
#ifdef IN_PLATFORM_WIN32
  _garbage.push_back(base);
#endif
  void* m = LoadAssembly(_out);
  if(!m)
    return ERR_RUNTIME_INVALID_ASSEMBLY;
  if(fn)
    err = (this->*fn)(m);
  (*_exports.FreeAssembly)(m);

  return err;
}
