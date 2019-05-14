// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "test.h"

TestHarness::TestHarness(FILE* out) : _target(out), _testdata(0, 0) {}

size_t TestHarness::Run(FILE* out)
{
  std::pair<const char*, void(TestHarness::*)()> tests[] = {
    { "wasm_malloc.c", &TestHarness::test_malloc },
    { "internal.c", &TestHarness::test_environment },
    { "path.h", &TestHarness::test_path },
    { "queue.h", &TestHarness::test_queue },
    { "stack.h", &TestHarness::test_stack },
    { "stream.h", &TestHarness::test_stream },
    { "util.h", &TestHarness::test_util },
    { "allocator", &TestHarness::test_allocator },
    { "parallel parsing", &TestHarness::test_parallel_parsing}
  };

  static const size_t NUMTESTS = sizeof(tests) / sizeof(decltype(tests[0]));
  static constexpr int COLUMNS[3] = { 24, 11, 8 };

  fprintf(out, "%-*s %-*s %-*s\n", COLUMNS[0], "Internal Tests", COLUMNS[1], "Subtests", COLUMNS[2], "Pass/Fail");
  fprintf(out, "%-*s %-*s %-*s\n", COLUMNS[0], "--------------", COLUMNS[1], "--------", COLUMNS[2], "---------");

  size_t failures = 0;
  for(size_t i = 0; i < NUMTESTS; ++i)
  {
    (this->*tests[i].second)();
    auto results = Results();
    failures += results.second - results.first;

    char buf[COLUMNS[1] + 1] = { 0 };
    snprintf(buf, COLUMNS[1] + 1, "%u/%u", results.first, results.second);
    fprintf(out, "%-*s %-*s %-*s\n", COLUMNS[0], tests[i].first, COLUMNS[1], buf, COLUMNS[2], (results.first == results.second) ? "PASS" : "FAIL");
  }

  // Test compiling EXE
  // Test compiling DLL with no entry point, ensure init function is called
  // Test compiling DLL with entry point that gets called in the init function
  // Test compiling DLL with entry point that doesn't get called in init function

  fprintf(out, "\n");
  return failures;
}