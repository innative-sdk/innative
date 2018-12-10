// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "test.h"
#include "innative/export.h"
#include <iostream>
#include <filesystem>
#include <fstream>

using namespace std::filesystem;

// This defines the testing environment that we need to inject
const char testenv[] = "(module $spectest "
"\n  (global $global_i32 (export \"global_i32\") i32 i32.const 666)"
"\n  (global $global_i64 (export \"global_i64\") i64 i64.const 666)"
"\n  (global $global_f32 (export \"global_f32\") f32 f32.const 666)"
"\n  (global $global_f64 (export \"global_f64\") f64 f64.const 666)"
"\n  (memory $memory1 (export \"memory\") 1 2)"
"\n  (table $table10 (export \"table\") 10 20 anyfunc)"
"\n  (func $print (export \"print\"))"
"\n  (func $print_i32 (export \"print_i32\") (param i32))"
"\n  (func $print_i64 (export \"print_i64\") (param i64))"
"\n  (func $print_f32 (export \"print_f32\") (param f32))"
"\n  (func $print_f64 (export \"print_f64\") (param f64))"
"\n  (func $print_i32_f32 (export \"print_i32_f32\") (param i32 f32))"
"\n  (func $print_f64_f64 (export \"print_f64_f64\") (param f64 f64))"
"\n) (register \"spectest\" $spectest)";

size_t internal_tests()
{
  std::pair<const char*, void(TestHarness::*)()> tests[] = {
    { "util.h", &TestHarness::test_allocator },
    { "internal.c", &TestHarness::test_environment },
    { "path.h", &TestHarness::test_path },
    { "queue.h", &TestHarness::test_queue },
    { "stack.h", &TestHarness::test_stack },
    { "stream.h", &TestHarness::test_stream },
    { "util.h", &TestHarness::test_util },
  };

  static const size_t NUMTESTS = sizeof(tests) / sizeof(decltype(tests[0]));
  static constexpr int COLUMNS[3] = { 24, 11, 8 };
  TestHarness harness(stderr);

  printf("%-*s %-*s %-*s\n", COLUMNS[0], "Internal Tests", COLUMNS[1], "Subtests", COLUMNS[2], "Pass/Fail");
  printf("%-*s %-*s %-*s\n", COLUMNS[0], "--------------", COLUMNS[1], "--------", COLUMNS[2], "---------");

  size_t failures = 0;
  for(size_t i = 0; i < NUMTESTS; ++i)
  {
    (harness.*tests[i].second)();
    auto results = harness.Results();
    failures += results.second - results.first;

    char buf[COLUMNS[1] + 1] = { 0 };
    snprintf(buf, COLUMNS[1] + 1, "%u/%u", results.first, results.second);
    printf("%-*s %-*s %-*s\n", COLUMNS[0], tests[i].first, COLUMNS[1], buf, COLUMNS[2], (results.first == results.second) ? "PASS" : "FAIL");
  }

  printf("\n");
  return failures;
}

int main(int argc, char *argv[])
{
  innative_set_work_dir_to_bin(!argc ? 0 : argv[0]);
  IRExports exports;
  innative_runtime(&exports);

  //std::ostream& target = std::cout;
  std::ofstream target("out.txt", std::fstream::binary | std::fstream::out);
  target << "inNative v" << INNATIVE_VERSION_MAJOR << "." << INNATIVE_VERSION_MINOR << "." << INNATIVE_VERSION_REVISION << " Test Utility" << std::endl;
  target << std::endl;

  internal_tests();

  path testdir("../spec/test/core");
  std::vector<path> testfiles;
  
  for(auto& p : recursive_directory_iterator(testdir, directory_options::skip_permission_denied))
  {
    if(!STRICMP(p.path().extension().u8string().data(), ".wast"))
      testfiles.push_back(p.path());
  }

  //testfiles = { "../spec/test/core/start.wast" };
  target << "Running through " << testfiles.size() << " official webassembly spec tests." << std::endl;
  testfiles.erase(testfiles.begin(), testfiles.begin() + 50);

  for(auto file : testfiles)
  {
    Environment* env = (*exports.CreateEnvironment)(ENV_LIBRARY | ENV_DEBUG | ENV_EMIT_LLVM | ENV_STRICT | ENV_HOMOGENIZE_FUNCTIONS, 0, ENV_FEATURE_ALL, 1, 0, (!argc ? 0 : argv[0]));
    int err = (*exports.AddEmbedding)(env, 0, (void*)INNATIVE_DEFAULT_ENVIRONMENT, 0);

    if(err >= 0)
      err = innative_compile_script(reinterpret_cast<const uint8_t*>(testenv), sizeof(testenv), env, false);
    if(err < 0)
      return assert(false), -1; // If the environment injection fails, abort everything

    err = innative_compile_script((const uint8_t*)file.generic_u8string().data(), 0, env, true);
    if(!err && !env->errors)
      target << file << ": SUCCESS" << std::endl;
    else
    {
      if(err < 0)
        target << "Error running script " << file << ": " << err << std::endl;
      target << file << ": FAILED" << std::endl;
      while(env->errors != nullptr)
      {
        target << "  ";
        if(env->errors->m != nullptr)
          target << env->errors->m->name.str() << ": ";
        target << env->errors->error << std::endl;
        env->errors = env->errors->next;
      }
      target << std::endl;
    }
    (*exports.DestroyEnvironment)(env);
  }

  // Test compiling EXE
  // Test compiling DLL with no entry point, ensure init function is called
  // Test compiling DLL with entry point that gets called in the init function
  // Test compiling DLL with entry point that doesn't get called in init function

  std::cout << std::endl << "Finished running tests, press enter to exit." << std::endl;
  getchar();
  return 0;
}