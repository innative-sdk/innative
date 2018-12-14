// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "test.h"
#include "innative/export.h"
#include "innative/khash.h"
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

// We use khash instead of unordered_set so we can make it case-insensitive
KHASH_INIT(match, kh_cstr_t, char, 0, kh_str_hash_funcins, kh_str_hash_insequal);

size_t internal_tests()
{
  std::pair<const char*, void(TestHarness::*)()> tests[] = {
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

  std::cout << "inNative v" << INNATIVE_VERSION_MAJOR << "." << INNATIVE_VERSION_MINOR << "." << INNATIVE_VERSION_REVISION << " Test Utility" << std::endl;
  std::cout << std::endl;

  internal_tests();

  std::unique_ptr<kh_match_t, void(*)(kh_match_t*)> matchfiles(kh_init_match(), kh_destroy_match);
  
  {
    int r;
    for(int i = 1; i < argc; ++i)
    {
      if(!STRICMP(argv[i], "-internal"))
        return 0; // If we only want the internal tests, just bail out at this point
      kh_put_match(matchfiles.get(), argv[i], &r);
    }
  }

  path testdir("../spec/test/core");
  std::vector<path> testfiles;

  for(auto& p : recursive_directory_iterator(testdir, directory_options::skip_permission_denied))
  {
    if(!STRICMP(p.path().extension().u8string().data(), ".wast"))
    {
      if(kh_end(matchfiles) > 0)
      {
        khiter_t iter = kh_get_match(matchfiles.get(), p.path().filename().u8string().data());
        if(!kh_exist2(matchfiles, iter))
          continue;
      }
      testfiles.push_back(p.path());
    }
  }

  std::cout << "Running through " << testfiles.size() << " official webassembly spec tests." << std::endl;

  for(auto file : testfiles)
  {
    Environment* env = (*exports.CreateEnvironment)(1, 0, (!argc ? 0 : argv[0]));
    env->flags = ENV_LIBRARY | ENV_DEBUG | ENV_EMIT_LLVM | ENV_STRICT | ENV_HOMOGENIZE_FUNCTIONS;
    env->optimize = 0;
    env->features = ENV_FEATURE_ALL;
    env->log = stdout;
    env->wasthook = [](void*) { fputc('.', stdout); fflush(stdout); };
    int err = (*exports.AddEmbedding)(env, 0, (void*)INNATIVE_DEFAULT_ENVIRONMENT, 0);

    if(err >= 0)
      err = innative_compile_script(reinterpret_cast<const uint8_t*>(testenv), sizeof(testenv), env, false);
    if(err < 0)
      return assert(false), -1; // If the environment injection fails, abort everything

    FPRINTF(env->log, "%s: .", file.generic_u8string().c_str());
    fflush(env->log);
    err = innative_compile_script((const uint8_t*)file.generic_u8string().data(), 0, env, true);

    if(!err && !env->errors)
      fputs("SUCCESS\n", env->log);
    else
    {
      fputs("FAILED\n", env->log);
      if(err < 0)
        FPRINTF(env->log, "Error running script %s: %i\n", file.generic_u8string().c_str(), err);
      while(env->errors != nullptr)
      {
        fputs("  ", env->log);
        if(env->errors->m != nullptr)
        {
          fputs(env->errors->m->name.str(), env->log);
          fputs(": ", env->log);
        }
        fputs(env->errors->error, env->log);
        fputc('\n', env->log);
        env->errors = env->errors->next;
      }
      fputc('\n', env->log);
      fflush(env->log);
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