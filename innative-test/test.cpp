// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "benchmark.h"
#include "test.h"
#include "innative/export.h"
#include "innative/khash.h"
#include <iostream>
#include <fstream>

// This defines the testing environment that we need to inject
const char testenv[] = "(module $spectest "
                       "\n  (global $global_i32 (export \"global_i32\") i32 i32.const 666)"
                       "\n  (global $global_i64 (export \"global_i64\") i64 i64.const 666)"
                       "\n  (global $global_f32 (export \"global_f32\") f32 f32.const 666)"
                       "\n  (global $global_f64 (export \"global_f64\") f64 f64.const 666)"
                       "\n  (memory $memory1 (export \"memory\") 1 2)"
                       "\n  (table $table10 (export \"table\") 10 20 funcref)"
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

enum TEST_STAGES
{
  TEST_INTERNAL  = (1 << 0),
  TEST_BENCHMARK = (1 << 1),
  TEST_WASM_CORE = (1 << 2),
};

int main(int argc, char* argv[])
{
  innative_set_work_dir_to_bin(!argc ? 0 : argv[0]);
  int log              = LOG_WARNING;
  int stages           = 0;
  std::string temppath = temp_directory_path().u8string();

  std::cout << "inNative v" << INNATIVE_VERSION_MAJOR << "." << INNATIVE_VERSION_MINOR << "." << INNATIVE_VERSION_REVISION
            << " Test Utility" << std::endl;
  std::cout << std::endl;

  std::unique_ptr<kh_match_t, void (*)(kh_match_t*)> matchfiles(kh_init_match(), kh_destroy_match);

  for(int i = 1; i < argc; ++i)
  {
    int r;
    if(!STRICMP(argv[i], "-internal"))
      stages |= TEST_INTERNAL;
    else if(!STRICMP(argv[i], "-benchmark"))
      stages |= TEST_BENCHMARK;
    else if(!STRICMP(argv[i], "-core"))
      stages |= TEST_WASM_CORE;
    else if(!STRICMP(argv[i], "-v"))
      log = LOG_DEBUG;
    else
      kh_put_match(matchfiles.get(), argv[i], &r);
  }

  if(!stages) // If no stages were specified, we default to all of them
  {
    if(kh_size(matchfiles) == 0)
      stages = TEST_INTERNAL | TEST_BENCHMARK | TEST_WASM_CORE;
    else // However, if you specify a specific test to run, we instead default to only running the core tests
      stages = TEST_WASM_CORE;
  }

  INExports exports;
  innative_runtime(&exports);

  if(stages & TEST_INTERNAL)
  {
    TestHarness harness(exports, !argc ? 0 : argv[0], log, stderr, temppath.c_str());
    harness.Run(stdout);
  }

  if(stages & TEST_BENCHMARK)
  {
    Benchmarks benchmarks(exports, !argc ? 0 : argv[0], log, temppath.c_str());
    benchmarks.Run(stdout);
  }

  if(stages & TEST_WASM_CORE)
  {
    path testdir("../spec/test/core");
    std::vector<path> testfiles;

    for(auto& p : recursive_directory_iterator(testdir, directory_options::skip_permission_denied))
    {
      if(!STRICMP(p.path().extension().u8string().data(), ".wast"))
      {
        if(kh_size(matchfiles) > 0)
        {
          khiter_t iter = kh_get_match(matchfiles.get(), p.path().filename().u8string().data());

          if(!kh_exist2(matchfiles, iter))
            continue;
        }
#ifdef IN_PLATFORM_WIN32 // It is impossible to catch the error this test is supposed to produce on windows
        else if(!STRICMP(p.path().filename().u8string().data(), "skip-stack-guard-page.wast"))
          continue;
#endif
        testfiles.push_back(p.path());
      }
    }

    std::cout << "\nRunning through " << testfiles.size() << " official webassembly spec tests." << std::endl;

    for(auto file : testfiles)
    {
      Environment* env = (*exports.CreateEnvironment)(1, 0, (!argc ? 0 : argv[0]));
      if(!env)
      {
        std::cout << "Failed to create inNative environment, aborting." << std::endl;
        return -1;
      }

      env->flags = ENV_LIBRARY | ENV_DEBUG | ENV_STRICT | ENV_HOMOGENIZE_FUNCTIONS;
#ifdef IN_DEBUG
      env->optimize = ENV_OPTIMIZE_O0;
#else
      env->optimize = ENV_OPTIMIZE_O3;
#endif
      env->features = ENV_FEATURE_ALL;
      env->log      = stdout;
      env->loglevel = log;
      env->wasthook = [](void*) {
        fputc('.', stdout);
        fflush(stdout);
      };

      int err = (*exports.AddEmbedding)(env, 0, INNATIVE_DEFAULT_ENVIRONMENT, 0, 0);
      if(err >= 0)
        err = (*exports.CompileScript)(reinterpret_cast<const uint8_t*>(testenv), sizeof(testenv) - 1, env, false,
                                       temppath.c_str());

      if(err < 0)
      {
        FPRINTF(env->log, "Error injecting test environment, aborting.");
        (*exports.DestroyEnvironment)(env);
        return -1; // If the environment injection fails, abort everything
      }

      FPRINTF(env->log, "%s: .", file.generic_u8string().c_str());
      fflush(env->log);
      err = (*exports.CompileScript)(reinterpret_cast<const uint8_t*>(file.generic_u8string().data()), 0, env, true,
                                     temppath.c_str());

      if(!err && !env->errors)
        fputs("SUCCESS\n", env->log);
      else
      {
        fputs("FAILED\n", env->log);

        if(err < 0)
        {
          const char* strerr = (*exports.GetErrorString)(err);
          if(strerr)
            FPRINTF(env->log, "Error running script %s: %s\n", file.generic_u8string().c_str(), strerr);
          else
            FPRINTF(env->log, "Error running script %s: %i\n", file.generic_u8string().c_str(), err);
        }

        while(env->errors != nullptr)
        {
          fputs("  ", env->log);

          if(env->errors->m >= 0)
          {
            fputs(env->modules[env->errors->m].name.str(), env->log);
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
  }

  std::cout << std::endl << "Finished running tests, press enter to exit." << std::endl;
  getchar();
  return 0;
}
