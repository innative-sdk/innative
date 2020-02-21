// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "test.h"
#include "innative/export.h"
#include <memory>
#include <atomic>
#include <thread>

void TestHarness::test_parallel_parsing()
{
  static constexpr char MODULE_TEMPLATE[] = "(module $parallel%i "
                                            "\n  (global $global_i32 (export \"global_i32_%i\") i32 i32.const 666)"
                                            "\n  (global $global_i64 (export \"global_i64_%i\") i64 i64.const 666)"
                                            "\n  (global $global_f32 (export \"global_f32_%i\") f32 f32.const 666)"
                                            "\n  (global $global_f64 (export \"global_f64_%i\") f64 f64.const 666)"
                                            "\n  (memory $memory (export \"memory_%i\") 1 2)"
                                            "\n  (table $table (export \"table_%i\") 10 20 funcref)"
                                            "\n  (func $print (export \"print_%i\"))"
                                            "\n  (func $print_i32 (export \"print_i32_%i\") (param i32))"
                                            "\n  (func $print_i64 (export \"print_i64_%i\") (param i64))"
                                            "\n  (func $print_f32 (export \"print_f32_%i\") (param f32))"
                                            "\n  (func $print_f64 (export \"print_f64_%i\") (param f64))"
                                            "\n  (func $print_i32_f32 (export \"print_i32_f32_%i\") (param i32 f32))"
                                            "\n  (func $print_f64_f64 (export \"print_f64_f64_%i\") (param f64 f64))"
                                            "\n)";

  const int NUM = 50;
  {
    std::unique_ptr<std::string[]> modules(new std::string[NUM]);

    for(int i = 0; i < NUM; ++i)
    {
      modules[i].resize(sizeof(MODULE_TEMPLATE));
      modules[i].resize(
        SPRINTF((char*)modules[i].data(), modules[i].size(), MODULE_TEMPLATE, i, i, i, i, i, i, i, i, i, i, i, i, i, i));
    }

    Environment* env = (*_exports.CreateEnvironment)(1, 0, 0);
    env->flags       = ENV_LIBRARY | ENV_DEBUG | ENV_STRICT | ENV_MULTITHREADED | ENV_ENABLE_WAT;
    env->features    = ENV_FEATURE_ALL;
    env->loglevel    = LOG_FATAL;

    std::unique_ptr<int[]> err(new int[NUM]);
    {
      for(int i = 0; i < NUM; ++i)
        (*_exports.AddModule)(env, modules[i].data(), modules[i].size(), "parallel", &err[i]);

      (*_exports.FinalizeEnvironment)(env);
    }

    for(int i = 0; i < NUM; ++i)
      TEST(!err[i]);

    (*_exports.DestroyEnvironment)(env);
  }
}