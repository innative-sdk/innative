// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "test.h"
#include "../innative/queue.h"
#include <memory>

using namespace innative;

void TestHarness::test_serializer()
{
  static constexpr char MODULE[] = "(module $reverse "
                                   "\n  (global $global_i32 (export \"global_i32\") i32 i32.const 666)"
                                   "\n  (memory $memory (export \"memory_0\") 1 2)"
                                   "\n  (table $table (export \"table_0\") 10 20 funcref)"
                                   "\n  (func $print_i32_f32 (export \"print_i32_f32\") (param i32 f32))"
                                   "\n  (func $print_f64_f64 (export \"print_f64_f64\") (param f64 f64))"
                                   "\n)";

  size_t len1 = 0, len2 = 0;
  std::unique_ptr<char[]> iter1, iter2;

  {
    Environment* env = (*_exports.CreateEnvironment)(1, 0, 0);
    env->flags       = ENV_LIBRARY | ENV_DEBUG | ENV_STRICT | ENV_ENABLE_WAT;
    env->features    = ENV_FEATURE_ALL;
    env->loglevel    = LOG_FATAL;

    int err = 0;
    (*_exports.AddModule)(env, MODULE, sizeof(MODULE), "reverse", &err);
    (*_exports.FinalizeEnvironment)(env);

    (*_exports.SerializeModule)(env, 0, 0, &len1, false);
    iter1 = std::unique_ptr<char[]>(new char[len1]);
    TEST((*_exports.SerializeModule)(env, 0, iter1.get(), &len1, false) == ERR_SUCCESS);
    (*_exports.DestroyEnvironment)(env);
  }

  {
    Environment* env = (*_exports.CreateEnvironment)(1, 0, 0);
    env->flags       = ENV_LIBRARY | ENV_DEBUG | ENV_STRICT | ENV_ENABLE_WAT;
    env->features    = ENV_FEATURE_ALL;
    env->loglevel    = LOG_FATAL;

    int err = 0;
    (*_exports.AddModule)(env, iter1.get(), len1, "reverse", &err);
    (*_exports.FinalizeEnvironment)(env);

    (*_exports.SerializeModule)(env, 0, 0, &len2, false);
    iter2 = std::unique_ptr<char[]>(new char[len2]);
    TEST((*_exports.SerializeModule)(env, 0, iter2.get(), &len2, false) == ERR_SUCCESS);
    (*_exports.DestroyEnvironment)(env);
  }

  TEST(len1 == len2);
  if(len1 == len2)
    TEST(!memcmp(iter1.get(), iter2.get(), len1));
}