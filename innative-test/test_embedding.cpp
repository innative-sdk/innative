// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "test.h"
#include "../innative/utility.h"

using namespace innative;

#ifdef IN_DEBUG
  #define TEST_EMBEDDING "innative-test-embedding-d" IN_STATIC_EXTENSION
#else
  #define TEST_EMBEDDING "innative-test-embedding" IN_STATIC_EXTENSION
#endif

int TestHarness::do_embedding(void* assembly)
{
  constexpr int i = 4;
  constexpr int j = 2;

  int (*test)(int, int) = (int (*)(int, int))(*_exports.LoadFunction)(assembly, "embedded", "test");
  TEST(test != nullptr);

  if(test)
    TEST((*test)(i, j) == 26);

  return ERR_SUCCESS;
}

int TestHarness::do_embedding2(void* assembly)
{
  constexpr int i = 4;
  constexpr int j = 2;

  int (*test)(int, int) = (int (*)(int, int))(*_exports.LoadFunction)(assembly, 0, "test");
  TEST(test != nullptr);

  if(test)
    TEST((*test)(i, j) == 26);

  return ERR_SUCCESS;
}

void TestHarness::test_embedding()
{
  const char* embed = TEST_EMBEDDING;
  size_t embedsz    = 0;
  const char* sys   = "env";
  auto lambda       = [&](Environment* env) -> int {
    int err = (*_exports.AddWhitelist)(env, "env", "my_factorial");
    TEST(!err);

    err = (*_exports.AddEmbedding)(env, 0, embed, embedsz, sys);
    TEST(!err);

    return err;
  };

  TEST(CompileWASM("../scripts/embedded.wat", &TestHarness::do_embedding, "", lambda) == ERR_SUCCESS);
  sys = "env2";
  TEST(CompileWASM("../scripts/embedded.wat", &TestHarness::do_embedding, "", lambda) != ERR_SUCCESS);
  sys = "";
  TEST(CompileWASM("../scripts/embedded.wat", &TestHarness::do_embedding, "", lambda) != ERR_SUCCESS);

  sys = 0;
  TEST(CompileWASM("../scripts/embedded.wat", &TestHarness::do_embedding, "env", lambda) == ERR_SUCCESS);

  auto embedfile = utility::LoadFile(TEST_EMBEDDING, embedsz);
  embed          = (const char*)embedfile.get();
  TEST(CompileWASM("../scripts/embedded.wat", &TestHarness::do_embedding, "env", lambda) == ERR_SUCCESS);

//#ifdef IN_PLATFORM_WIN32
  // Here, we demonstrate loading a webassembly module that depends on another webassembly module.
  // First, we compile the module we depend on, which is "embedded". Because our libaries aren't
  // webassembly aware, we pass in "" as the name, forcing them to use C linkage on exported functions.
  TEST(CompileWASM("../scripts/embedded.wat", &TestHarness::do_embedding2, "env", lambda, "") == ERR_SUCCESS);

  // Now we add the module that depends on "embedded"
  auto env = (*_exports.CreateEnvironment)(1, 0, 0);
  TEST(env);
  if(!env)
    return;

  env->flags    = ENV_ENABLE_WAT | ENV_LIBRARY;
  env->optimize = ENV_OPTIMIZE_O3;
  env->features = ENV_FEATURE_ALL;
  env->loghook  = &TestHarness::Log;
  env->loglevel = _loglevel;
  env->system   = "env"; // Make sure we set this to env

  #ifdef IN_DEBUG
  env->flags |= ENV_DEBUG;
  env->optimize = ENV_OPTIMIZE_O0;
  #endif

  path lib = _out;
#ifdef IN_PLATFORM_WIN32
  lib.replace_extension(".lib");
#endif
  auto libstr = lib.u8string();

  int err = (*_exports.AddEmbedding)(env, 0, (void*)INNATIVE_DEFAULT_ENVIRONMENT, 0, 0);
  TEST(!err);

  // When we compiled "embedded", we got a .lib file in addition to the .dll, which we add to the environment.
  // On linux, we simply link the .so file that we compiled directly.
  if(err >= 0)
    err = (*_exports.AddEmbedding)(env, 0, libstr.c_str(), 0, 0);
  TEST(!err);

  // Then, because we forced embedded to export everything as "C" functions, we have to whitelist it.
  if(err >= 0)
    err = (*_exports.AddWhitelist)(env, "env", "test");
  TEST(!err);

  path file = "../scripts/depend.wasm";
  if(err >= 0)
    (*_exports.AddModule)(env, file.u8string().c_str(), 0, file.stem().u8string().c_str(), &err);
  TEST(!err);

  if(err >= 0)
    err = (*_exports.FinalizeEnvironment)(env);
  TEST(!err);

  path base = _folder / file.stem();
  _out      = base;
  _out.replace_extension(IN_LIBRARY_EXTENSION);

  // And we compile, which results in depend.dll/depend.so which depends on embedded.dll/embedded.so, exactly what we wanted.
  if(err >= 0)
    err = (*_exports.Compile)(env, _out.u8string().c_str());
  (*_exports.DestroyEnvironment)(env);

  TEST(!err);
  if(err >= 0)
  {
    _garbage.push_back(_out);
#ifdef IN_PLATFORM_WIN32
    base.replace_extension(".lib");
    _garbage.push_back(base);
#endif
  }
  utility::SetWorkingDir(_folder);
  void* m = LoadAssembly(_out);
  TEST(m != nullptr);
  if(m)
  {
    int (*test)(int, int, int) = (int (*)(int, int, int))(*_exports.LoadFunction)(m, "depend", "call_test");
    TEST(test != nullptr);

    if(test)
      TEST((*test)(4, 1, 2) == 30);
    (*_exports.FreeAssembly)(m);
  }
  innative_set_work_dir_to_bin(_arg0);
}
