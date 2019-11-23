// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "test.h"
#include "innative/export.h"
#include <memory>
#include <atomic>
#include <thread>

void TestHarness::test_manual()
{
  Environment* env = (*_exports.CreateEnvironment)(1, 0, 0);
  env->flags |= ENV_LIBRARY;
  env->system = "env";
#ifdef IN_DEBUG
  env->optimize = ENV_OPTIMIZE_O0;
  env->flags |= ENV_DEBUG;
#endif
  path dll_path = "manual" IN_LIBRARY_EXTENSION;

  // We can safely manipulate modules ourselves, but anything involving a pointer usually needs to use inNative's special
  // handling functions, which ensure the resulting module is in a valid state.
  Module m = { INNATIVE_WASM_MAGIC_COOKIE, INNATIVE_WASM_MAGIC_VERSION };
  m.filepath = "manual.wasm.fake"; // This must be set if we want to emit debug information
  int err  = (*_exports.SetIdentifier)(env, &m.name, "manual");
  TEST(!err);

  err = (*_exports.InsertModuleSection)(env, &m, WASM_MODULE_TYPE, 0);
  TEST(!err);
  m.type.functions[0].form = TE_func; // Set our function form

  // Add our function return type (we set the parameters later)
  err = (*_exports.InsertModuleReturn)(env, &m.type.functions[0], 0, TE_i32);
  TEST(!err);

  err = (*_exports.InsertModuleSection)(env, &m, WASM_MODULE_FUNCTION, 0);
  TEST(!err);
  m.function.funcdecl[0] = 0; // Refer to our 0th type for this function

  err = (*_exports.InsertModuleSection)(env, &m, WASM_MODULE_CODE, 0);
  TEST(!err);
  Instruction get_a = { OP_local_get, { 0 } };
  Instruction get_b = { OP_local_get, { 1 } };
  Instruction add = { OP_i32_add };
  Instruction end = { OP_end };

  err = (*_exports.InsertModuleInstruction)(env, &m.code.funcbody[0], 0, &get_a);
  TEST(!err);
  err = (*_exports.InsertModuleInstruction)(env, &m.code.funcbody[0], 1, &get_b);
  TEST(!err);
  err = (*_exports.InsertModuleInstruction)(env, &m.code.funcbody[0], 2, &add);
  TEST(!err);
  err = (*_exports.InsertModuleInstruction)(env, &m.code.funcbody[0], 3, &end);
  TEST(!err);

  // Insert parameters once we have the corresponding function body
  DebugInfo info_a = { 1, 1 };
  DebugInfo info_b = { 1, 1 };

  err = (*_exports.SetIdentifier)(env, &info_a.name, "a");
  TEST(!err);
  err = (*_exports.SetIdentifier)(env, &info_b.name, "b");
  TEST(!err);
  err = (*_exports.InsertModuleParam)(env, &m.type.functions[0], &m.code.funcbody[0], 0, TE_i32, &info_a);
  TEST(!err);
  err = (*_exports.InsertModuleParam)(env, &m.type.functions[0], &m.code.funcbody[0], 1, TE_i32, &info_b);
  TEST(!err);

  err = (*_exports.InsertModuleSection)(env, &m, WASM_MODULE_EXPORT, 0);
  TEST(!err);

  err = (*_exports.SetIdentifier)(env, &m.exportsection.exports[0].name, "add");
  TEST(!err);
  m.exportsection.exports[0].kind  = WASM_KIND_FUNCTION;
  m.exportsection.exports[0].index = 0;

  err = (*_exports.AddModuleObject)(env, &m);
  TEST(!err);
  err = (*_exports.AddEmbedding)(env, 0, (void*)INNATIVE_DEFAULT_ENVIRONMENT, 0);
  TEST(!err);

  TEST(!err);
  err = (*_exports.FinalizeEnvironment)(env);
  TEST(!err);

  err = (*_exports.Compile)(env, dll_path.u8string().c_str());
  TEST(!err);

  (*_exports.DestroyEnvironment)(env);

  void* assembly = (*_exports.LoadAssembly)(dll_path.u8string().c_str());
  TEST(assembly);
  if(assembly)
  {
    auto func = (varsint32(*)(varsint32, varsint32))(*_exports.LoadFunction(assembly, "manual", "add"));
    TEST(func);

    if(func)
      TEST(func(2, 2) == 4);

    (*_exports.FreeAssembly)(assembly);
  }

  remove(dll_path);
}