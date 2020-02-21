// Copyright (c)2020 Black Sphere Studios
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
  path dll_path = _folder / "manual" IN_LIBRARY_EXTENSION;

  (*_exports.AddWhitelist)(env, "", "_innative_funcptr");

  // We can safely manipulate modules ourselves, but anything involving a pointer usually needs to use inNative's special
  // handling functions, which ensure the resulting module is in a valid state.
  Module m   = { INNATIVE_WASM_MAGIC_COOKIE, INNATIVE_WASM_MAGIC_VERSION };
  m.filepath = "manual.wasm.fake"; // This must be set if we want to emit debug information
  int err    = (*_exports.SetIdentifier)(env, &m.name, "manual");
  TEST(!err);
  err = (*_exports.InsertModuleSection)(env, &m, WASM_MODULE_IMPORT_FUNCTION, 0);
  err = (*_exports.SetIdentifier)(env, &m.importsection.imports[0].export_name, "_innative_funcptr");
  m.importsection.imports[0].kind                 = WASM_KIND_FUNCTION;
  m.importsection.imports[0].func_desc.type_index = 1;

  err = (*_exports.InsertModuleSection)(env, &m, WASM_MODULE_MEMORY, 0);
  TEST(!err);
  m.memory.memories[0].limits.minimum = 1;

  err = (*_exports.InsertModuleSection)(env, &m, WASM_MODULE_TABLE, 0);
  TEST(!err);
  m.table.tables[0].element_type      = TE_funcref;
  m.table.tables[0].resizable.minimum = 1;

  err = (*_exports.InsertModuleSection)(env, &m, WASM_MODULE_ELEMENT, 0);
  TEST(!err);
  m.element.elements[0].n_elements    = 1;
  varuint32 elems[]                   = { 1 };
  m.element.elements[0].elements      = elems;
  m.element.elements[0].offset.opcode = OP_i32_const;

  err = (*_exports.InsertModuleSection)(env, &m, WASM_MODULE_GLOBAL, 0);
  TEST(!err);
  m.global.globals[0].desc.type                     = TE_i64;
  m.global.globals[0].init.opcode                   = OP_i64_const;
  m.global.globals[0].init.immediates[0]._varuint64 = 15;

  err = (*_exports.InsertModuleSection)(env, &m, WASM_MODULE_GLOBAL, 1);
  TEST(!err);
  m.global.globals[1].desc.type                   = TE_f32;
  m.global.globals[1].init.opcode                 = OP_f32_const;
  m.global.globals[1].init.immediates[0]._float32 = 3.6f;

  err = (*_exports.InsertModuleSection)(env, &m, WASM_MODULE_TYPE, 0);
  TEST(!err);
  m.type.functypes[0].form = TE_func; // Set our function form

  // Add our function return type (we set the parameters later)
  err = (*_exports.InsertModuleReturn)(env, &m.type.functypes[0], 0, TE_i32);
  TEST(!err);

  err = (*_exports.InsertModuleSection)(env, &m, WASM_MODULE_TYPE, 1);
  TEST(!err);
  m.type.functypes[1].form = TE_func; // Set our function form

  err = (*_exports.InsertModuleReturn)(env, &m.type.functypes[1], 0, TE_i64);
  TEST(!err);
  err = (*_exports.InsertModuleParam)(env, &m.type.functypes[1], 0, 0, TE_i32, 0);
  TEST(!err);

  err = (*_exports.InsertModuleSection)(env, &m, WASM_MODULE_FUNCTION, 0);
  TEST(!err);
  m.function.funcdecl[0].type_index = 0; // Refer to our 0th type for this function

  err = (*_exports.InsertModuleSection)(env, &m, WASM_MODULE_FUNCTION, 1);
  TEST(!err);
  m.function.funcdecl[1].type_index = 1; // This is a wrapper around the imported intrinsic

  err = (*_exports.InsertModuleSection)(env, &m, WASM_MODULE_CODE, 0);
  TEST(!err);
  Instruction get_a = { OP_local_get, { 0 } };
  Instruction get_b = { OP_local_get, { 1 } };
  Instruction add   = { OP_i32_add };
  Instruction end   = { OP_end };

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
  err = (*_exports.InsertModuleParam)(env, &m.type.functypes[0], &m.function.funcdecl[0], 0, TE_i32, &info_a);
  TEST(!err);
  err = (*_exports.InsertModuleParam)(env, &m.type.functypes[0], &m.function.funcdecl[0], 1, TE_i32, &info_b);
  TEST(!err);

  err = (*_exports.InsertModuleSection)(env, &m, WASM_MODULE_CODE, 1);
  TEST(!err);
  Instruction call_0 = { OP_call, { 0 } };

  err = (*_exports.InsertModuleInstruction)(env, &m.code.funcbody[1], 0, &get_a);
  TEST(!err);
  err = (*_exports.InsertModuleInstruction)(env, &m.code.funcbody[1], 1, &call_0);
  TEST(!err);
  err = (*_exports.InsertModuleInstruction)(env, &m.code.funcbody[1], 2, &end);
  TEST(!err);

  err = (*_exports.InsertModuleSection)(env, &m, WASM_MODULE_EXPORT, 0);
  TEST(!err);
  err = (*_exports.SetIdentifier)(env, &m.exportsection.exports[0].name, "add");
  TEST(!err);
  m.exportsection.exports[0].kind  = WASM_KIND_FUNCTION;
  m.exportsection.exports[0].index = 1;

  err = (*_exports.InsertModuleSection)(env, &m, WASM_MODULE_EXPORT, 0);
  TEST(!err);
  err = (*_exports.SetIdentifier)(env, &m.exportsection.exports[0].name, "funcptr");
  TEST(!err);
  m.exportsection.exports[0].kind  = WASM_KIND_FUNCTION;
  m.exportsection.exports[0].index = 2;

  err = (*_exports.InsertModuleSection)(env, &m, WASM_MODULE_EXPORT, 0);
  TEST(!err);
  err = (*_exports.SetIdentifier)(env, &m.exportsection.exports[0].name, "table_test");
  TEST(!err);
  m.exportsection.exports[0].kind  = WASM_KIND_TABLE;
  m.exportsection.exports[0].index = 0;

  err = (*_exports.AddModuleObject)(env, &m);
  TEST(!err);
  err = (*_exports.AddEmbedding)(env, 0, (void*)INNATIVE_DEFAULT_ENVIRONMENT, 0, 0);
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

    auto funcptr = (decltype(func) (*)(varsint32))(*_exports.LoadFunction(assembly, "manual", "funcptr"));
    TEST(funcptr);
    if(funcptr)
      TEST(funcptr(1) == func);

    INModuleMetadata* metadata = (*_exports.GetModuleMetadata)(assembly, 0);
    TEST(metadata);
    if(metadata)
    {
      TEST(metadata->version == 1);
      TEST(!STRICMP(metadata->name, "manual"));
      TEST(metadata->n_functions > 1);
      if(metadata->n_functions > 1 && metadata->functions)
        TEST(metadata->functions[1] == (IN_Entrypoint)func);
    }

    auto mem = (*_exports.LoadMemoryIndex)(assembly, 0, 0);
    TEST(mem);
    if(mem)
    {
      TEST(mem->memory.bytes);
      if(mem->memory.bytes)
        TEST(mem->memory.size == (1 << 16));
    }

    auto tablefunc = (varsint32(*)(varsint32, varsint32))(*_exports.LoadTableIndex)(assembly, 0, 0, 0);
    TEST(tablefunc);
    if(tablefunc)
      TEST(tablefunc !=
           nullptr); // Note: This is the internal fastcc call, not the __cdecl exported wrapper, so it is not safe to call

    auto tablefunc2 = (varsint32(*)(varsint32, varsint32))(*_exports.LoadTable)(assembly, "manual", "table_test", 0);
    TEST(tablefunc2);
    if(tablefunc2)
      TEST(tablefunc2 == tablefunc);

    auto glob1 = (*_exports.LoadGlobalIndex)(assembly, 0, 0);
    TEST(glob1);
    if(glob1)
      TEST(glob1->i64 == 15);

    auto glob2 = (*_exports.LoadGlobalIndex)(assembly, 0, 1);
    TEST(glob2);
    if(glob2)
      TEST(glob2->f32 == 3.6f);

    TEST(!(*_exports.LoadGlobalIndex)(assembly, 0, 2));
    TEST(!(*_exports.LoadMemoryIndex)(assembly, 0, 1));
    TEST(!(*_exports.LoadTableIndex)(assembly, 0, 1, 0));
    TEST(!(*_exports.LoadGlobalIndex)(assembly, 1, 0));
    TEST(!(*_exports.LoadMemoryIndex)(assembly, 1, 0));
    TEST(!(*_exports.LoadTableIndex)(assembly, 1, 0, 0));
    TEST(!(*_exports.GetModuleMetadata)(assembly, 1));
    TEST(!(*_exports.LoadTable)(assembly, "manual", "table_test", 1));

    (*_exports.FreeAssembly)(assembly);
  }

  remove(dll_path);
}