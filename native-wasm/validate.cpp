// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in native-wasm.h

#include "validate.h"
#include "util.h"
#include "stack.h"
#include <stdio.h>
#include <stdarg.h>
#include <atomic>

static const char trailingBytesForUTF8[256] = {
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5
};

// UTF8 validator based off the official unicode C validator source
bool ValidateIdentifier(ByteArray& b)
{
  size_t i = 0;
  char a;
  char c;
  const char *srcptr;
  while(i < b.n_bytes)
  {
    c = b.bytes[i];
    int length = trailingBytesForUTF8[c] + 1;
    srcptr = reinterpret_cast<char*>(b.bytes) + i + length;
    switch(length)
    {
    default: return false;
      /* Everything else falls through when "true"... */
    case 4: if((a = (*--srcptr)) < 0x80 || a > 0xBF) return false;
    case 3: if((a = (*--srcptr)) < 0x80 || a > 0xBF) return false;
    case 2: if((a = (*--srcptr)) < 0x80 || a > 0xBF) return false;

      switch(c)
      {
        /* no fall-through in this inner switch */
      case 0xE0: if(a < 0xA0) return false; break;
      case 0xED: if(a > 0x9F) return false; break;
      case 0xF0: if(a < 0x90) return false; break;
      case 0xF4: if(a > 0x8F) return false; break;
      default:   if(a < 0x80) return false;
      }

    case 1: if(c >= 0x80 && c < 0xC2) return false;
    }
    if(c > 0xF4) return false;
    i += length;
  }

  return i == b.n_bytes; // If these aren't exactly equal there was an expected length mismatch
}

bool ValidateValueType(varsint7 type)
{
  switch(type)
  {
  case TE_i32:
  case TE_i64:
  case TE_f32:
  case TE_f64:
    return true;
  }
  return false;
}

void AppendError(Environment& env, Module* m, int code, const char* fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  int len = vsnprintf(0, 0, fmt, args);
  ValidationError* err = reinterpret_cast<ValidationError*>(malloc(sizeof(ValidationError) + len + 1));
  err->error = reinterpret_cast<char*>(err + 1);
  vsnprintf(err->error, len, fmt, args);
  va_end(args);

  err->error[len] = 0;
  err->code = code;
  err->m = m;

  do
  {
    err->next = ((std::atomic<ValidationError*>&)env.errors).load(std::memory_order_relaxed);
  } while(!((std::atomic<ValidationError*>&)env.errors).compare_exchange_weak(err->next, err, std::memory_order_release, std::memory_order_relaxed));
}

void ValidateFunctionSig(FunctionSig& sig, Environment& env, Module* m)
{
  if(sig.form == TE_func)
  {
    if(sig.n_returns > 1)
      AppendError(env, m, ERR_MULTIPLE_RETURN_VALUES, "Return count of %u encountered: only 0 or 1 allowed.", sig.form);
  }
  else
    AppendError(env, m, ERR_UNKNOWN_SIGNATURE_TYPE, "Illegal function type %hhi encountered: only -0x20 allowed", sig.form);
}

void ValidateImport(Import& imp, Environment& env, Module* m)
{
  if(!ValidateIdentifier(imp.module_name))
    AppendError(env, m, ERR_INVALID_IDENTIFIER, "Identifier not valid UTF8: %s", imp.module_name.bytes);
  if(!ValidateIdentifier(imp.export_name))
    AppendError(env, m, ERR_INVALID_IDENTIFIER, "Identifier not valid UTF8: %s", imp.export_name.bytes);

  khint_t iter = kh_get_modules(env.modulemap, (char*)imp.module_name.bytes);
  if(iter == kh_end(env.modulemap))
    return AppendError(env, m, ERR_UNKNOWN_MODULE, "%s module not found", imp.module_name.bytes);

  varuint32 i = kh_value(env.modulemap, iter);
  if(i >= env.n_modules)
    return AppendError(env, m, ERR_UNKNOWN_MODULE, "%s module index (%u) not in range (%u)", imp.module_name.bytes, i, env.n_modules);

  iter = kh_get_exports(env.modules[i].exports, (char*)imp.export_name.bytes);
  if(iter == kh_end(env.modules[i].exports))
    return AppendError(env, m, ERR_UNKNOWN_EXPORT, "%s export not found", imp.export_name.bytes);

  varuint32 j = kh_value(env.modules[i].exports, iter);
  if(j >= env.modules[i].exportsection.n_exports)
    return AppendError(env, m, ERR_UNKNOWN_EXPORT, "%s export index (%u) not in range (%u)", imp.module_name.bytes, j, env.modules[i].exportsection.n_exports);

  Export& exp = env.modules[i].exportsection.exports[j];
  if(exp.kind != imp.kind)
    return AppendError(env, m, ERR_IMPORT_EXPORT_MISMATCH, "export kind (%u) does not match import kind (%u)", exp.kind, imp.kind);

  if(exp.name.n_bytes != imp.export_name.n_bytes || memcmp(exp.name.bytes, imp.export_name.bytes, exp.name.n_bytes) != 0)
    AppendError(env, m, ERR_IMPORT_EXPORT_MISMATCH, "export name (%s) does not match import name (%s)", exp.name.bytes, imp.export_name.bytes);

  switch(imp.kind)
  {
  case KIND_FUNCTION:
    if(imp.sig_index >= m->type.n_functions)
      AppendError(env, m, ERR_INVALID_TYPE_INDEX, "Invalid imported function type index %u", imp.sig_index);
    break;
  case KIND_TABLE:
  {
    TableDesc* table = ModuleTable(env.modules[i], exp.index);
    if(!table)
      AppendError(env, m, ERR_INVALID_TABLE_INDEX, "Invalid exported table index %u", exp.index);
    else
    {
      if(imp.table_desc.resizable.minimum > table->resizable.minimum)
        AppendError(env, m, ERR_INVALID_IMPORT_MEMORY_MINIMUM, "Imported table minimum (%u) greater than exported table minimum (%u).", imp.table_desc.resizable.minimum, table->resizable.minimum);
      if(table->resizable.flags & 1)
      {
        if(!(imp.table_desc.resizable.flags & 1))
          AppendError(env, m, ERR_INVALID_IMPORT_MEMORY_MAXIMUM, "Imported table doesn't have a maximum, but exported table does.");
        else if(imp.table_desc.resizable.maximum < table->resizable.maximum)
          AppendError(env, m, ERR_INVALID_IMPORT_MEMORY_MAXIMUM, "Imported table maximum (%u) less than exported table maximum (%u).", imp.table_desc.resizable.maximum, table->resizable.maximum);
      }
    }
    break;
  }
  case KIND_MEMORY:
  {
    MemoryDesc* mem = ModuleMemory(env.modules[i], exp.index);
    if(!mem)
      AppendError(env, m, ERR_INVALID_MEMORY_INDEX, "Invalid exported memory index %u", exp.index);
    else
    {
      if(imp.mem_desc.limits.minimum > mem->limits.minimum)
        AppendError(env, m, ERR_INVALID_IMPORT_MEMORY_MINIMUM, "Imported memory minimum (%u) greater than exported memory minimum (%u).", imp.mem_desc.limits.minimum, mem->limits.minimum);
      if(mem->limits.flags & 1)
      {
        if(!(imp.mem_desc.limits.flags & 1))
          AppendError(env, m, ERR_INVALID_IMPORT_MEMORY_MAXIMUM, "Imported memory doesn't have a maximum, but exported memory does.");
        else if(imp.mem_desc.limits.maximum < mem->limits.maximum)
          AppendError(env, m, ERR_INVALID_IMPORT_MEMORY_MAXIMUM, "Imported memory maximum (%u) less than exported memory maximum (%u).", imp.mem_desc.limits.maximum, mem->limits.maximum);
      }
    }
    break;
  }
  case KIND_GLOBAL:
  {
    GlobalDesc* global = ModuleGlobal(env.modules[i], exp.index);
    if(!global)
      AppendError(env, m, ERR_INVALID_GLOBAL_INDEX, "Invalid exported global index %u", exp.index);
    else if(global->mutability) // Imported globals must be immutable for right now
      AppendError(env, m, ERR_MUTABLE_GLOBAL, "Exported global %u cannot be mutable.", exp.index);
    break;
  }
  default:
    AppendError(env, m, ERR_FATAL_UNKNOWN_KIND, "unknown export kind: %hhu", imp.kind);
  }
}

void ValidateFunction(varuint32& decl, Environment& env, Module* m)
{
  if(decl >= m->type.n_functions)
    AppendError(env, m, ERR_INVALID_TYPE_INDEX, "Invalid function declaration type index: %u", decl);
}

void ValidateLimits(ResizableLimits& limits, Environment& env, Module* m)
{
  if(limits.maximum < limits.minimum)
    AppendError(env, m, ERR_INVALID_LIMITS, "Limits maximum (%u) cannot be smaller than minimum (%u)", limits.maximum, limits.minimum);
}

void ValidateTable(TableDesc& table, Environment& env, Module* m)
{
  if(table.element_type != TE_anyfunc)
    AppendError(env, m, ERR_INVALID_TABLE_ELEMENT_TYPE, "Table element type is %hhi: only anyfunc allowed.", table.element_type);
  ValidateLimits(table.resizable, env, m);
}

void ValidateMemory(MemoryDesc& mem, Environment& env, Module* m)
{
  ValidateLimits(mem.limits, env, m);
}

void ValidateInstruction(Instruction& ins, Environment& env, Module* m)
{
  // TODO
}

varsint7 ValidateInitializer(Instruction& ins, Environment& env, Module* m)
{
  ValidateInstruction(ins, env, m);

  switch(ins.opcode)
  {
  case OP_i32_const: return TE_i32;
  case OP_i64_const: return TE_i64;
  case OP_f32_const: return TE_f32;
  case OP_f64_const: return TE_f64;
  case OP_get_global:
    if(ins.immediates[0]._varuint32 < m->importsection.globals)
      return m->importsection.imports[ins.immediates[0]._varuint32].global_desc.type;
    else
      AppendError(env, m, ERR_INVALID_INITIALIZER, "A get_global initializer must be an import.", ins.opcode);
    return TE_i32;
  }

  AppendError(env, m, ERR_INVALID_INITIALIZER, "An initializer must be a get_global or const instruction, not %hhu", ins.opcode);
  return TE_i32;
}

void ValidateGlobal(GlobalDecl& decl, Environment& env, Module* m)
{
  varsint7 type = ValidateInitializer(decl.init, env, m);
  if(type != decl.desc.type)
    AppendError(env, m, ERR_INVALID_GLOBAL_TYPE, "The global initializer has type %hhi, must be the same as the description type %hhi.", type, decl.desc.type);
}

void ValidateExport(Export& e, Environment& env, Module* m)
{
  ValidateIdentifier(e.name);

  switch(e.kind)
  {
  case KIND_FUNCTION:
    if(!ModuleFunction(*m, e.index))
      AppendError(env, m, ERR_INVALID_FUNCTION_INDEX, "Invalid function index %u", e.index);
    break;
  case KIND_TABLE:
    if(!ModuleTable(*m, e.index))
      AppendError(env, m, ERR_INVALID_TABLE_INDEX, "Invalid table index %u", e.index);
    break;
  case KIND_MEMORY:
    if(!ModuleMemory(*m, e.index))
      AppendError(env, m, ERR_INVALID_MEMORY_INDEX, "Invalid memory index %u", e.index);
    break;
  case KIND_GLOBAL:
  {
    GlobalDesc* g = ModuleGlobal(*m, e.index);
    if(!g)
      AppendError(env, m, ERR_INVALID_GLOBAL_INDEX, "Invalid global index %u", e.index);
    else if(g->mutability)
      AppendError(env, m, ERR_MUTABLE_GLOBAL, "Exported global %s index %u should not be mutable.", e.name.bytes, e.index);
    break;
  }
  default:
    AppendError(env, m, ERR_FATAL_UNKNOWN_KIND, "The %s export has invalid kind %hhu", e.name.bytes, e.kind);
    break;
  }
}

varsint32 EvalInitializerI32(Instruction& ins, Environment& env, Module* m)
{
  switch(ins.opcode)
  {
  case OP_i32_const:
    return ins.immediates[0]._varsint32;
  case OP_get_global:
  {
    GlobalDecl* global = 0;
    size_t i = ins.immediates[0]._varsint32 + m->importsection.memory; // Shift index to globals section
    if(i < m->importsection.globals)
    {
      std::pair<Module*, Export*> p = ResolveExport(env, m->importsection.imports[i]);
      if(!p.second || p.second->kind != KIND_GLOBAL || p.second->index < p.first->importsection.globals)
        AppendError(env, m, ERR_INVALID_GLOBAL_INDEX, "Invalid global import %u", ins.immediates[0]._varsint32);
      else
        return EvalInitializerI32(p.first->global.globals[p.second->index - p.first->importsection.globals].init, env, p.first);
      break;
    }
    i -= m->importsection.globals;
    if(i < m->global.n_globals)
      global = &m->global.globals[i];

    if(!global)
      AppendError(env, m, ERR_INVALID_GLOBAL_INDEX, "Invalid global index %u", ins.immediates[0]._varsint32);
    else
      return EvalInitializerI32(global->init, env, nullptr);
    break;
  }
  default:
    AppendError(env, m, ERR_INVALID_INITIALIZER, "Expected i32 type but got %hhu", ins.opcode);
  }

  return 0;
}

void ValidateTableOffset(TableInit& init, Environment& env, Module* m)
{
  varsint7 type = ValidateInitializer(init.offset, env, m);
  if(type != TE_i32)
    AppendError(env, m, ERR_INVALID_TABLE_TYPE, "Expected table offset instruction type of i32, got %hhi instead.", type);

  TableDesc* table = ModuleTable(*m, init.index);
  if(!table)
    AppendError(env, m, ERR_INVALID_TABLE_INDEX, "Invalid table index %u", init.index);
  else if(table->element_type == TE_anyfunc)
  {
    varsint32 offset = EvalInitializerI32(init.offset, env, m);
    if(offset + init.n_elems > table->resizable.minimum)
      AppendError(env, m, ERR_INVALID_TABLE_OFFSET, "Offset (%i) plus element count (%u) exceeds minimum table length (%u)", offset, init.n_elems, table->resizable.minimum);

    for(varuint32 i = 0; i < init.n_elems; ++i)
      if(!ModuleFunction(*m, init.elems[i]))
        AppendError(env, m, ERR_INVALID_FUNCTION_INDEX, "Invalid element initializer %u function index: %u", i, init.elems[i]);
  }
  else
    AppendError(env, m, ERR_INVALID_TABLE_ELEMENT_TYPE, "Invalid table element type %hhi", table->element_type);
}

void ValidateFunctionBody(FunctionBody& body, Environment& env, Module* m)
{
  Instruction* cur = body.body;
  Stack<varuint7> control; // control-flow stack that must be closed by end instructions
  control.Push(OP_block); // Push fake block instruction to represent the function block for analysis purposes

  if(!body.n_body)
    return AppendError(env, m, ERR_INVALID_FUNCTION_BODY, "Cannot have an empty function body!");

  for(varuint32 i = 0; i < body.n_body; ++i)
  {
    ValidateInstruction(cur[i], env, m);

    // TODO: verify value stack along all reachable control paths

    switch(cur[i].opcode)
    {
    case OP_block:
    case OP_loop:
    case OP_if:
      control.Push(cur[i].opcode);
      break;
    case OP_end:
      if(!control.Size())
        AppendError(env, m, ERR_INVALID_FUNCTION_BODY, "Mismatched end instruction at index %u!", i);
      else
        control.Pop();
      break;
    case OP_else:
      if(!control.Size())
        AppendError(env, m, ERR_INVALID_FUNCTION_BODY, "Mismatched else instruction at index %u!", i);
      else
      {
        varuint7 op = control.Pop();
        if(op != OP_if)
          AppendError(env, m, ERR_INVALID_FUNCTION_BODY, "Expected else instruction to terminate if block, but found %hhi instead.", op);
        control.Push(OP_else); // Push a new else block that must be terminated by an end instruction
      }
    }
  }

  if(control.Size() > 0)
    AppendError(env, m, ERR_INVALID_FUNCTION_BODY, "Control stack not fully terminated, off by %zu", control.Size());

  if(cur[body.n_body - 1].opcode != OP_end)
    AppendError(env, m, ERR_INVALID_FUNCTION_BODY, "Expected end instruction to terminate function body, got %hhu instead.", cur[body.n_body - 1].opcode);
}

void ValidateDataOffset(DataInit& init, Environment& env, Module* m)
{
  varsint7 type = ValidateInitializer(init.offset, env, m);
  if(type != TE_i32)
    AppendError(env, m, ERR_INVALID_MEMORY_TYPE, "Expected memory offset instruction type of i32, got %hhi instead.", type);

  MemoryDesc* memory = ModuleMemory(*m, init.index);
  if(!memory)
    AppendError(env, m, ERR_INVALID_MEMORY_INDEX, "Invalid memory index %u", init.index);
  else
  {
    varsint32 offset = EvalInitializerI32(init.offset, env, m);
    if(offset + init.data.n_bytes > memory->limits.minimum)
      AppendError(env, m, ERR_INVALID_MEMORY_OFFSET, "Offset (%i) plus element count (%u) exceeds minimum memory length (%u)", offset, init.data.n_bytes, memory->limits.minimum);
  }
}

template<class T, void(*VALIDATE)(T&, Environment&, Module*)>
void ValidateSection(T* a, varuint32 n, Environment& env, Module* m)
{
  for(varuint32 i = 0; i < n; ++i)
    VALIDATE(a[i], env, m);
}

// Performs all post-load validation that couldn't be done during parsing
void ValidateEnvironment(Environment& env)
{
  for(size_t i = 0; i < env.n_modules; ++i)
  {
    if(env.modules[i].knownsections&(1 << SECTION_TYPE))
      ValidateSection<FunctionSig, &ValidateFunctionSig>(env.modules[i].type.functions, env.modules[i].type.n_functions, env, env.modules + i);

    if(env.modules[i].knownsections&(1 << SECTION_IMPORT))
      ValidateSection<Import, &ValidateImport>(env.modules[i].importsection.imports, env.modules[i].importsection.n_import, env, env.modules + i);

    if(env.modules[i].knownsections&(1 << SECTION_FUNCTION))
    {
      ValidateSection<varuint32, &ValidateFunction>(env.modules[i].function.funcdecl, env.modules[i].function.n_funcdecl, env, env.modules + i);

      if(env.modules[i].function.n_funcdecl != env.modules[i].code.n_funcbody)
        AppendError(env, env.modules + i, ERR_FUNCTION_BODY_MISMATCH, "The number of function declarations (%u) does not equal the number of function bodies (%u)", env.modules[i].function.n_funcdecl, env.modules[i].code.n_funcbody);
    }

    if(env.modules[i].knownsections&(1 << SECTION_TABLE))
      ValidateSection<TableDesc, &ValidateTable>(env.modules[i].table.tables, env.modules[i].table.n_tables, env, env.modules + i);

    if(env.modules[i].knownsections&(1 << SECTION_MEMORY))
      ValidateSection<MemoryDesc, &ValidateMemory>(env.modules[i].memory.memory, env.modules[i].memory.n_memory, env, env.modules + i);

    if(env.modules[i].knownsections&(1 << SECTION_GLOBAL))
      ValidateSection<GlobalDecl, &ValidateGlobal>(env.modules[i].global.globals, env.modules[i].global.n_globals, env, env.modules + i);

    if(env.modules[i].knownsections&(1 << SECTION_EXPORT))
      ValidateSection<Export, &ValidateExport>(env.modules[i].exportsection.exports, env.modules[i].exportsection.n_exports, env, env.modules + i);

    if(env.modules[i].knownsections&(1 << SECTION_START))
    {
      FunctionSig* f = ModuleFunction(env.modules[i], env.modules[i].start);
      if(f)
      {
        if(env.modules[i].start < env.modules[i].importsection.functions)
          AppendError(env, env.modules + i, ERR_INVALID_START_FUNCTION, "The start function (%hhu) cannot be an imported function", env.modules[i].start);
        if(f->n_params > 0 || f->n_returns > 0)
          AppendError(env, env.modules + i, ERR_INVALID_START_FUNCTION, "Starting function must have no parameters and no return value, instead it has %u parameters and %u return values.", f->n_params, f->n_returns);
      }
      else
        AppendError(env, env.modules + i, ERR_INVALID_FUNCTION_INDEX, "Start module function index %u does not exist.", env.modules[i].start);
    }

    if(env.modules[i].knownsections&(1 << SECTION_ELEMENT))
      ValidateSection<TableInit, &ValidateTableOffset>(env.modules[i].element.elements, env.modules[i].element.n_elements, env, env.modules + i);

    if(env.modules[i].knownsections&(1 << SECTION_CODE))
      ValidateSection<FunctionBody, &ValidateFunctionBody>(env.modules[i].code.funcbody, env.modules[i].code.n_funcbody, env, env.modules + i);

    if(env.modules[i].knownsections&(1 << SECTION_DATA))
      ValidateSection<DataInit, &ValidateDataOffset>(env.modules[i].data.data, env.modules[i].data.n_data, env, env.modules + i);
  }
}

bool ValidateSectionOrder(uint32& sections, varuint7 opcode)
{
  return (sections & ((~0) << opcode)) == 0;
}