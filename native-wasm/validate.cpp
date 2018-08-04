// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in native-wasm.h

#include "validate.h"
#include "util.h"
#include "stack.h"
#include <stdio.h>
#include <stdarg.h>
#include <atomic>

// Hashes a pair of strings seperated by a null terminator
static kh_inline khint_t __ac_X31_hash_string_pair(const char *s)
{
  khint_t h = (khint_t)*s;
  if(h) for(++s; *s; ++s) h = (h << 5) - h + (khint_t)*s;
  for(++s; *s; ++s) h = (h << 5) - h + (khint_t)*s;
  return h;
}

#define str_pair_hash_equal(a, b) (strcmp(a, b) == 0) && (strcmp(strchr(a, 0)+1, strchr(a, 0)+1) == 0)

__KHASH_IMPL(modulepair, kh_inline, kh_cstr_t, FunctionSig, 1, __ac_X31_hash_string_pair, str_pair_hash_equal);
__KHASH_IMPL(cimport, kh_inline, kh_cstr_t, char, 0, kh_str_hash_func, kh_str_hash_equal);

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

void AppendError(ValidationError*& errors, Module* m, int code, const char* fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  int len = vsnprintf(0, 0, fmt, args);
  ValidationError* err = reinterpret_cast<ValidationError*>(GreedyAlloc(sizeof(ValidationError) + len + 1));
  err->error = reinterpret_cast<char*>(err + 1);
  vsnprintf(err->error, len + 1, fmt, args);
  va_end(args);

  err->error[len] = 0;
  err->code = code;
  err->m = m;

  do
  {
    err->next = ((std::atomic<ValidationError*>&)errors).load(std::memory_order_relaxed);
  } while(!((std::atomic<ValidationError*>&)errors).compare_exchange_weak(err->next, err, std::memory_order_release, std::memory_order_relaxed));
}

void ValidateFunctionSig(FunctionSig& sig, Environment& env, Module* m)
{
  if(sig.form == TE_func)
  {
    if(sig.n_returns > 1)
      AppendError(env.errors, m, ERR_MULTIPLE_RETURN_VALUES, "Return count of %u encountered: only 0 or 1 allowed.", sig.form);
  }
  else
    AppendError(env.errors, m, ERR_UNKNOWN_SIGNATURE_TYPE, "Illegal function type %hhi encountered: only -0x20 allowed", sig.form);
}

bool MatchFunctionSig(FunctionSig& a, FunctionSig& b)
{
  if(a.form != b.form || a.n_params != b.n_params || a.n_returns != b.n_returns)
    return false;

  for(varuint32 i = 0; i < a.n_params; ++i)
    if(a.params[i] != b.params[i])
      return false;

  for(varuint32 i = 0; i < a.n_returns; ++i)
    if(a.returns[i] != b.returns[i])
      return false;

  return true;
}

void ValidateImport(Import& imp, Environment& env, Module* m)
{
  if(!ValidateIdentifier(imp.module_name))
    AppendError(env.errors, m, ERR_INVALID_IDENTIFIER, "Identifier not valid UTF8: %s", imp.module_name.bytes);
  if(!ValidateIdentifier(imp.export_name))
    AppendError(env.errors, m, ERR_INVALID_IDENTIFIER, "Identifier not valid UTF8: %s", imp.export_name.bytes);

  char* modname = (char*)imp.module_name.bytes;

  khint_t iter = kh_get_modules(env.modulemap, modname); // WASM modules do not understand !CALL convention appendings, so we use the full name no matter what
  if(iter == kh_end(env.modulemap))
  {
    if(env.whitelist)
    {
      khiter_t iter = kh_get_modulepair(env.whitelist, CanonWhitelist(imp.module_name.bytes, "").c_str()); // Check for a wildcard match first
      if(!kh_exist2(env.whitelist, iter))
      {
        khiter_t iter = kh_get_modulepair(env.whitelist, CanonWhitelist(imp.module_name.bytes, imp.export_name.bytes).c_str()); // We already canonized the whitelist imports to eliminate unnecessary !C specifiers
        if(!kh_exist2(env.whitelist, iter))
          return AppendError(env.errors, m, ERR_ILLEGAL_C_IMPORT, "%s:%s is not a whitelisted C import, nor a valid webassembly import.", imp.module_name.bytes, imp.export_name.bytes);
        if(imp.kind != KIND_FUNCTION)
          return AppendError(env.errors, m, ERR_ILLEGAL_C_IMPORT, "%s:%s is not a function. You can only import C functions at this time.", imp.module_name.bytes, imp.export_name.bytes);

        FunctionSig& sig = kh_val(env.whitelist, iter);
        if(sig.form != TE_NONE) // If we have a function signature, verify it
        {
          if(imp.func_desc.sig_index >= m->type.n_functions)
            return AppendError(env.errors, m, ERR_INVALID_TYPE_INDEX, "Invalid imported function type index %u", imp.func_desc.sig_index);
          if(!MatchFunctionSig(sig, m->type.functions[imp.func_desc.sig_index]))
            return AppendError(env.errors, m, ERR_ILLEGAL_C_IMPORT, "%s:%s does not match function signature provided by environment.", imp.module_name.bytes, imp.export_name.bytes);
          return;
        }
        else if(env.flags & ENV_STRICT) // Strict mode enforces function signatures
          return AppendError(env.errors, m, ERR_ILLEGAL_C_IMPORT, "%s:%s has no function signature - Strict mode requires a valid function signature for all whitelisted C imports.", imp.module_name.bytes, imp.export_name.bytes);
      } else if(env.flags & ENV_STRICT) // Wildcard whitelists are not allowed in strict mode becuase we must know the function type in strict mode.
        return AppendError(env.errors, m, ERR_ILLEGAL_C_IMPORT, "Wildcard imports (%s) are not allowed in strict mode! Strict mode requires a valid function signature for all whitelisted C imports.", imp.module_name.bytes);
    }

    if(env.cimports) 
    {
      // TODO: actually enforce this
      return;
      std::string name = CanonImportName(imp);
      khiter_t iter = kh_get_cimport(env.cimports, name.c_str());
      if(kh_exist2(env.cimports, iter))
        return; // This function exists and we already have verified the signature if there was a whitelist, so just return
      if(!modname || !modname[0] || modname[0] == '!') // Blank imports must have been C imports, otherwise it could have been a failed WASM module import attempt.
        return AppendError(env.errors, m, ERR_UNKNOWN_BLANK_IMPORT, "%s not found in C library imports", name.c_str());
    }

    return AppendError(env.errors, m, ERR_UNKNOWN_MODULE, "%s module not found", imp.module_name.bytes);
  }
  varuint32 i = kh_value(env.modulemap, iter);
  if(i >= env.n_modules)
    return AppendError(env.errors, m, ERR_UNKNOWN_MODULE, "%s module index (%u) not in range (%u)", imp.module_name.bytes, i, env.n_modules);

  iter = kh_get_exports(env.modules[i].exports, (char*)imp.export_name.bytes);
  if(iter == kh_end(env.modules[i].exports))
    return AppendError(env.errors, m, ERR_UNKNOWN_EXPORT, "%s export not found", imp.export_name.bytes);

  varuint32 j = kh_value(env.modules[i].exports, iter);
  if(j >= env.modules[i].exportsection.n_exports)
    return AppendError(env.errors, m, ERR_UNKNOWN_EXPORT, "%s export index (%u) not in range (%u)", imp.module_name.bytes, j, env.modules[i].exportsection.n_exports);

  Export& exp = env.modules[i].exportsection.exports[j];
  if(exp.kind != imp.kind)
    return AppendError(env.errors, m, ERR_IMPORT_EXPORT_MISMATCH, "export kind (%u) does not match import kind (%u)", exp.kind, imp.kind);

  if(exp.name.n_bytes != imp.export_name.n_bytes || memcmp(exp.name.bytes, imp.export_name.bytes, exp.name.n_bytes) != 0)
    AppendError(env.errors, m, ERR_IMPORT_EXPORT_MISMATCH, "export name (%s) does not match import name (%s)", exp.name.bytes, imp.export_name.bytes);

  switch(imp.kind)
  {
  case KIND_FUNCTION:
    if(imp.func_desc.sig_index >= m->type.n_functions)
      AppendError(env.errors, m, ERR_INVALID_TYPE_INDEX, "Invalid imported function type index %u", imp.func_desc.sig_index);
    break;
  case KIND_TABLE:
  {
    TableDesc* table = ModuleTable(env.modules[i], exp.index);
    if(!table)
      AppendError(env.errors, m, ERR_INVALID_TABLE_INDEX, "Invalid exported table index %u", exp.index);
    else
    {
      if(imp.table_desc.resizable.minimum > table->resizable.minimum)
        AppendError(env.errors, m, ERR_INVALID_IMPORT_MEMORY_MINIMUM, "Imported table minimum (%u) greater than exported table minimum (%u).", imp.table_desc.resizable.minimum, table->resizable.minimum);
      if(table->resizable.flags & 1)
      {
        if(!(imp.table_desc.resizable.flags & 1))
          AppendError(env.errors, m, ERR_INVALID_IMPORT_MEMORY_MAXIMUM, "Imported table doesn't have a maximum, but exported table does.");
        else if(imp.table_desc.resizable.maximum < table->resizable.maximum)
          AppendError(env.errors, m, ERR_INVALID_IMPORT_MEMORY_MAXIMUM, "Imported table maximum (%u) less than exported table maximum (%u).", imp.table_desc.resizable.maximum, table->resizable.maximum);
      }
    }
    break;
  }
  case KIND_MEMORY:
  {
    MemoryDesc* mem = ModuleMemory(env.modules[i], exp.index);
    if(!mem)
      AppendError(env.errors, m, ERR_INVALID_MEMORY_INDEX, "Invalid exported memory index %u", exp.index);
    else
    {
      if(imp.mem_desc.limits.minimum > mem->limits.minimum)
        AppendError(env.errors, m, ERR_INVALID_IMPORT_MEMORY_MINIMUM, "Imported memory minimum (%u) greater than exported memory minimum (%u).", imp.mem_desc.limits.minimum, mem->limits.minimum);
      if(mem->limits.flags & 1)
      {
        if(!(imp.mem_desc.limits.flags & 1))
          AppendError(env.errors, m, ERR_INVALID_IMPORT_MEMORY_MAXIMUM, "Imported memory doesn't have a maximum, but exported memory does.");
        else if(imp.mem_desc.limits.maximum < mem->limits.maximum)
          AppendError(env.errors, m, ERR_INVALID_IMPORT_MEMORY_MAXIMUM, "Imported memory maximum (%u) less than exported memory maximum (%u).", imp.mem_desc.limits.maximum, mem->limits.maximum);
      }
    }
    break;
  }
  case KIND_GLOBAL:
  {
    GlobalDesc* global = ModuleGlobal(env.modules[i], exp.index);
    if(!global)
      AppendError(env.errors, m, ERR_INVALID_GLOBAL_INDEX, "Invalid exported global index %u", exp.index);
    else if(global->mutability) // Imported globals must be immutable for right now
      AppendError(env.errors, m, ERR_MUTABLE_GLOBAL, "Exported global %u cannot be mutable.", exp.index);
    break;
  }
  default:
    AppendError(env.errors, m, ERR_FATAL_UNKNOWN_KIND, "unknown export kind: %hhu", imp.kind);
  }
}

void ValidateFunction(varuint32& decl, Environment& env, Module* m)
{
  if(decl >= m->type.n_functions)
    AppendError(env.errors, m, ERR_INVALID_TYPE_INDEX, "Invalid function declaration type index: %u", decl);
}

void ValidateLimits(ResizableLimits& limits, Environment& env, Module* m)
{
  if(limits.maximum < limits.minimum)
    AppendError(env.errors, m, ERR_INVALID_LIMITS, "Limits maximum (%u) cannot be smaller than minimum (%u)", limits.maximum, limits.minimum);
}

void ValidateTable(TableDesc& table, Environment& env, Module* m)
{
  if(table.element_type != TE_anyfunc)
    AppendError(env.errors, m, ERR_INVALID_TABLE_ELEMENT_TYPE, "Table element type is %hhi: only anyfunc allowed.", table.element_type);
  ValidateLimits(table.resizable, env, m);
}

void ValidateMemory(MemoryDesc& mem, Environment& env, Module* m)
{
  ValidateLimits(mem.limits, env, m);
}

void ValidateBlockSignature(varsint7 sig, Environment& env, Module* m)
{
  switch(sig)
  {
  case TE_i32:
  case TE_i64:
  case TE_f32:
  case TE_f64:
  case TE_void:
    break;
  default:
    AppendError(env.errors, m, ERR_INVALID_BLOCK_SIGNATURE, "%hhi is not a valid block signature type.", sig);
  }
}

void ValidatePopType(Stack<varsint7>& values, varsint7 type, Environment& env, Module* m)
{
  if(values.Size() < 1)
    AppendError(env.errors, m, ERR_INVALID_VALUE_STACK, "Expected a value on the stack, but stack was empty.");
  else
  {
    varsint7 t = values.Pop();
    if(type != 0 && t != type)
      AppendError(env.errors, m, ERR_INVALID_VALUE_STACK, "Expected %hhi on the stack, but found %hhi.", type, t);
  }
}

void ValidateSignature(varsint7 sig, Stack<varsint7>& values, Environment& env, Module* m)
{
  if(sig != TE_void)
  {
    if(values.Size() > 0)
    {
      if(values.Peek() != sig)
        AppendError(env.errors, m, ERR_INVALID_VALUE_STACK, "block signature expected %hhi, but value stack had %hhi instead!", sig, values.Peek());
    }
    else
      AppendError(env.errors, m, ERR_INVALID_VALUE_STACK, "block signature expected %hhi, but value stack was empty!", sig);
  }
  else if(values.Size() != 0)
    AppendError(env.errors, m, ERR_INVALID_VALUE_STACK, "block signature was void, but stack wasn't empty!");
}

struct ControlBlock
{
  size_t limit; // Previous limit of value stack
  varsint7 sig; // Block signature
  byte type; // instruction that pushed this label
};

void ValidateBranch(varuint32 depth, Stack<varsint7>& values, Stack<ControlBlock>& control, Environment& env, Module* m)
{
  if(depth >= control.Size())
    AppendError(env.errors, m, ERR_INVALID_BRANCH_DEPTH, "Invalid branch depth: %u exceeds %zu", depth, control.Size());
  if(control[depth].type != OP_loop) // any branch that targets a loop just discards the entire stack to that point.
    ValidateSignature(control[depth].sig, values, env, m);
}

void ValidateBranchTable(varuint32 n_table, varuint32* table, varuint32 def, Stack<varsint7>& values, Stack<ControlBlock>& control, Environment& env, Module* m)
{
  ValidateBranch(def, values, control, env, m);
  for(varuint32 i = 0; i < n_table; ++i)
    ValidateBranch(table[i], values, control, env, m);
}

template<typename T, TYPE_ENCODING PUSH>
void ValidateLoad(varuint32 align, Stack<varsint7>& values, Environment& env, Module* m)
{
  if(!ModuleMemory(*m, 0))
    AppendError(env.errors, m, ERR_INVALID_MEMORY_INDEX, "No default linear memory in module.");
  if((1ULL << align) > sizeof(T))
    AppendError(env.errors, m, ERR_INVALID_MEMORY_ALIGNMENT, "Alignment of %u exceeds number of accessed bytes %i", (1 << align), sizeof(T));
  ValidatePopType(values, TE_i32, env, m);
  values.Push(PUSH);
}

template<typename T, TYPE_ENCODING POP>
void ValidateStore(varuint32 align, Stack<varsint7>& values, Environment& env, Module* m)
{
  if(!ModuleMemory(*m, 0))
    AppendError(env.errors, m, ERR_INVALID_MEMORY_INDEX, "No default linear memory in module.");
  if((1ULL << align) > sizeof(T))
    AppendError(env.errors, m, ERR_INVALID_MEMORY_ALIGNMENT, "Alignment of %u exceeds number of accessed bytes %i", (1 << align), sizeof(T));
  ValidatePopType(values, POP, env, m);
  ValidatePopType(values, TE_i32, env, m);
}

template<TYPE_ENCODING ARG1, TYPE_ENCODING RESULT>
void ValidateUnaryOp(Stack<varsint7>& values, Environment& env, Module* m)
{
  ValidatePopType(values, ARG1, env, m);
  values.Push(RESULT);
}

template<TYPE_ENCODING ARG1, TYPE_ENCODING ARG2, TYPE_ENCODING RESULT>
void ValidateBinaryOp(Stack<varsint7>& values, Environment& env, Module* m)
{
  ValidatePopType(values, ARG2, env, m);
  ValidatePopType(values, ARG1, env, m);
  values.Push(RESULT);
}

void ValidateFunctionSig(Stack<varsint7>& values, FunctionSig& sig, Environment& env, Module* m)
{
  for(varuint32 i = sig.n_params; i-- > 0;) // Pop in reverse order
    ValidatePopType(values, sig.params[i], env, m);

  if(sig.n_returns > 1)
    AppendError(env.errors, m, ERR_INVALID_FUNCTION_INDEX, "Cannot return more than one value yet, tried to return %i.", sig.n_returns);

  for(varuint32 i = 0; i < sig.n_returns; ++i)
    values.Push(sig.returns[i]);
}

void ValidateIndirectCall(Stack<varsint7>& values, varuint32 sig, Environment& env, Module* m)
{
  ValidatePopType(values, TE_i32, env, m); // Pop callee
  if(sig < m->type.n_functions)
    ValidateFunctionSig(values, m->type.functions[sig], env, m);
  else
    AppendError(env.errors, m, ERR_INVALID_FUNCTION_INDEX, "signature index was %u, which is an invalid function signature index.", sig);
}

void ValidateCall(Stack<varsint7>& values, varuint32 callee, Environment& env, Module* m)
{
  FunctionSig* sig = ModuleFunction(*m, callee);
  if(sig)
    ValidateFunctionSig(values, *sig, env, m);
  else
    AppendError(env.errors, m, ERR_INVALID_FUNCTION_INDEX, "callee was %u, which is an invalid function index.", callee);
}

void ValidateInstruction(Instruction& ins, Stack<varsint7>& values, Stack<ControlBlock>& control, varuint32 n_locals, varsint7* locals, Environment& env, Module* m)
{
  switch(ins.opcode)
  {
  case OP_unreachable:
  case OP_nop:
    break;
  case OP_if:
    ValidatePopType(values, TE_i32, env, m);
  case OP_block:
  case OP_loop:
    ValidateBlockSignature(ins.immediates[0]._varsint7, env, m);
    break;
  case OP_else:
  case OP_end:
    break;
  case OP_br_if:
    ValidatePopType(values, TE_i32, env, m);
  case OP_br:
    ValidateBranch(ins.immediates[0]._varuint32, values, control, env, m);
    break;
  case OP_br_table:
    ValidatePopType(values, TE_i32, env, m);
    ValidateBranchTable(ins.immediates[0].n_table, ins.immediates[0].table, ins.immediates[1]._varuint32, values, control, env, m);
    break;
  case OP_return:
    if(control.Size() > 0)
      ValidateSignature(control[0].sig, values, env, m);
    else
      AppendError(env.errors, m, ERR_INVALID_FUNCTION_BODY, "Empty control stack at return statement.");
    break;

    // Call operators
  case OP_call:
    ValidateCall(values, ins.immediates[0]._varuint32, env, m);
    break;
  case OP_call_indirect:
    ValidateIndirectCall(values, ins.immediates[0]._varuint32, env, m);
    break;

    // Parametric operators
  case OP_drop:
    ValidatePopType(values, 0, env, m);
    break;
  case OP_select:
    ValidatePopType(values, TE_i32, env, m);
    ValidatePopType(values, 0, env, m);
    ValidatePopType(values, 0, env, m);
    break;

    // Variable access
  case OP_get_local:
    if(ins.immediates[0]._varuint32 >= n_locals)
      AppendError(env.errors, m, ERR_INVALID_LOCAL_INDEX, "Invalid local index for get_local.");
    values.Push(locals[ins.immediates[0]._varuint32]);
    break;
  case OP_set_local:
    if(ins.immediates[0]._varuint32 >= n_locals)
      AppendError(env.errors, m, ERR_INVALID_LOCAL_INDEX, "Invalid local index for set_local.");
    ValidatePopType(values, locals[ins.immediates[0]._varuint32], env, m);
    break;
  case OP_tee_local:
    if(ins.immediates[0]._varuint32 >= n_locals)
      AppendError(env.errors, m, ERR_INVALID_LOCAL_INDEX, "Invalid local index for set_local.");
    ValidatePopType(values, locals[ins.immediates[0]._varuint32], env, m);
    values.Push(locals[ins.immediates[0]._varuint32]);
    break;
  case OP_get_global:
  {
    GlobalDesc* desc = ModuleGlobal(*m, ins.immediates[0]._varuint32);
    if(!desc)
      AppendError(env.errors, m, ERR_INVALID_LOCAL_INDEX, "Invalid global index for get_global.");
    else
      values.Push(desc->type);
  }
  break;
  case OP_set_global:
  {
    GlobalDesc* desc = ModuleGlobal(*m, ins.immediates[0]._varuint32);
    if(!desc)
      AppendError(env.errors, m, ERR_INVALID_LOCAL_INDEX, "Invalid global index for get_global.");
    else
      ValidatePopType(values, desc->type, env, m);
  }
  break;

  // Memory-related operators
  case OP_i32_load: ValidateLoad<int32_t, TE_i32>(ins.immediates[0]._memflags, values, env, m); break;
  case OP_i64_load: ValidateLoad<int64_t, TE_i64>(ins.immediates[0]._memflags, values, env, m); break;
  case OP_f32_load: ValidateLoad<float, TE_f32>(ins.immediates[0]._memflags, values, env, m); break;
  case OP_f64_load: ValidateLoad<double, TE_f64>(ins.immediates[0]._memflags, values, env, m); break;
  case OP_i32_load8_s:
  case OP_i32_load8_u: ValidateLoad<int8_t, TE_i32>(ins.immediates[0]._memflags, values, env, m); break;
  case OP_i32_load16_s:
  case OP_i32_load16_u: ValidateLoad<int16_t, TE_i32>(ins.immediates[0]._memflags, values, env, m); break;
  case OP_i64_load8_s:
  case OP_i64_load8_u: ValidateLoad<int8_t, TE_i64>(ins.immediates[0]._memflags, values, env, m); break;
  case OP_i64_load16_s:
  case OP_i64_load16_u: ValidateLoad<int16_t, TE_i64>(ins.immediates[0]._memflags, values, env, m); break;
  case OP_i64_load32_s:
  case OP_i64_load32_u: ValidateLoad<int32_t, TE_i64>(ins.immediates[0]._memflags, values, env, m); break;
  case OP_i32_store: ValidateStore<int32_t, TE_i32>(ins.immediates[0]._memflags, values, env, m); break;
  case OP_i64_store: ValidateStore<int64_t, TE_i64>(ins.immediates[0]._memflags, values, env, m); break;
  case OP_f32_store: ValidateStore<float, TE_f32>(ins.immediates[0]._memflags, values, env, m); break;
  case OP_f64_store: ValidateStore<double, TE_f64>(ins.immediates[0]._memflags, values, env, m); break;
  case OP_i32_store8: ValidateStore<int8_t, TE_i32>(ins.immediates[0]._memflags, values, env, m); break;
  case OP_i32_store16: ValidateStore<int16_t, TE_i32>(ins.immediates[0]._memflags, values, env, m); break;
  case OP_i64_store8: ValidateStore<int8_t, TE_i64>(ins.immediates[0]._memflags, values, env, m); break;
  case OP_i64_store16: ValidateStore<int16_t, TE_i64>(ins.immediates[0]._memflags, values, env, m); break;
  case OP_i64_store32: ValidateStore<int32_t, TE_i64>(ins.immediates[0]._memflags, values, env, m); break;
  case OP_memory_size:
    if(ins.immediates[0]._varuint1 != 0)
      AppendError(env.errors, m, ERR_INVALID_RESERVED_VALUE, "reserved must be 0.");
    values.Push(TE_i32);
    break;
  case OP_memory_grow:
    if(ins.immediates[0]._varuint1 != 0)
      AppendError(env.errors, m, ERR_INVALID_RESERVED_VALUE, "reserved must be 0.");
    ValidatePopType(values, TE_i32, env, m);
    values.Push(TE_i32);
    break;

    // Constants
  case OP_i32_const: values.Push(TE_i32); break;
  case OP_i64_const: values.Push(TE_i64); break;
  case OP_f32_const: values.Push(TE_f32); break;
  case OP_f64_const: values.Push(TE_f64); break;

    // Comparison operators
  case OP_i32_eqz: ValidateUnaryOp<TE_i32, TE_i32>(values, env, m); break;
  case OP_i32_eq:
  case OP_i32_ne:
  case OP_i32_lt_s:
  case OP_i32_lt_u:
  case OP_i32_gt_s:
  case OP_i32_gt_u:
  case OP_i32_le_s:
  case OP_i32_le_u:
  case OP_i32_ge_s:
  case OP_i32_ge_u: ValidateBinaryOp<TE_i32, TE_i32, TE_i32>(values, env, m); break;
  case OP_i64_eqz: ValidateUnaryOp<TE_i64, TE_i32>(values, env, m); break;
  case OP_i64_eq:
  case OP_i64_ne:
  case OP_i64_lt_s:
  case OP_i64_lt_u:
  case OP_i64_gt_s:
  case OP_i64_gt_u:
  case OP_i64_le_s:
  case OP_i64_le_u:
  case OP_i64_ge_s:
  case OP_i64_ge_u: ValidateBinaryOp<TE_i64, TE_i64, TE_i32>(values, env, m); break;
  case OP_f32_eq:
  case OP_f32_ne:
  case OP_f32_lt:
  case OP_f32_gt:
  case OP_f32_le:
  case OP_f32_ge: ValidateBinaryOp<TE_f32, TE_f32, TE_i32>(values, env, m); break;
  case OP_f64_eq:
  case OP_f64_ne:
  case OP_f64_lt:
  case OP_f64_gt:
  case OP_f64_le:
  case OP_f64_ge: ValidateBinaryOp<TE_f64, TE_f64, TE_i32>(values, env, m); break;

    // Numeric operators
  case OP_i32_clz:
  case OP_i32_ctz:
  case OP_i32_popcnt: ValidateUnaryOp<TE_i32, TE_i32>(values, env, m); break;
  case OP_i32_add:
  case OP_i32_sub:
  case OP_i32_mul:
  case OP_i32_div_s:
  case OP_i32_div_u:
  case OP_i32_rem_s:
  case OP_i32_rem_u:
  case OP_i32_and:
  case OP_i32_or:
  case OP_i32_xor:
  case OP_i32_shl:
  case OP_i32_shr_s:
  case OP_i32_shr_u:
  case OP_i32_rotl:
  case OP_i32_rotr: ValidateBinaryOp<TE_i32, TE_i32, TE_i32>(values, env, m); break;
  case OP_i64_clz:
  case OP_i64_ctz:
  case OP_i64_popcnt: ValidateUnaryOp<TE_i64, TE_i64>(values, env, m); break;
  case OP_i64_add:
  case OP_i64_sub:
  case OP_i64_mul:
  case OP_i64_div_s:
  case OP_i64_div_u:
  case OP_i64_rem_s:
  case OP_i64_rem_u:
  case OP_i64_and:
  case OP_i64_or:
  case OP_i64_xor:
  case OP_i64_shl:
  case OP_i64_shr_s:
  case OP_i64_shr_u:
  case OP_i64_rotl:
  case OP_i64_rotr: ValidateBinaryOp<TE_i64, TE_i64, TE_i64>(values, env, m); break;
  case OP_f32_abs:
  case OP_f32_neg:
  case OP_f32_ceil:
  case OP_f32_floor:
  case OP_f32_trunc:
  case OP_f32_nearest:
  case OP_f32_sqrt: ValidateUnaryOp<TE_f32, TE_f32>(values, env, m); break;
  case OP_f32_add:
  case OP_f32_sub:
  case OP_f32_mul:
  case OP_f32_div:
  case OP_f32_min:
  case OP_f32_max:
  case OP_f32_copysign: ValidateBinaryOp<TE_f32, TE_f32, TE_f32>(values, env, m); break;
  case OP_f64_abs:
  case OP_f64_neg:
  case OP_f64_ceil:
  case OP_f64_floor:
  case OP_f64_trunc:
  case OP_f64_nearest:
  case OP_f64_sqrt: ValidateUnaryOp<TE_f64, TE_f64>(values, env, m); break;
  case OP_f64_add:
  case OP_f64_sub:
  case OP_f64_mul:
  case OP_f64_div:
  case OP_f64_min:
  case OP_f64_max:
  case OP_f64_copysign: ValidateBinaryOp<TE_f64, TE_f64, TE_f64>(values, env, m); break;

    // Conversions
  case OP_i32_wrap_i64: ValidateUnaryOp<TE_i64, TE_i32>(values, env, m); break;
  case OP_i32_trunc_s_f32:
  case OP_i32_trunc_u_f32: ValidateUnaryOp<TE_f32, TE_i32>(values, env, m); break;
  case OP_i32_trunc_s_f64:
  case OP_i32_trunc_u_f64: ValidateUnaryOp<TE_f64, TE_i32>(values, env, m); break;
  case OP_i64_extend_s_i32:
  case OP_i64_extend_u_i32: ValidateUnaryOp<TE_i32, TE_i64>(values, env, m); break;
  case OP_i64_trunc_s_f32:
  case OP_i64_trunc_u_f32: ValidateUnaryOp<TE_f32, TE_i64>(values, env, m); break;
  case OP_i64_trunc_s_f64:
  case OP_i64_trunc_u_f64: ValidateUnaryOp<TE_f64, TE_i64>(values, env, m); break;
  case OP_f32_convert_s_i32:
  case OP_f32_convert_u_i32: ValidateUnaryOp<TE_i32, TE_f32>(values, env, m); break;
  case OP_f32_convert_s_i64:
  case OP_f32_convert_u_i64: ValidateUnaryOp<TE_i64, TE_f32>(values, env, m); break;
  case OP_f32_demote_f64: ValidateUnaryOp<TE_f64, TE_f32>(values, env, m); break;
  case OP_f64_convert_s_i32:
  case OP_f64_convert_u_i32: ValidateUnaryOp<TE_i32, TE_f64>(values, env, m); break;
  case OP_f64_convert_s_i64:
  case OP_f64_convert_u_i64: ValidateUnaryOp<TE_i64, TE_f64>(values, env, m); break;
  case OP_f64_promote_f32: ValidateUnaryOp<TE_f32, TE_f64>(values, env, m); break;

    // Reinterpretations
  case OP_i32_reinterpret_f32: ValidateUnaryOp<TE_f32, TE_i32>(values, env, m); break;
  case OP_i64_reinterpret_f64: ValidateUnaryOp<TE_f64, TE_i64>(values, env, m); break;
  case OP_f32_reinterpret_i32: ValidateUnaryOp<TE_i32, TE_f32>(values, env, m); break;
  case OP_f64_reinterpret_i64: ValidateUnaryOp<TE_i64, TE_f64>(values, env, m); break;
  default:
    AppendError(env.errors, m, ERR_FATAL_UNKNOWN_INSTRUCTION, "Unknown instruction code %hhu", ins.opcode);
  }
}

varsint7 ValidateInitializer(Instruction& ins, Environment& env, Module* m)
{
  switch(ins.opcode)
  {
  case OP_i32_const: return TE_i32;
  case OP_i64_const: return TE_i64;
  case OP_f32_const: return TE_f32;
  case OP_f64_const: return TE_f64;
  case OP_get_global:
    if(!ModuleGlobal(*m, ins.immediates[0]._varuint32))
      AppendError(env.errors, m, ERR_INVALID_LOCAL_INDEX, "Invalid global index for get_global.");
    else if(ins.immediates[0]._varuint32 < m->importsection.globals)
      return m->importsection.imports[ins.immediates[0]._varuint32].global_desc.type;
    else
      AppendError(env.errors, m, ERR_INVALID_INITIALIZER, "A get_global initializer must be an import.", ins.opcode);
    return TE_i32;
  }

  AppendError(env.errors, m, ERR_INVALID_INITIALIZER, "An initializer must be a get_global or const instruction, not %hhu", ins.opcode);
  return TE_i32;
}

void ValidateGlobal(GlobalDecl& decl, Environment& env, Module* m)
{
  varsint7 type = ValidateInitializer(decl.init, env, m);
  if(type != decl.desc.type)
    AppendError(env.errors, m, ERR_INVALID_GLOBAL_TYPE, "The global initializer has type %hhi, must be the same as the description type %hhi.", type, decl.desc.type);
}

void ValidateExport(Export& e, Environment& env, Module* m)
{
  ValidateIdentifier(e.name);

  switch(e.kind)
  {
  case KIND_FUNCTION:
    if(!ModuleFunction(*m, e.index))
      AppendError(env.errors, m, ERR_INVALID_FUNCTION_INDEX, "Invalid function index %u", e.index);
    break;
  case KIND_TABLE:
    if(!ModuleTable(*m, e.index))
      AppendError(env.errors, m, ERR_INVALID_TABLE_INDEX, "Invalid table index %u", e.index);
    break;
  case KIND_MEMORY:
    if(!ModuleMemory(*m, e.index))
      AppendError(env.errors, m, ERR_INVALID_MEMORY_INDEX, "Invalid memory index %u", e.index);
    break;
  case KIND_GLOBAL:
  {
    GlobalDesc* g = ModuleGlobal(*m, e.index);
    if(!g)
      AppendError(env.errors, m, ERR_INVALID_GLOBAL_INDEX, "Invalid global index %u", e.index);
    else if(g->mutability)
      AppendError(env.errors, m, ERR_MUTABLE_GLOBAL, "Exported global %s index %u should not be mutable.", e.name.bytes, e.index);
    break;
  }
  default:
    AppendError(env.errors, m, ERR_FATAL_UNKNOWN_KIND, "The %s export has invalid kind %hhu", e.name.bytes, e.kind);
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
        AppendError(env.errors, m, ERR_INVALID_GLOBAL_INDEX, "Invalid global import %u", ins.immediates[0]._varsint32);
      else
        return EvalInitializerI32(p.first->global.globals[p.second->index - p.first->importsection.globals].init, env, p.first);
      break;
    }
    i -= m->importsection.globals;
    if(i < m->global.n_globals)
      global = &m->global.globals[i];

    if(!global)
      AppendError(env.errors, m, ERR_INVALID_GLOBAL_INDEX, "Invalid global index %u", ins.immediates[0]._varsint32);
    else
      return EvalInitializerI32(global->init, env, nullptr);
    break;
  }
  default:
    AppendError(env.errors, m, ERR_INVALID_INITIALIZER, "Expected i32 type but got %hhu", ins.opcode);
  }

  return 0;
}

void ValidateTableOffset(TableInit& init, Environment& env, Module* m)
{
  varsint7 type = ValidateInitializer(init.offset, env, m);
  if(type != TE_i32)
    AppendError(env.errors, m, ERR_INVALID_TABLE_TYPE, "Expected table offset instruction type of i32, got %hhi instead.", type);

  TableDesc* table = ModuleTable(*m, init.index);
  if(!table)
    AppendError(env.errors, m, ERR_INVALID_TABLE_INDEX, "Invalid table index %u", init.index);
  else if(table->element_type == TE_anyfunc)
  {
    varsint32 offset = EvalInitializerI32(init.offset, env, m);
    if(offset + init.n_elems > table->resizable.minimum)
      AppendError(env.errors, m, ERR_INVALID_TABLE_OFFSET, "Offset (%i) plus element count (%u) exceeds minimum table length (%u)", offset, init.n_elems, table->resizable.minimum);

    for(varuint32 i = 0; i < init.n_elems; ++i)
      if(!ModuleFunction(*m, init.elems[i]))
        AppendError(env.errors, m, ERR_INVALID_FUNCTION_INDEX, "Invalid element initializer %u function index: %u", i, init.elems[i]);
  }
  else
    AppendError(env.errors, m, ERR_INVALID_TABLE_ELEMENT_TYPE, "Invalid table element type %hhi", table->element_type);
}

void ValidateEndBlock(ControlBlock block, Stack<varsint7>& values, Environment& env, Module* m)
{
  ValidateSignature(block.sig, values, env, m);
  values.SetLimit(block.limit); // Reset old limit value
}

void ValidateFunctionBody(FunctionSig& sig, FunctionBody& body, Environment& env, Module* m)
{
  Instruction* cur = body.body;
  Stack<ControlBlock> control; // control-flow stack that must be closed by end instructions
  Stack<varsint7> values; // Current stack of value types
  varsint7 ret = TE_void;
  if(sig.n_returns > 0)
    ret = sig.returns[0];

  // Calculate function locals
  varuint32 n_local = sig.n_params + body.n_locals;

  varsint7* locals = tmalloc<varsint7>(n_local);
  if(locals)
    memcpy(locals, sig.params, sig.n_params * sizeof(varsint7));
  n_local = sig.n_params;
  for(varuint32 i = 0; i < body.n_locals; ++i)
    locals[n_local++] = body.locals[i];

  control.Push({ values.Limit(), ret, OP_block }); // Push the function body block with the function signature

  if(!body.n_body)
    return AppendError(env.errors, m, ERR_INVALID_FUNCTION_BODY, "Cannot have an empty function body!");

  for(varuint32 i = 0; i < body.n_body; ++i)
  {
    ValidateInstruction(cur[i], values, control, n_local, locals, env, m);

    switch(cur[i].opcode)
    {
    case OP_block:
    case OP_loop:
    case OP_if:
      control.Push({ values.Limit(), cur[i].immediates[0]._varsint7, cur[i].opcode });
      values.SetLimit(values.Size() + values.Limit());
      break;
    case OP_end:
      if(!control.Size())
        AppendError(env.errors, m, ERR_INVALID_FUNCTION_BODY, "Mismatched end instruction at index %u!", i);
      else
        ValidateEndBlock(control.Pop(), values, env, m);
      break;
    case OP_else:
      if(!control.Size())
        AppendError(env.errors, m, ERR_INVALID_FUNCTION_BODY, "Mismatched else instruction at index %u!", i);
      else
      {
        ControlBlock block = control.Pop();
        if(block.type != OP_if)
          AppendError(env.errors, m, ERR_INVALID_FUNCTION_BODY, "Expected else instruction to terminate if block, but found %hhi instead.", block.type);
        ValidateEndBlock(block, values, env, m);
        control.Push({ values.Limit(), block.sig, OP_else }); // Push a new else block that must be terminated by an end instruction
        values.SetLimit(values.Size() + values.Limit());
      }
    }
  }

  for(varuint32 i = 0; i < sig.n_returns; ++i)
    ValidatePopType(values, sig.returns[i], env, m);

  if(control.Size() > 0)
    AppendError(env.errors, m, ERR_INVALID_FUNCTION_BODY, "Control stack not fully terminated, off by %zu", control.Size());

  if(values.Size() > 0 || values.Limit() > 0)
    AppendError(env.errors, m, ERR_INVALID_VALUE_STACK, "Value stack not fully empty, off by %zu", values.Size() + values.Limit());

  if(cur[body.n_body - 1].opcode != OP_end)
    AppendError(env.errors, m, ERR_INVALID_FUNCTION_BODY, "Expected end instruction to terminate function body, got %hhu instead.", cur[body.n_body - 1].opcode);
}

void ValidateDataOffset(DataInit& init, Environment& env, Module* m)
{
  varsint7 type = ValidateInitializer(init.offset, env, m);
  if(type != TE_i32)
    AppendError(env.errors, m, ERR_INVALID_MEMORY_TYPE, "Expected memory offset instruction type of i32, got %hhi instead.", type);

  MemoryDesc* memory = ModuleMemory(*m, init.index);
  if(!memory)
    AppendError(env.errors, m, ERR_INVALID_MEMORY_INDEX, "Invalid memory index %u", init.index);
  else
  {
    varsint32 offset = EvalInitializerI32(init.offset, env, m);
    if(offset + init.data.n_bytes > memory->limits.minimum)
      AppendError(env.errors, m, ERR_INVALID_MEMORY_OFFSET, "Offset (%i) plus element count (%u) exceeds minimum memory length (%u)", offset, init.data.n_bytes, memory->limits.minimum);
  }
}

template<class T, void(*VALIDATE)(T&, Environment&, Module*)>
void ValidateSection(T* a, varuint32 n, Environment& env, Module* m)
{
  for(varuint32 i = 0; i < n; ++i)
    VALIDATE(a[i], env, m);
}

void ValidateModule(Environment& env, Module& m)
{
  if(m.knownsections&(1 << SECTION_TYPE))
    ValidateSection<FunctionSig, &ValidateFunctionSig>(m.type.functions, m.type.n_functions, env, &m);

  if(m.knownsections&(1 << SECTION_IMPORT))
    ValidateSection<Import, &ValidateImport>(m.importsection.imports, m.importsection.n_import, env, &m);

  if(m.knownsections&(1 << SECTION_FUNCTION))
  {
    ValidateSection<varuint32, &ValidateFunction>(m.function.funcdecl, m.function.n_funcdecl, env, &m);

    if(m.function.n_funcdecl != m.code.n_funcbody)
      AppendError(env.errors, &m, ERR_FUNCTION_BODY_MISMATCH, "The number of function declarations (%u) does not equal the number of function bodies (%u)", m.function.n_funcdecl, m.code.n_funcbody);
  }

  if(m.knownsections&(1 << SECTION_TABLE))
    ValidateSection<TableDesc, &ValidateTable>(m.table.tables, m.table.n_tables, env, &m);

  if(m.knownsections&(1 << SECTION_MEMORY))
    ValidateSection<MemoryDesc, &ValidateMemory>(m.memory.memory, m.memory.n_memory, env, &m);

  if(m.knownsections&(1 << SECTION_GLOBAL))
    ValidateSection<GlobalDecl, &ValidateGlobal>(m.global.globals, m.global.n_globals, env, &m);

  if(m.knownsections&(1 << SECTION_EXPORT))
    ValidateSection<Export, &ValidateExport>(m.exportsection.exports, m.exportsection.n_exports, env, &m);

  if(m.knownsections&(1 << SECTION_START))
  {
    FunctionSig* f = ModuleFunction(m, m.start);
    if(f)
    {
      if(m.start < m.importsection.functions)
        AppendError(env.errors, &m, ERR_INVALID_START_FUNCTION, "The start function (%hhu) cannot be an imported function", m.start);
      if(f->n_params > 0 || f->n_returns > 0)
        AppendError(env.errors, &m, ERR_INVALID_START_FUNCTION, "Starting function must have no parameters and no return value, instead it has %u parameters and %u return values.", f->n_params, f->n_returns);
    }
    else
      AppendError(env.errors, &m, ERR_INVALID_FUNCTION_INDEX, "Start module function index %u does not exist.", m.start);
  }

  if(m.knownsections&(1 << SECTION_ELEMENT))
    ValidateSection<TableInit, &ValidateTableOffset>(m.element.elements, m.element.n_elements, env, &m);

  if(m.knownsections&(1 << SECTION_CODE))
  {
    for(varuint32 j = 0; j < m.code.n_funcbody; ++j)
    {
      if(m.function.funcdecl[j] < m.type.n_functions)
        ValidateFunctionBody(m.type.functions[m.function.funcdecl[j]], m.code.funcbody[j], env, &m);
    }
  }

  if(m.knownsections&(1 << SECTION_DATA))
    ValidateSection<DataInit, &ValidateDataOffset>(m.data.data, m.data.n_data, env, &m);
}

// Performs all post-load validation that couldn't be done during parsing
void ValidateEnvironment(Environment& env)
{
  int tmp;
  if(!(env.flags&ENV_STRICT))
  {
    kh_put_cimport(env.cimports, "_native_wasm_to_c", &tmp);
    kh_put_cimport(env.cimports, "_native_wasm_from_c", &tmp);
  }
  // TODO: replace with proper lib lookup
  //kh_put_cimport(env.cimports, "_native_wasm_internal_env_print", &tmp);
  //kh_put_cimport(env.cimports, "GetStdHandle", &tmp);
  //kh_put_cimport(env.cimports, "WriteConsoleA", &tmp);

  for(size_t i = 0; i < env.n_modules; ++i)
    ValidateModule(env, env.modules[i]);
}

bool ValidateSectionOrder(uint32& sections, varuint7 opcode)
{
  return (sections & ((~0) << opcode)) == 0;
}