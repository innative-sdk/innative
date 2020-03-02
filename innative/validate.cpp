// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "validate.h"
#include "utility.h"
#include "stack.h"
#include "link.h"
#include <stdio.h>
#include <stdarg.h>
#include <atomic>
#include <limits>

using namespace innative;
using namespace utility;

namespace innative {
  namespace internal {
    struct ControlBlock
    {
      size_t limit; // Previous limit of value stack
      varsint7 sig; // Block signature
      uint8_t type; // instruction that pushed this label
    };
  }
}

// UTF8 validator based off the official unicode C validator source
bool innative::ValidateIdentifier(const ByteArray& b)
{
  static const char trailingBytesForUTF8[256] = {
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
    1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  2,  2,  2,  2,  2,  2,  2,  2,
    2,  2,  2,  2,  2,  2,  2,  2,  3,  3,  3,  3,  3,  3,  3,  3,  4,  4,  4,  4,  5,  5,  5,  5
  };

  varuint32 i = 0;
  unsigned char a;
  unsigned char c;
  while(i < b.size())
  {
    c          = b[i];
    int length = trailingBytesForUTF8[c] + 1;
    if(!length) // This is an invalid first code point
      return false;
    varuint32 index = i + length;
    if(index > b.size())
      return false;

    switch(length)
    {
    default:
      return false;
      /* Everything else falls through when "true"... */
    case 4:
      if((a = b[--index]) < 0x80 || a > 0xBF)
        return false;
    case 3:
      if((a = b[--index]) < 0x80 || a > 0xBF)
        return false;
    case 2:
      if((a = b[--index]) < 0x80 || a > 0xBF)
        return false;

      switch(c)
      {
        /* no fall-through in this inner switch */
      case 0xE0:
        if(a < 0xA0)
          return false;
        break;
      case 0xED:
        if(a > 0x9F)
          return false;
        break;
      case 0xF0:
        if(a < 0x90)
          return false;
        break;
      case 0xF4:
        if(a > 0x8F)
          return false;
        break;
      default:
        if(a < 0x80)
          return false;
      }

    case 1:
      if(c >= 0x80 && c < 0xC2)
        return false;
    }
    if(c > 0xF4)
      return false;
    i += length;
  }

  return i == b.size(); // If these aren't exactly equal there was an expected length mismatch
}

void innative::AppendError(const Environment& env, ValidationError*& errors, Module* m, int code, const char* fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  int len = vsnprintf(0, 0, fmt, args);
  va_end(args);
  ValidationError* err = reinterpret_cast<ValidationError*>(env.alloc->allocate(sizeof(ValidationError) + len + 1));
  err->error           = reinterpret_cast<char*>(err + 1);
  va_start(args, fmt);
  vsnprintf(err->error, len + 1, fmt, args);
  va_end(args);

  err->error[len] = 0;
  err->code       = code;
  err->m          = -1;
  if(m >= env.modules && size_t(m - env.modules) < env.n_modules)
    err->m = m - env.modules;

  do
  {
    err->next = ((std::atomic<ValidationError*>&)errors).load(std::memory_order_relaxed);
  } while(!((std::atomic<ValidationError*>&)errors)
             .compare_exchange_weak(err->next, err, std::memory_order_release, std::memory_order_relaxed));
}

void innative::ValidateFunctionSig(const FunctionType& sig, Environment& env, Module* m)
{
  if(sig.form == TE_func)
  {
    if(sig.n_returns > 1)
      AppendError(env, env.errors, m, ERR_MULTIPLE_RETURN_VALUES, "Return count of %u encountered: only 0 or 1 allowed.",
                  sig.n_returns);
  }
  else
  {
    char buf[10];
    AppendError(env, env.errors, m, ERR_UNKNOWN_SIGNATURE_TYPE,
                "Illegal function type %s encountered: only TE_func allowed",
                EnumToString(TYPE_ENCODING_MAP, sig.form, buf, 10));
  }
}

bool innative::MatchFunctionType(const FunctionType& a, const FunctionType& b)
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

void innative::ValidateImport(const Import& imp, Environment& env, Module* m)
{
  if(!ValidateIdentifier(imp.module_name))
    AppendError(env, env.errors, m, ERR_INVALID_UTF8_ENCODING, "Identifier not valid UTF8: %s", imp.module_name.str());
  if(!ValidateIdentifier(imp.export_name))
    AppendError(env, env.errors, m, ERR_INVALID_UTF8_ENCODING, "Identifier not valid UTF8: %s", imp.export_name.str());

  imp.alternate = false;
  khint_t iter  = kh_get_modules(
    env.modulemap,
    imp.module_name); // WASM modules do not understand !CALL convention appendings, so we use the full name no matter what
  if(iter == kh_end(env.modulemap))
  {
    if(env.cimports)
    {
    RESTART_IMPORT_CHECK:
      std::string name    = ABIMangle(CanonImportName(imp, env.system), CURRENT_ABI, GetCallingConvention(imp),
                                   !m ? 0 : GetParameterBytes(*m, imp));
      khiter_t iterimport = kh_get_cimport(env.cimports, ByteArray::Identifier(name.c_str(), name.size()));
      if(kh_exist2(env.cimports, iterimport))
      {
        imp.ignore = kh_value(env.cimports, iterimport); // if true, this is a fake symbol

        if(env.flags & ENV_WHITELIST)
        {
          khiter_t itermodule = kh_get_modulepair(
            env.whitelist,
            CanonWhitelist(imp.module_name.str(), "", env.system).c_str()); // Check for a wildcard match first
          if(!kh_exist2(env.whitelist, itermodule))
          {
            khiter_t iterexport = kh_get_modulepair(
              env.whitelist,
              CanonWhitelist(imp.module_name.str(), imp.export_name.str(), env.system)
                .c_str()); // We already canonized the whitelist imports to eliminate unnecessary !C specifiers
            if(!kh_exist2(env.whitelist, iterexport))
              return AppendError(env, env.errors, m, ERR_ILLEGAL_C_IMPORT,
                                 "%s:%s is not a whitelisted C import, nor a valid webassembly import.",
                                 imp.module_name.str(), imp.export_name.str());
            if(imp.kind != WASM_KIND_FUNCTION)
              return AppendError(env, env.errors, m, ERR_ILLEGAL_C_IMPORT,
                                 "%s:%s is not a function. You can only import C functions at this time.",
                                 imp.module_name.str(), imp.export_name.str());
          }
        }
        return; // This is valid
      }
      else if(!imp.alternate)
      {
        imp.alternate = true;
        // We use goto here because the return keywords prevent us from wrapping the whitelist check in a function
        goto RESTART_IMPORT_CHECK;
      }

      if(IsSystemImport(imp.module_name, env.system))
      {
        if(!imp.export_name.size())
          return AppendError(env, env.errors, m, ERR_EMPTY_IMPORT, "Empty imports are invalid.");
        return AppendError(env, env.errors, m, ERR_UNKNOWN_BLANK_IMPORT, "%s not found in C library imports", name.c_str());
      }
    }

    return AppendError(env, env.errors, m, ERR_UNKNOWN_MODULE, "%s module not found", imp.module_name.str());
  }

  size_t i = kh_value(env.modulemap, iter);
  if(i >= env.n_modules)
    return AppendError(env, env.errors, m, ERR_UNKNOWN_MODULE, "%s module index (%u) not in range (%u)",
                       imp.module_name.str(), i, env.n_modules);

  iter = kh_get_exports(env.modules[i].exports, imp.export_name);
  if(iter == kh_end(env.modules[i].exports))
    return AppendError(env, env.errors, m, ERR_UNKNOWN_EXPORT, "%s export not found", imp.export_name.str());

  varuint32 j = kh_value(env.modules[i].exports, iter);
  if(j >= env.modules[i].exportsection.n_exports)
    return AppendError(env, env.errors, m, ERR_UNKNOWN_EXPORT, "%s export index (%u) not in range (%u)",
                       imp.module_name.str(), j, env.modules[i].exportsection.n_exports);

  Export& exp = env.modules[i].exportsection.exports[j];
  if(exp.kind != imp.kind)
    return AppendError(env, env.errors, m, ERR_IMPORT_EXPORT_TYPE_MISMATCH,
                       "export kind (%u) does not match import kind (%u)", exp.kind, imp.kind);

  if(exp.name != imp.export_name)
    AppendError(env, env.errors, m, ERR_IMPORT_EXPORT_MISMATCH, "export name (%s) does not match import name (%s)",
                exp.name.str(), imp.export_name.str());

  switch(imp.kind)
  {
  case WASM_KIND_FUNCTION:
  {
    FunctionType* func = ModuleFunction(env.modules[i], exp.index);
    if(!func)
      AppendError(env, env.errors, m, ERR_INVALID_FUNCTION_INDEX, "Invalid exported function index %u", exp.index);
    else if(imp.func_desc.type_index >= m->type.n_functypes)
      AppendError(env, env.errors, m, ERR_INVALID_TYPE_INDEX, "Invalid imported function type index %u",
                  imp.func_desc.type_index);
    else if(!MatchFunctionType(m->type.functypes[imp.func_desc.type_index], *func))
      AppendError(env, env.errors, m, ERR_INVALID_FUNCTION_IMPORT_TYPE,
                  "Imported function signature didn't match exported function signature.");
    break;
  }
  case WASM_KIND_TABLE:
  {
    TableDesc* table = ModuleTable(env.modules[i], exp.index);
    if(!table)
      AppendError(env, env.errors, m, ERR_INVALID_TABLE_INDEX, "Invalid exported table index %u", exp.index);
    else
    {
      if(imp.table_desc.resizable.minimum > table->resizable.minimum)
        AppendError(env, env.errors, m, ERR_INVALID_IMPORT_TABLE_MINIMUM,
                    "Imported table minimum (%u) greater than exported table minimum (%u).",
                    imp.table_desc.resizable.minimum, table->resizable.minimum);
      if(imp.table_desc.resizable.flags & WASM_LIMIT_HAS_MAXIMUM)
      {
        if(!(table->resizable.flags & WASM_LIMIT_HAS_MAXIMUM))
          AppendError(env, env.errors, m, ERR_INVALID_IMPORT_TABLE_MAXIMUM,
                      "Exported table doesn't have a maximum, but imported table does.");
        else if(imp.table_desc.resizable.maximum < table->resizable.maximum)
          AppendError(env, env.errors, m, ERR_INVALID_IMPORT_TABLE_MAXIMUM,
                      "Imported table maximum (%u) less than exported table maximum (%u).",
                      imp.table_desc.resizable.maximum, table->resizable.maximum);
      }
    }
    break;
  }
  case WASM_KIND_MEMORY:
  {
    MemoryDesc* mem = ModuleMemory(env.modules[i], exp.index);
    if(!mem)
      AppendError(env, env.errors, m, ERR_INVALID_MEMORY_INDEX, "Invalid exported memory index %u", exp.index);
    else
    {
      if(imp.mem_desc.limits.minimum > mem->limits.minimum)
        AppendError(env, env.errors, m, ERR_INVALID_IMPORT_MEMORY_MINIMUM,
                    "Imported memory minimum (%u) greater than exported memory minimum (%u).", imp.mem_desc.limits.minimum,
                    mem->limits.minimum);
      if(imp.mem_desc.limits.minimum > 65536)
        AppendError(env, env.errors, m, ERR_MEMORY_MINIMUM_TOO_LARGE, "Memory minimum cannot exceed 65536");
      if(imp.mem_desc.limits.flags & WASM_LIMIT_HAS_MAXIMUM)
      {
        if(imp.mem_desc.limits.maximum > 65536)
          AppendError(env, env.errors, m, ERR_MEMORY_MAXIMUM_TOO_LARGE, "Memory maximum cannot exceed 65536");
        if(!(mem->limits.flags & WASM_LIMIT_HAS_MAXIMUM))
          AppendError(env, env.errors, m, ERR_INVALID_IMPORT_MEMORY_MAXIMUM,
                      "Exported memory doesn't have a maximum, but imported memory does.");
        else if(imp.mem_desc.limits.maximum < mem->limits.maximum)
          AppendError(env, env.errors, m, ERR_INVALID_IMPORT_MEMORY_MAXIMUM,
                      "Imported memory maximum (%u) less than exported memory maximum (%u).", imp.mem_desc.limits.maximum,
                      mem->limits.maximum);
      }
    }
    break;
  }
  case WASM_KIND_GLOBAL:
  {
    GlobalDesc* global = ModuleGlobal(env.modules[i], exp.index);
    if(!global)
      AppendError(env, env.errors, m, ERR_INVALID_GLOBAL_INDEX, "Invalid exported global index %u", exp.index);
    else if(imp.global_desc.mutability != global->mutability || imp.global_desc.type != global->type)
    {
      char buf[10];
      char buf2[10];
      AppendError(env, env.errors, m, ERR_INVALID_GLOBAL_IMPORT_TYPE,
                  "Imported global type (%s) or mutability (%hhu) does not match exported type (%s) or mutability (%hhu)",
                  EnumToString(TYPE_ENCODING_MAP, imp.global_desc.type, buf, 10), imp.global_desc.mutability,
                  EnumToString(TYPE_ENCODING_MAP, global->type, buf2, 10), global->mutability);
    }
    else if(!(env.features & ENV_FEATURE_MUTABLE_GLOBALS) && imp.global_desc.mutability)
      AppendError(env, env.errors, m, ERR_INVALID_GLOBAL_IMPORT_TYPE, "Imported global cannot be mutable.");
    break;
  }
  default: AppendError(env, env.errors, m, ERR_FATAL_UNKNOWN_KIND, "unknown export kind: %hhu", imp.kind);
  }
}

void innative::ValidateFunction(const FunctionDesc& decl, Environment& env, Module* m)
{
  if(decl.type_index >= m->type.n_functypes)
    AppendError(env, env.errors, m, ERR_INVALID_TYPE_INDEX, "Invalid function declaration type index: %u", decl);
}

void innative::ValidateLimits(const ResizableLimits& limits, Environment& env, Module* m)
{
  if((limits.flags & WASM_LIMIT_HAS_MAXIMUM) && limits.maximum < limits.minimum)
    AppendError(env, env.errors, m, ERR_INVALID_LIMITS, "Limits maximum (%u) cannot be smaller than minimum (%u)",
                limits.maximum, limits.minimum);
}

void innative::ValidateTable(const TableDesc& table, Environment& env, Module* m)
{
  if(table.element_type != TE_funcref)
  {
    char buf[10];
    AppendError(env, env.errors, m, ERR_INVALID_TABLE_ELEMENT_TYPE, "Table element type is %s: only funcref allowed.",
                EnumToString(TYPE_ENCODING_MAP, table.element_type, buf, 10));
  }
  ValidateLimits(table.resizable, env, m);
}

void innative::ValidateMemory(const MemoryDesc& mem, Environment& env, Module* m)
{
  ValidateLimits(mem.limits, env, m);
  if(mem.limits.minimum > 65536)
    AppendError(env, env.errors, m, ERR_MEMORY_MINIMUM_TOO_LARGE, "Memory minimum cannot exceed 65536");
  if((mem.limits.flags & WASM_LIMIT_HAS_MAXIMUM) && mem.limits.maximum > 65536)
    AppendError(env, env.errors, m, ERR_MEMORY_MAXIMUM_TOO_LARGE, "Memory maximum cannot exceed 65536");
}

void innative::ValidateBlockSignature(const Instruction& ins, varsint7 sig, Environment& env, Module* m)
{
  char buf[10];
  switch(sig)
  {
  case TE_i32:
  case TE_i64:
  case TE_f32:
  case TE_f64:
  case TE_void: break;
  default:
    AppendError(env, env.errors, m, ERR_INVALID_BLOCK_SIGNATURE, "[%u] %s is not a valid block signature type.", ins.line,
                EnumToString(TYPE_ENCODING_MAP, sig, buf, 10));
  }
}

namespace innative {
  varsint7 ValidatePopType(const Instruction& ins, Stack<varsint7>& values, varsint7 type, Environment& env, Module* m)
  {
    if(values.Size() < 1)
      AppendError(env, env.errors, m, ERR_EMPTY_VALUE_STACK, "[%u] Expected a value on the stack, but stack was empty.",
                  ins.line);
    else if(values.Peek() != TE_POLY)
    {
      varsint7 t = values.Pop();
      if(type != 0 && t != type)
      {
        if(type == TE_cref && (t == TE_i32 || t == TE_i64)) // Special case for imaginary cref type
          return t;
        char buf[10];
        char buf2[10];
        AppendError(env, env.errors, m, ERR_INVALID_TYPE, "[%u] Expected %s on the stack, but found %s.", ins.line,
                    EnumToString(TYPE_ENCODING_MAP, type, buf, 10), EnumToString(TYPE_ENCODING_MAP, t, buf2, 10));
      }
      return t;
    }
    else
      return TE_POLY;
    return 0;
  }

  void ValidateBranchSignature(const Instruction& ins, varsint7 sig, Stack<varsint7>& values, Environment& env, Module* m)
  {
    char buf[10];
    if(sig != TE_void)
    {
      if(values.Size() > 0)
      {
        char buf2[10];
        // if(values.Size() > 2 || (values.Size() == 2 && values.Peek() != TE_POLY)) // TE_POLY can count as 0
        //  AppendError(env, env.errors, m, ERR_INVALID_TYPE, "block signature expected one value, but value stack had
        //  %zu!", values.Size());
        if(values.Peek() != TE_POLY && values.Peek() != sig)
          AppendError(env, env.errors, m, ERR_INVALID_TYPE,
                      "[%u] block signature expected %s, but value stack had %s instead!", ins.line,
                      EnumToString(TYPE_ENCODING_MAP, sig, buf, 10),
                      EnumToString(TYPE_ENCODING_MAP, values.Peek(), buf2, 10));
        if(values.Peek() == TE_POLY) // If this is a polymorphic type, it now HAS to evaluate to this branch's type
                                     // regardless of which branch is chosen.
          values.Push(sig);
      }
      else
        AppendError(env, env.errors, m, ERR_EMPTY_VALUE_STACK,
                    "[%u] block signature expected %s, but value stack was empty!", ins.line,
                    EnumToString(TYPE_ENCODING_MAP, sig, buf, 10));
    }
  }

  void ValidateSignature(const Instruction& ins, varsint7 sig, Stack<varsint7>& values, Environment& env, Module* m)
  {
    if(sig != TE_void)
      ValidateBranchSignature(ins, sig, values, env, m);
    else if(values.Size() > 1 || (values.Size() == 1 && values.Peek() != TE_POLY)) // TE_POLY can count as 0
      AppendError(env, env.errors, m, ERR_INVALID_VALUE_STACK, "[%u] block signature was void, but stack wasn't empty!",
                  ins.line);
  }

  void ValidateBranch(const Instruction& ins, size_t depth, Stack<varsint7>& values, Stack<internal::ControlBlock>& control,
                      Environment& env, Module* m)
  {
    if(depth >= control.Size())
      AppendError(env, env.errors, m, ERR_INVALID_BRANCH_DEPTH, "[%u] Invalid branch depth: %u exceeds %zu", depth,
                  control.Size(), ins.line);
    else if(control[depth].type != OP_loop) // A branch to a loop always has a signature of TE_void
      ValidateBranchSignature(ins, control[depth].sig, values, env, m);
  }

  // Pops every single value off of the stack (the function assumes the types were already validated) and then pushes a
  // TE_POLY type.
  void PolymorphStack(Stack<varsint7>& values)
  {
    while(values.Size())
      values.Pop();
    values.Push(TE_POLY);
  }

  IN_FORCEINLINE varsint7 GetBlockSig(const internal::ControlBlock& block)
  {
    return block.type == OP_loop ? TE_void : block.sig;
  }

  void ValidateBranchTable(const Instruction& ins, varuint32 n_table, varuint32* table, varuint32 def,
                           Stack<varsint7>& values, Stack<internal::ControlBlock>& control, Environment& env, Module* m)
  {
    ValidateBranch(ins, def, values, control, env, m);
    for(varuint32 i = 0; i < n_table; ++i)
      ValidateBranch(ins, table[i], values, control, env, m);

    // Ensure all block label targets have the exact same signature as the default label
    if(def < control.Size())
    {
      for(varuint32 i = 0; i < n_table; ++i)
      {
        varsint7 type = GetBlockSig(control[def]);
        char buf[10];
        char buf2[10];
        if(table[i] < control.Size() && GetBlockSig(control[table[i]]) != type)
          AppendError(env, env.errors, m, ERR_INVALID_TYPE,
                      "[%u] Branch table target has type signature %s, but default branch has %s", ins.line,
                      EnumToString(TYPE_ENCODING_MAP, control[table[i]].sig, buf, 10),
                      EnumToString(TYPE_ENCODING_MAP, control[def].sig, buf2, 10));
      }
    }
  }

  template<typename T, WASM_TYPE_ENCODING PUSH>
  void ValidateLoad(const Instruction& ins, varuint32 align, Stack<varsint7>& values, Environment& env, Module* m)
  {
    if(!ModuleMemory(*m, 0))
      AppendError(env, env.errors, m, ERR_INVALID_MEMORY_INDEX, "[%u] No default linear memory in module.", ins.line);
    if((1ULL << align) > sizeof(T))
      AppendError(env, env.errors, m, ERR_INVALID_MEMORY_ALIGNMENT,
                  "[%u] Alignment of %u exceeds number of accessed bytes %i", ins.line, (1 << align), sizeof(T));
    ValidatePopType(ins, values, TE_i32, env, m);
    values.Push(PUSH);
  }

  template<typename T, WASM_TYPE_ENCODING POP>
  void ValidateStore(const Instruction& ins, varuint32 align, Stack<varsint7>& values, Environment& env, Module* m)
  {
    if(!ModuleMemory(*m, 0))
      AppendError(env, env.errors, m, ERR_INVALID_MEMORY_INDEX, "[%u] No default linear memory in module.", ins.line);
    if((1ULL << align) > sizeof(T))
      AppendError(env, env.errors, m, ERR_INVALID_MEMORY_ALIGNMENT,
                  "[%u] Alignment of %u exceeds number of accessed bytes %i", ins.line, (1 << align), sizeof(T));
    ValidatePopType(ins, values, POP, env, m);
    ValidatePopType(ins, values, TE_i32, env, m);
  }

  template<WASM_TYPE_ENCODING ARG1, WASM_TYPE_ENCODING RESULT>
  void ValidateUnaryOp(const Instruction& ins, Stack<varsint7>& values, Environment& env, Module* m)
  {
    ValidatePopType(ins, values, ARG1, env, m);
    values.Push(RESULT);
  }

  template<WASM_TYPE_ENCODING ARG1, WASM_TYPE_ENCODING ARG2, WASM_TYPE_ENCODING RESULT>
  void ValidateBinaryOp(const Instruction& ins, Stack<varsint7>& values, Environment& env, Module* m)
  {
    ValidatePopType(ins, values, ARG2, env, m);
    ValidatePopType(ins, values, ARG1, env, m);
    values.Push(RESULT);
  }

  void ValidateFunctionSig(const Instruction& ins, Stack<varsint7>& values, FunctionType& sig, Environment& env, Module* m)
  {
    for(varuint32 i = sig.n_params; i-- > 0;) // Pop in reverse order
      ValidatePopType(ins, values, sig.params[i], env, m);

    if(sig.n_returns > 1)
      AppendError(env, env.errors, m, ERR_INVALID_FUNCTION_SIG,
                  "[%u] Cannot return more than one value yet, tried to return %i.", ins.line, sig.n_returns);

    for(varuint32 i = 0; i < sig.n_returns; ++i)
      values.Push(sig.returns[i]);
  }

  void ValidateIndirectCall(const Instruction& ins, Stack<varsint7>& values, varuint32 sig, Environment& env, Module* m)
  {
    if(!ModuleTable(*m, 0))
      AppendError(env, env.errors, m, ERR_INVALID_TABLE_INDEX,
                  "[%u] 0 is not a valid table index because there are 0 tables.", ins.line);

    ValidatePopType(ins, values, TE_i32, env, m); // Pop callee
    if(sig < m->type.n_functypes)
      ValidateFunctionSig(ins, values, m->type.functypes[sig], env, m);
    else
      AppendError(env, env.errors, m, ERR_INVALID_FUNCTION_INDEX,
                  "[%u] signature index was %u, which is an invalid function signature index.", ins.line, sig);
  }

  void ValidateCall(const Instruction& ins, Stack<varsint7>& values, varuint32 callee, Environment& env, Module* m)
  {
    FunctionType* sig = ModuleFunction(*m, callee);
    if(sig)
      ValidateFunctionSig(ins, values, *sig, env, m);
    else
      AppendError(env, env.errors, m, ERR_INVALID_FUNCTION_INDEX, "[%u] callee was %u, which is an invalid function index.",
                  ins.line, callee);
  }

  void ValidateInstruction(const Instruction& ins, Stack<varsint7>& values, Stack<internal::ControlBlock>& control,
                           varuint32 n_locals, varsint7* locals, Environment& env, Module* m)
  {
    switch(ins.opcode)
    {
    case OP_unreachable: PolymorphStack(values);
    case OP_nop: break;
    case OP_if: ValidatePopType(ins, values, TE_i32, env, m);
    case OP_block:
    case OP_loop: ValidateBlockSignature(ins, ins.immediates[0]._varsint7, env, m); break;
    case OP_else:
    case OP_end: break;
    case OP_br:
      ValidateBranch(ins, ins.immediates[0]._varuint32, values, control, env, m);
      PolymorphStack(values);
      break;
    case OP_br_if:
      ValidatePopType(ins, values, TE_i32, env, m);
      ValidateBranch(ins, ins.immediates[0]._varuint32, values, control, env, m);
      break;
    case OP_br_table:
      ValidatePopType(ins, values, TE_i32, env, m);
      ValidateBranchTable(ins, ins.immediates[0].n_table, ins.immediates[0].table, ins.immediates[1]._varuint32, values,
                          control, env, m);
      PolymorphStack(values);
      break;
    case OP_return:
    {
      size_t cache = control.Limit();
      control.SetLimit(
        0); // A return statement is an unconditional branch to the end of the function, so we have to validate that branch
      ValidateBranch(ins, control.Size() - 1, values, control, env, m);
      control.SetLimit(cache);

      if(control.Size() > 0)
        ValidateBranchSignature(ins, control[0].sig, values, env, m);
      else
        AppendError(env, env.errors, m, ERR_INVALID_FUNCTION_BODY, "[%u] Empty control stack at return statement.",
                    ins.line);
      PolymorphStack(values);
      break;
    }

    // Call operators
    case OP_call: ValidateCall(ins, values, ins.immediates[0]._varuint32, env, m); break;
    case OP_call_indirect:
      ValidateIndirectCall(ins, values, ins.immediates[0]._varuint32, env, m);
      break;

      // Parametric operators
    case OP_drop: ValidatePopType(ins, values, 0, env, m); break;
    case OP_select:
    {
      ValidatePopType(ins, values, TE_i32, env, m);
      varsint7 type = ValidatePopType(ins, values, 0, env, m); // Pop the first value and get it's type
      ValidatePopType(ins, values, type, env, m);              // Verify the second value has the same type
      values.Push(type);                                       // Push the type
    }
    break;

    // Variable access
    case OP_local_get:
      if(ins.immediates[0]._varuint32 >= n_locals)
        AppendError(env, env.errors, m, ERR_INVALID_LOCAL_INDEX, "[%u] Invalid local index for get_local.", ins.line);
      else
        values.Push(locals[ins.immediates[0]._varuint32]);
      break;
    case OP_local_set:
      if(ins.immediates[0]._varuint32 >= n_locals)
      {
        AppendError(env, env.errors, m, ERR_INVALID_LOCAL_INDEX, "[%u] Invalid local index for set_local.", ins.line);
        ValidatePopType(ins, values, 0, env, m);
      }
      else
        ValidatePopType(ins, values, locals[ins.immediates[0]._varuint32], env, m);
      break;
    case OP_local_tee:
      if(ins.immediates[0]._varuint32 >= n_locals)
      {
        AppendError(env, env.errors, m, ERR_INVALID_LOCAL_INDEX, "[%u] Invalid local index for set_local.", ins.line);
        ValidatePopType(ins, values, 0, env, m);
      }
      else
      {
        ValidatePopType(ins, values, locals[ins.immediates[0]._varuint32], env, m);
        values.Push(locals[ins.immediates[0]._varuint32]);
      }
      break;
    case OP_global_get:
    {
      GlobalDesc* desc = ModuleGlobal(*m, ins.immediates[0]._varuint32);
      if(!desc)
        AppendError(env, env.errors, m, ERR_INVALID_GLOBAL_INDEX, "[%u] Invalid global index for get_global.", ins.line);
      else
        values.Push(desc->type);
    }
    break;
    case OP_global_set:
    {
      GlobalDesc* desc = ModuleGlobal(*m, ins.immediates[0]._varuint32);
      if(!desc)
      {
        AppendError(env, env.errors, m, ERR_INVALID_GLOBAL_INDEX, "[%u] Invalid global index for set_global.", ins.line);
        ValidatePopType(ins, values, 0, env, m);
      }
      else if(!desc->mutability)
      {
        AppendError(env, env.errors, m, ERR_IMMUTABLE_GLOBAL, "[%u] Cannot call set_global on an immutable global.",
                    ins.line);
        ValidatePopType(ins, values, 0, env, m);
      }
      else
        ValidatePopType(ins, values, desc->type, env, m);
    }
    break;

    // Memory-related operators
    case OP_i32_load: ValidateLoad<int32_t, TE_i32>(ins, ins.immediates[0]._varuint32, values, env, m); break;
    case OP_i64_load: ValidateLoad<int64_t, TE_i64>(ins, ins.immediates[0]._varuint32, values, env, m); break;
    case OP_f32_load: ValidateLoad<float, TE_f32>(ins, ins.immediates[0]._varuint32, values, env, m); break;
    case OP_f64_load: ValidateLoad<double, TE_f64>(ins, ins.immediates[0]._varuint32, values, env, m); break;
    case OP_i32_load8_s:
    case OP_i32_load8_u: ValidateLoad<int8_t, TE_i32>(ins, ins.immediates[0]._varuint32, values, env, m); break;
    case OP_i32_load16_s:
    case OP_i32_load16_u: ValidateLoad<int16_t, TE_i32>(ins, ins.immediates[0]._varuint32, values, env, m); break;
    case OP_i64_load8_s:
    case OP_i64_load8_u: ValidateLoad<int8_t, TE_i64>(ins, ins.immediates[0]._varuint32, values, env, m); break;
    case OP_i64_load16_s:
    case OP_i64_load16_u: ValidateLoad<int16_t, TE_i64>(ins, ins.immediates[0]._varuint32, values, env, m); break;
    case OP_i64_load32_s:
    case OP_i64_load32_u: ValidateLoad<int32_t, TE_i64>(ins, ins.immediates[0]._varuint32, values, env, m); break;
    case OP_i32_store: ValidateStore<int32_t, TE_i32>(ins, ins.immediates[0]._varuint32, values, env, m); break;
    case OP_i64_store: ValidateStore<int64_t, TE_i64>(ins, ins.immediates[0]._varuint32, values, env, m); break;
    case OP_f32_store: ValidateStore<float, TE_f32>(ins, ins.immediates[0]._varuint32, values, env, m); break;
    case OP_f64_store: ValidateStore<double, TE_f64>(ins, ins.immediates[0]._varuint32, values, env, m); break;
    case OP_i32_store8: ValidateStore<int8_t, TE_i32>(ins, ins.immediates[0]._varuint32, values, env, m); break;
    case OP_i32_store16: ValidateStore<int16_t, TE_i32>(ins, ins.immediates[0]._varuint32, values, env, m); break;
    case OP_i64_store8: ValidateStore<int8_t, TE_i64>(ins, ins.immediates[0]._varuint32, values, env, m); break;
    case OP_i64_store16: ValidateStore<int16_t, TE_i64>(ins, ins.immediates[0]._varuint32, values, env, m); break;
    case OP_i64_store32: ValidateStore<int32_t, TE_i64>(ins, ins.immediates[0]._varuint32, values, env, m); break;
    case OP_memory_size:
      if(!ModuleMemory(*m, 0))
        AppendError(env, env.errors, m, ERR_INVALID_MEMORY_INDEX, "[%u] No default linear memory in module.", ins.line);
      if(ins.immediates[0]._varuint1 != 0)
        AppendError(env, env.errors, m, ERR_INVALID_RESERVED_VALUE, "[%u] reserved must be 0.", ins.line);
      values.Push(TE_i32);
      break;
    case OP_memory_grow:
      if(!ModuleMemory(*m, 0))
        AppendError(env, env.errors, m, ERR_INVALID_MEMORY_INDEX, "[%u] No default linear memory in module.", ins.line);
      if(ins.immediates[0]._varuint1 != 0)
        AppendError(env, env.errors, m, ERR_INVALID_RESERVED_VALUE, "[%u] reserved must be 0.", ins.line);
      ValidatePopType(ins, values, TE_i32, env, m);
      values.Push(TE_i32);
      break;

      // Constants
    case OP_i32_const: values.Push(TE_i32); break;
    case OP_i64_const: values.Push(TE_i64); break;
    case OP_f32_const: values.Push(TE_f32); break;
    case OP_f64_const:
      values.Push(TE_f64);
      break;

      // Comparison operators
    case OP_i32_eqz: ValidateUnaryOp<TE_i32, TE_i32>(ins, values, env, m); break;
    case OP_i32_eq:
    case OP_i32_ne:
    case OP_i32_lt_s:
    case OP_i32_lt_u:
    case OP_i32_gt_s:
    case OP_i32_gt_u:
    case OP_i32_le_s:
    case OP_i32_le_u:
    case OP_i32_ge_s:
    case OP_i32_ge_u: ValidateBinaryOp<TE_i32, TE_i32, TE_i32>(ins, values, env, m); break;
    case OP_i64_eqz: ValidateUnaryOp<TE_i64, TE_i32>(ins, values, env, m); break;
    case OP_i64_eq:
    case OP_i64_ne:
    case OP_i64_lt_s:
    case OP_i64_lt_u:
    case OP_i64_gt_s:
    case OP_i64_gt_u:
    case OP_i64_le_s:
    case OP_i64_le_u:
    case OP_i64_ge_s:
    case OP_i64_ge_u: ValidateBinaryOp<TE_i64, TE_i64, TE_i32>(ins, values, env, m); break;
    case OP_f32_eq:
    case OP_f32_ne:
    case OP_f32_lt:
    case OP_f32_gt:
    case OP_f32_le:
    case OP_f32_ge: ValidateBinaryOp<TE_f32, TE_f32, TE_i32>(ins, values, env, m); break;
    case OP_f64_eq:
    case OP_f64_ne:
    case OP_f64_lt:
    case OP_f64_gt:
    case OP_f64_le:
    case OP_f64_ge:
      ValidateBinaryOp<TE_f64, TE_f64, TE_i32>(ins, values, env, m);
      break;

      // Numeric operators
    case OP_i32_clz:
    case OP_i32_ctz:
    case OP_i32_popcnt: ValidateUnaryOp<TE_i32, TE_i32>(ins, values, env, m); break;
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
    case OP_i32_rotr: ValidateBinaryOp<TE_i32, TE_i32, TE_i32>(ins, values, env, m); break;
    case OP_i64_clz:
    case OP_i64_ctz:
    case OP_i64_popcnt: ValidateUnaryOp<TE_i64, TE_i64>(ins, values, env, m); break;
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
    case OP_i64_rotr: ValidateBinaryOp<TE_i64, TE_i64, TE_i64>(ins, values, env, m); break;
    case OP_f32_abs:
    case OP_f32_neg:
    case OP_f32_ceil:
    case OP_f32_floor:
    case OP_f32_trunc:
    case OP_f32_nearest:
    case OP_f32_sqrt: ValidateUnaryOp<TE_f32, TE_f32>(ins, values, env, m); break;
    case OP_f32_add:
    case OP_f32_sub:
    case OP_f32_mul:
    case OP_f32_div:
    case OP_f32_min:
    case OP_f32_max:
    case OP_f32_copysign: ValidateBinaryOp<TE_f32, TE_f32, TE_f32>(ins, values, env, m); break;
    case OP_f64_abs:
    case OP_f64_neg:
    case OP_f64_ceil:
    case OP_f64_floor:
    case OP_f64_trunc:
    case OP_f64_nearest:
    case OP_f64_sqrt: ValidateUnaryOp<TE_f64, TE_f64>(ins, values, env, m); break;
    case OP_f64_add:
    case OP_f64_sub:
    case OP_f64_mul:
    case OP_f64_div:
    case OP_f64_min:
    case OP_f64_max:
    case OP_f64_copysign:
      ValidateBinaryOp<TE_f64, TE_f64, TE_f64>(ins, values, env, m);
      break;

      // Conversions
    case OP_i32_wrap_i64: ValidateUnaryOp<TE_i64, TE_i32>(ins, values, env, m); break;
    case OP_i32_trunc_f32_s:
    case OP_i32_trunc_f32_u: ValidateUnaryOp<TE_f32, TE_i32>(ins, values, env, m); break;
    case OP_i32_trunc_f64_s:
    case OP_i32_trunc_f64_u: ValidateUnaryOp<TE_f64, TE_i32>(ins, values, env, m); break;
    case OP_i64_extend_i32_s:
    case OP_i64_extend_i32_u: ValidateUnaryOp<TE_i32, TE_i64>(ins, values, env, m); break;
    case OP_i64_trunc_f32_s:
    case OP_i64_trunc_f32_u: ValidateUnaryOp<TE_f32, TE_i64>(ins, values, env, m); break;
    case OP_i64_trunc_f64_s:
    case OP_i64_trunc_f64_u: ValidateUnaryOp<TE_f64, TE_i64>(ins, values, env, m); break;
    case OP_f32_convert_i32_s:
    case OP_f32_convert_i32_u: ValidateUnaryOp<TE_i32, TE_f32>(ins, values, env, m); break;
    case OP_f32_convert_i64_s:
    case OP_f32_convert_i64_u: ValidateUnaryOp<TE_i64, TE_f32>(ins, values, env, m); break;
    case OP_f32_demote_f64: ValidateUnaryOp<TE_f64, TE_f32>(ins, values, env, m); break;
    case OP_f64_convert_i32_s:
    case OP_f64_convert_i32_u: ValidateUnaryOp<TE_i32, TE_f64>(ins, values, env, m); break;
    case OP_f64_convert_i64_s:
    case OP_f64_convert_i64_u: ValidateUnaryOp<TE_i64, TE_f64>(ins, values, env, m); break;
    case OP_f64_promote_f32:
      ValidateUnaryOp<TE_f32, TE_f64>(ins, values, env, m);
      break;

      // Reinterpretations
    case OP_i32_reinterpret_f32: ValidateUnaryOp<TE_f32, TE_i32>(ins, values, env, m); break;
    case OP_i64_reinterpret_f64: ValidateUnaryOp<TE_f64, TE_i64>(ins, values, env, m); break;
    case OP_f32_reinterpret_i32: ValidateUnaryOp<TE_i32, TE_f32>(ins, values, env, m); break;
    case OP_f64_reinterpret_i64: ValidateUnaryOp<TE_i64, TE_f64>(ins, values, env, m); break;
    default:
      AppendError(env, env.errors, m, ERR_FATAL_UNKNOWN_INSTRUCTION, "[%u] Unknown instruction code %hhu", ins.line,
                  ins.opcode);
    }
  }

  void ValidateEndBlock(const Instruction& ins, internal::ControlBlock block, Stack<varsint7>& values, Environment& env,
                        Module* m, bool restore)
  {
    ValidateSignature(ins, block.sig, values, env, m);

    if(block.sig != TE_void && values.Size() > 0)
    {
      values.Pop();
      if(values.Size() > 0 && values.Peek() != TE_POLY)
        AppendError(env, env.errors, m, ERR_INVALID_VALUE_STACK,
                    "[%u] block signature wanted one value, but found %i values!", ins.line, values.Size());
    }

    // Replace the value stack with the expected signature
    while(values.Size())
      values.Pop();
    if(restore &&
       block.sig != TE_void) // Only restore the block signature if this is an end statement, not an else statement
      values.Push(block.sig);

    values.SetLimit(block.limit); // Reset old limit value
  }

  template<class T, void (*VALIDATE)(const T&, Environment&, Module*)>
  void ValidateSection(const T* a, varuint32 n, Environment& env, Module* m)
  {
    for(uint64_t i = 0; i < n; ++i)
      VALIDATE(a[i], env, m);
  }
}

varsint7 innative::ValidateInitializer(const Instruction& ins, Environment& env, Module* m)
{
  switch(ins.opcode)
  {
  case OP_i32_const: return TE_i32;
  case OP_i64_const: return TE_i64;
  case OP_f32_const: return TE_f32;
  case OP_f64_const: return TE_f64;
  case OP_global_get:
    if(!ModuleGlobal(*m, ins.immediates[0]._varuint32))
      AppendError(env, env.errors, m, ERR_INVALID_LOCAL_INDEX, "[%u] Invalid global index for get_global.", ins.line);
    else if(ins.immediates[0]._varuint32 + m->importsection.memories < m->importsection.globals)
      return m->importsection.imports[ins.immediates[0]._varuint32 + m->importsection.memories].global_desc.type;
    else
      AppendError(env, env.errors, m, ERR_INVALID_GLOBAL_INITIALIZER, "[%u] A get_global initializer must be an import.",
                  ins.line, ins.opcode);
    return TE_i32;
  }

  AppendError(env, env.errors, m, ERR_INVALID_INITIALIZER,
              "[%u] An initializer must be a get_global or const instruction, not %hhu", ins.line, ins.opcode);
  return TE_NONE;
}

void innative::ValidateGlobal(const GlobalDecl& decl, Environment& env, Module* m)
{
  varsint7 type = ValidateInitializer(decl.init, env, m);
  if(type != TE_NONE && type != decl.desc.type)
  {
    char buf[10];
    char buf2[10];
    AppendError(env, env.errors, m, ERR_INVALID_GLOBAL_TYPE,
                "The global initializer has type %s, must be the same as the description type %s.",
                EnumToString(TYPE_ENCODING_MAP, type, buf, 10), EnumToString(TYPE_ENCODING_MAP, decl.desc.type, buf2, 10));
  }
}

void innative::ValidateExport(const Export& e, Environment& env, Module* m)
{
  if(!ValidateIdentifier(e.name))
    AppendError(env, env.errors, m, ERR_INVALID_UTF8_ENCODING, "Identifier not valid UTF8: %s", e.name.str());

  switch(e.kind)
  {
  case WASM_KIND_FUNCTION:
    if(!ModuleFunction(*m, e.index))
      AppendError(env, env.errors, m, ERR_INVALID_FUNCTION_INDEX, "Invalid function index %u", e.index);
    break;
  case WASM_KIND_TABLE:
    if(!ModuleTable(*m, e.index))
      AppendError(env, env.errors, m, ERR_INVALID_TABLE_INDEX, "Invalid table index %u", e.index);
    break;
  case WASM_KIND_MEMORY:
    if(!ModuleMemory(*m, e.index))
      AppendError(env, env.errors, m, ERR_INVALID_MEMORY_INDEX, "Invalid memory index %u", e.index);
    break;
  case WASM_KIND_GLOBAL:
  {
    GlobalDesc* g = ModuleGlobal(*m, e.index);
    if(!g)
      AppendError(env, env.errors, m, ERR_INVALID_GLOBAL_INDEX, "Invalid global index %u", e.index);
    else if(!(env.features & ENV_FEATURE_MUTABLE_GLOBALS) && g->mutability)
      AppendError(env, env.errors, m, ERR_INVALID_GLOBAL_IMPORT_TYPE, "Exported global cannot be mutable.");
    break;
  }
  default:
    AppendError(env, env.errors, m, ERR_FATAL_UNKNOWN_KIND, "The %s export has invalid kind %hhu", e.name.str(), e.kind);
    break;
  }
}

varsint32 innative::EvalInitializerI32(const Instruction& ins, Environment& env, Module* m)
{
  switch(ins.opcode)
  {
  case OP_i32_const: return ins.immediates[0]._varsint32;
  case OP_global_get:
  {
    GlobalDecl* global = 0;
    size_t i           = ins.immediates[0]._varsint32 + m->importsection.memories; // Shift index to globals section
    if(i < m->importsection.globals)
    {
      std::pair<Module*, Export*> p = ResolveExport(env, m->importsection.imports[i]);
      if(!p.second || p.second->kind != WASM_KIND_GLOBAL || p.second->index < p.first->importsection.globals)
        AppendError(env, env.errors, m, ERR_INVALID_GLOBAL_INDEX, "[%u] Invalid global import %u", ins.line,
                    ins.immediates[0]._varsint32);
      else
        return EvalInitializerI32(p.first->global.globals[p.second->index - p.first->importsection.globals].init, env,
                                  p.first);
      break;
    }
    i -= m->importsection.globals;
    if(i < m->global.n_globals)
      global = &m->global.globals[i];

    if(!global)
      AppendError(env, env.errors, m, ERR_INVALID_GLOBAL_INDEX, "[%u] Invalid global index %u", ins.line,
                  ins.immediates[0]._varsint32);
    else
      return EvalInitializerI32(global->init, env, nullptr);
    break;
  }
  case OP_i64_const:
  case OP_f32_const:
  case OP_f64_const:
    AppendError(env, env.errors, m, ERR_INVALID_INITIALIZER_TYPE, "[%u] Expected i32 type but got %s", ins.line,
                OPNAMES[ins.opcode]);
    break;
  default: break; // If this isn't even a valid instruction, don't bother emitting an error because it will be redundant.
  }

  return 0;
}

void innative::ValidateTableOffset(const TableInit& init, Environment& env, Module* m)
{
  varsint7 type = ValidateInitializer(init.offset, env, m);
  if(type != TE_NONE && type != TE_i32)
  {
    char buf[10];
    AppendError(env, env.errors, m, ERR_INVALID_TABLE_TYPE,
                "Expected table offset instruction type of i32, got %s instead.",
                EnumToString(TYPE_ENCODING_MAP, type, buf, 10));
  }

  TableDesc* table = ModuleTable(*m, init.index);
  if(!table)
    AppendError(env, env.errors, m, ERR_INVALID_TABLE_INDEX, "Invalid table index %u", init.index);
  else if(table->element_type == TE_funcref)
  {
    if(type != TE_i32) // We cannot verify the offset if it's the wrong type
      return;

    // If this is imported, we use it's actual minimum value instead of the one we claim it has
    if(init.index + m->importsection.functions < m->importsection.tables)
    {
      auto pair = ResolveExport(env, m->importsection.imports[init.index + m->importsection.functions]);
      if(!pair.first || !pair.second || pair.second->kind != WASM_KIND_TABLE ||
         !(table = ModuleTable(*pair.first, pair.second->index)))
        AppendError(env, env.errors, m, ERR_INVALID_TABLE_INDEX, "Could not resolve table import %u", init.index);
    }

    varsint32 offset = EvalInitializerI32(init.offset, env, m);
    if(offset < 0 || offset + init.n_elements > table->resizable.minimum)
      AppendError(env, env.errors, m, ERR_INVALID_TABLE_OFFSET,
                  "Offset (%i) plus element count (%u) exceeds minimum table length (%u)", offset, init.n_elements,
                  table->resizable.minimum);

    for(varuint32 i = 0; i < init.n_elements; ++i)
      if(!ModuleFunction(*m, init.elements[i]))
        AppendError(env, env.errors, m, ERR_INVALID_FUNCTION_INDEX, "Invalid element initializer %u function index: %u", i,
                    init.elements[i]);
  }
  else
  {
    char buf[10];
    AppendError(env, env.errors, m, ERR_INVALID_TABLE_ELEMENT_TYPE, "Invalid table element type %s",
                EnumToString(TYPE_ENCODING_MAP, table->element_type, buf, 10));
  }
}

void innative::ValidateFunctionBody(const FunctionType& sig, const FunctionBody& body, Environment& env, Module* m)
{
  Instruction* cur = body.body;
  Stack<internal::ControlBlock> control; // control-flow stack that must be closed by end instructions
  Stack<varsint7> values;                // Current stack of value types
  varsint7 ret = TE_void;
  if(sig.n_returns > 1) // This is already an invalid function so don't pollute the output with more errors.
    return;

  if(sig.n_returns > 0)
    ret = sig.returns[0];

  // Calculate function locals
  if(sig.n_params > (std::numeric_limits<uint32_t>::max() - body.local_size))
  {
    AppendError(env, env.errors, m, ERR_FATAL_TOO_MANY_LOCALS, "n_local + n_params exceeds the max value of uint32!");
    return;
  }
  varuint32 n_local = sig.n_params + body.local_size;

  varsint7* locals = tmalloc<varsint7>(env, n_local);
  if(locals)
    tmemcpy<varsint7>(locals, n_local, sig.params, sig.n_params);
  else if(n_local > 0)
    abort(); // Out of memory, abort execution

  n_local = sig.n_params;
  for(varuint32 i = 0; i < body.n_locals; ++i)
    for(varuint32 j = 0; j < body.locals[i].count; ++j)
      locals[n_local++] = body.locals[i].type;

  control.Push({ values.Limit(), ret, OP_block }); // Push the function body block with the function signature

  if(!body.n_body)
    return AppendError(env, env.errors, m, ERR_INVALID_FUNCTION_BODY, "Cannot have an empty function body!");

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
        AppendError(env, env.errors, m, ERR_INVALID_FUNCTION_BODY, "Mismatched end instruction at index %u!", i);
      else
      {
        char buf[10];
        if(control.Peek().type == OP_if && control.Peek().sig != TE_void)
          AppendError(env, env.errors, m, ERR_INVALID_BLOCK_SIGNATURE,
                      "If statement without else cannot have a non-void block signature, had %s.",
                      EnumToString(TYPE_ENCODING_MAP, control.Peek().sig, buf, 10));
        ValidateEndBlock(cur[i], control.Pop(), values, env, m, true);
      }
      break;
    case OP_else:
      if(!control.Size())
        AppendError(env, env.errors, m, ERR_INVALID_FUNCTION_BODY, "Mismatched else instruction at index %u!", i);
      else
      {
        char buf[10];
        internal::ControlBlock block = control.Pop();
        if(block.type != OP_if)
          AppendError(env, env.errors, m, ERR_INVALID_FUNCTION_BODY,
                      "Expected else instruction to terminate if block, but found %s instead.",
                      EnumToString(TYPE_ENCODING_MAP, block.type, buf, 10));
        ValidateEndBlock(cur[i], block, values, env, m, false);
        control.Push(
          { values.Limit(), block.sig, OP_else }); // Push a new else block that must be terminated by an end instruction
        values.SetLimit(values.Size() + values.Limit());
      }
    }
  }

  for(varuint32 i = 0; i < sig.n_returns; ++i)
    ValidatePopType(Instruction{ 0, 0, body.line, body.column }, values, sig.returns[i], env, m);

  if(control.Size() > 0)
    AppendError(env, env.errors, m, ERR_INVALID_FUNCTION_BODY, "Control stack not fully terminated, off by %zu",
                control.Size());

  if(values.Size() > 0 || values.Limit() > 0)
    AppendError(env, env.errors, m, ERR_INVALID_VALUE_STACK, "Value stack not fully empty, off by %zu",
                values.Size() + values.Limit());

  if(cur[body.n_body - 1].opcode != OP_end)
    AppendError(env, env.errors, m, ERR_INVALID_FUNCTION_BODY,
                "Expected end instruction to terminate function body, got %hhu instead.", cur[body.n_body - 1].opcode);
}

void innative::ValidateDataOffset(const DataInit& init, Environment& env, Module* m)
{
  varsint7 type = ValidateInitializer(init.offset, env, m);
  if(type != TE_NONE && type != TE_i32)
  {
    char buf[10];
    AppendError(env, env.errors, m, ERR_INVALID_MEMORY_TYPE,
                "Expected memory offset instruction type of i32, got %s instead.",
                EnumToString(TYPE_ENCODING_MAP, type, buf, 10));
  }

  MemoryDesc* memory = ModuleMemory(*m, init.index);
  if(!memory)
  {
    AppendError(env, env.errors, m, ERR_INVALID_MEMORY_INDEX, "Invalid memory index %u", init.index);
    memory = ModuleMemory(*m, init.index);
  }
  else
  {
    // If this is imported, we use it's actual minimum value instead of the one we claim it has
    if(init.index + m->importsection.tables < m->importsection.memories)
    {
      auto pair = ResolveExport(env, m->importsection.imports[init.index + m->importsection.tables]);
      if(!pair.first || !pair.second || pair.second->kind != WASM_KIND_MEMORY ||
         !(memory = ModuleMemory(*pair.first, pair.second->index)))
        AppendError(env, env.errors, m, ERR_INVALID_MEMORY_INDEX, "Could not resolve memory import %u", init.index);
    }

    varsint32 offset = EvalInitializerI32(init.offset, env, m);
    if(offset < 0 || offset + (uint64_t)init.data.size() > (((uint64_t)memory->limits.minimum) << 16))
      AppendError(env, env.errors, m, ERR_INVALID_DATA_SEGMENT,
                  "Offset (%i) plus element count (%u) exceeds minimum memory length (%llu)", offset, init.data.size(),
                  (((uint64_t)memory->limits.minimum) << 16));
  }
}

void innative::ValidateImportOrder(Module& m)
{
  int kind = 0;
  for(varuint32 i = 0; i < m.importsection.n_import; ++i)
  {
    // This is a debug operation that ensures we haven't forgotten to sort imports at some point
    assert(kind <= m.importsection.imports[i].kind);
    kind = m.importsection.imports[i].kind;
  }
}

void innative::ValidateModule(Environment& env, Module& m)
{
  if(m.magic_cookie != WASM_MAGIC_COOKIE)
    AppendError(env, env.errors, &m, ERR_PARSE_INVALID_MAGIC_COOKIE, "Module magic cookie is corrupted.");
  if(m.version != WASM_MAGIC_VERSION)
    AppendError(env, env.errors, &m, ERR_PARSE_INVALID_VERSION, "Module has an unrecognized WebAssembly version.");
  if(!m.name.size())
    AppendError(env, env.errors, &m, ERR_PARSE_INVALID_NAME, "Module has no name!");

  ValidateImportOrder(m);

  if(ModuleTable(m, 1) != nullptr)
    AppendError(env, env.errors, &m, ERR_MULTIPLE_TABLES, "Cannot have more than 1 table defined.");
  if(ModuleMemory(m, 1) != nullptr)
    AppendError(env, env.errors, &m, ERR_MULTIPLE_MEMORIES, "Cannot have more than 1 memory defined.");

  if(m.knownsections & (1 << WASM_SECTION_TYPE))
    ValidateSection<FunctionType, &ValidateFunctionSig>(m.type.functypes, m.type.n_functypes, env, &m);

  if(m.knownsections & (1 << WASM_SECTION_IMPORT))
    ValidateSection<Import, &ValidateImport>(m.importsection.imports, m.importsection.n_import, env, &m);

  if(m.knownsections & (1 << WASM_SECTION_FUNCTION))
  {
    ValidateSection<FunctionDesc, &ValidateFunction>(m.function.funcdecl, m.function.n_funcdecl, env, &m);

    if(m.function.n_funcdecl != m.code.n_funcbody)
      AppendError(env, env.errors, &m, ERR_FUNCTION_BODY_MISMATCH,
                  "The number of function declarations (%u) does not equal the number of function bodies (%u)",
                  m.function.n_funcdecl, m.code.n_funcbody);
  }

  if(m.knownsections & (1 << WASM_SECTION_TABLE))
    ValidateSection<TableDesc, &ValidateTable>(m.table.tables, m.table.n_tables, env, &m);

  if(m.knownsections & (1 << WASM_SECTION_MEMORY))
    ValidateSection<MemoryDesc, &ValidateMemory>(m.memory.memories, m.memory.n_memories, env, &m);

  if(m.knownsections & (1 << WASM_SECTION_GLOBAL))
    ValidateSection<GlobalDecl, &ValidateGlobal>(m.global.globals, m.global.n_globals, env, &m);

  if(m.knownsections & (1 << WASM_SECTION_EXPORT))
    ValidateSection<Export, &ValidateExport>(m.exportsection.exports, m.exportsection.n_exports, env, &m);

  if(m.knownsections & (1 << WASM_SECTION_START))
  {
    FunctionType* f = ModuleFunction(m, m.start);
    if(f)
    {
      if(f->n_params > 0 || f->n_returns > 0)
        AppendError(
          env, env.errors, &m, ERR_INVALID_START_FUNCTION,
          "Starting function must have no parameters and no return value, instead it has %u parameters and %u return values.",
          f->n_params, f->n_returns);
    }
    else
      AppendError(env, env.errors, &m, ERR_INVALID_FUNCTION_INDEX, "Start module function index %u does not exist.",
                  m.start);
  }

  if(m.knownsections & (1 << WASM_SECTION_ELEMENT))
    ValidateSection<TableInit, &ValidateTableOffset>(m.element.elements, m.element.n_elements, env, &m);

  if(m.knownsections & (1 << WASM_SECTION_CODE))
  {
    for(varuint32 j = 0; j < m.code.n_funcbody; ++j)
    {
      if(m.function.funcdecl[j].type_index < m.type.n_functypes)
        ValidateFunctionBody(m.type.functypes[m.function.funcdecl[j].type_index], m.code.funcbody[j], env, &m);
    }
  }

  if(m.knownsections & (1 << WASM_SECTION_DATA))
    ValidateSection<DataInit, &ValidateDataOffset>(m.data.data, m.data.n_data, env, &m);
}

// Performs all post-load validation that couldn't be done during parsing
void innative::ValidateEnvironment(Environment& env)
{
  AppendIntrinsics(env);

  for(size_t i = 0; i < env.n_modules; ++i)
    ValidateModule(env, env.modules[i]);
}

bool innative::ValidateSectionOrder(const uint32& sections, varuint7 opcode) { return (sections & ((~0) << opcode)) == 0; }
