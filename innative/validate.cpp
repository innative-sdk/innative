// Copyright (c)2021 Fundament Software
// For conditions of distribution and use, see copyright notice in innative.h

#include "validate.h"
#include "utility.h"
#include "stack.h"
#include "link.h"
#include "atomic_instructions.h"
#include <stdio.h>
#include <stdarg.h>
#include <atomic>
#include <limits>
#include <inttypes.h>

using namespace innative;
using namespace utility;

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
    if(!(env.features & ENV_FEATURE_MULTI_VALUE) && sig.n_returns > 1)
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
      std::string name    = ABIMangle(CanonImportName(imp, env.system), env.abi, env.arch, GetCallingConvention(imp),
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
      else if(imp.mem_desc.limits.flags & WASM_LIMIT_SHARED) // SHARED && !HAS_MAXIMUM
      {
        AppendError(env, env.errors, m, ERR_SHARED_MEMORY_MAXIMUM_MISSING, "Shared memory must have maximum");
      }

      if((imp.mem_desc.limits.flags & WASM_LIMIT_SHARED) ^ (mem->limits.flags & WASM_LIMIT_SHARED))
      {
        AppendError(env, env.errors, m, ERR_SHARED_MEMORY_MISMATCH,
                    "Imported memory shared flag must match exported memory shared flag");
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
  if((mem.limits.flags & WASM_LIMIT_SHARED) && !(mem.limits.flags & WASM_LIMIT_HAS_MAXIMUM))
    AppendError(env, env.errors, m, ERR_SHARED_MEMORY_MAXIMUM_MISSING, "Shared memory must have maximum");
}

namespace innative {
  struct CtrlFrame
  {
    uint8_t op;
    varsint64 sig;
    size_t height;
    bool unreachable;
  };

  struct FrameValues
  {
    Module* m;
    varsint64 sig;
    bool is_end;

    varuint32 Size() const { return is_end ? GetBlockSigResults(sig, *m) : GetBlockSigParams(sig, *m); }
    varsint7 Get(varuint32 i) const { return is_end ? GetBlockSigResult(sig, i, *m) : GetBlockSigParam(sig, i, *m); }

    bool operator==(const FrameValues& other) const { return m == other.m && sig == other.sig && is_end == other.is_end; }
    bool operator!=(const FrameValues& other) const { return !operator==(other); }

    bool SigEq(const FrameValues& other)
    {
      // Sig reference equality
      if(*this == other)
        return true;

      // Value equality
      auto sz = Size();
      if(sz != other.Size())
        return false;
      for(varuint32 i = 0; i < sz; ++i)
        if(Get(i) != other.Get(i))
          return false;
      return true;
    }

    struct iter;
    struct riter;

    iter begin() const;
    iter end() const;
    riter rbegin() const;
    riter rend() const;
  };

  struct FrameValues::iter
  {
    FrameValues fv;
    varuint32 i;

    varsint7 operator*() const { return fv.Get(i); }
    iter& operator++()
    {
      i++;
      return *this;
    }
    iter operator++(int)
    {
      auto old = *this;
      operator++();
      return old;
    }
    bool operator==(const iter& other) const { return fv == other.fv && i == other.i; }
    bool operator!=(const iter& other) const { return !operator==(other); }
  };
  struct FrameValues::riter
  {
    FrameValues fv;
    varuint32 i;

    varsint7 operator*() const { return fv.Get(i - 1); }
    riter& operator++()
    {
      i--;
      return *this;
    }
    riter operator++(int)
    {
      auto old = *this;
      operator++();
      return old;
    }
    bool operator==(const riter& other) const { return fv == other.fv && i == other.i; }
    bool operator!=(const riter& other) const { return !operator==(other); }
  };

  FrameValues::iter FrameValues::begin() const { return { *this, 0 }; }
  FrameValues::iter FrameValues::end() const { return { *this, Size() }; }
  FrameValues::riter FrameValues::rbegin() const { return { *this, Size() }; }
  FrameValues::riter FrameValues::rend() const { return { *this, 0 }; }

  struct ValidationStack
  {
    Environment& env;
    Module* m;
    Stack<varsint7> opds;
    Stack<CtrlFrame> ctrls;

    ValidationStack(Environment& envi, Module* mod) : env(envi), m(mod) {}

    void PushOpd(varsint7 type) { opds.Push(type); }

    varsint7 PopOpd(const Instruction& ins)
    {
      if(ctrls.Size() && opds.Size() == ctrls[0].height && ctrls[0].unreachable)
        return TE_UNKNOWN;
      else if(ctrls.Size() && opds.Size() == ctrls[0].height)
        AppendError(env, env.errors, m, ERR_EMPTY_VALUE_STACK, "[%u] Expected a value on the stack, but stack was empty.",
                    ins.line);
      else if(opds.Size())
        return opds.Pop();
      return 0;
    }

    varsint7 PopOpd(const Instruction& ins, varsint7 expected)
    {
      auto actual = PopOpd(ins);
      if(actual == TE_UNKNOWN)
        return expected;
      if(expected == TE_UNKNOWN)
        return actual;
      if(expected != 0 && actual != expected)
      {
        if(expected == TE_cref && (actual == TE_i32 || actual == TE_i64)) // Special case for imaginary cref type
          return actual;
        char buf[10];
        char buf2[10];
        AppendError(env, env.errors, m, ERR_INVALID_TYPE, "[%u] Expected %s on the stack, but found %s.", ins.line,
                    EnumToString(TYPE_ENCODING_MAP, expected, buf, 10), EnumToString(TYPE_ENCODING_MAP, actual, buf2, 10));
      }
      return actual;
    }

    template<typename C = std::initializer_list<varsint7>> inline void PushOpds(const C& types)
    {
      for(auto t : types)
        PushOpd((varsint7)t);
    }

    template<typename C = std::initializer_list<varsint7>> inline void PopOpds(const Instruction& ins, const C& types)
    {
      for(auto it = std::rbegin(types); it != std::rend(types); ++it)
      {
        PopOpd(ins, (varsint7)*it);
      }
    }

    template<typename POP = std::initializer_list<varsint7>, typename PUSH = std::initializer_list<varsint7>>
    inline void Op(const Instruction& ins, const POP& pop, const PUSH& push)
    {
      PopOpds(ins, pop);
      PushOpds(push);
    }

    FrameValues StartTypes(varsint64 sig) { return { m, sig, false }; }
    FrameValues EndTypes(varsint64 sig) { return { m, sig, true }; }
    FrameValues StartTypes(const CtrlFrame& frame) { return StartTypes(frame.sig); }
    FrameValues EndTypes(const CtrlFrame& frame) { return EndTypes(frame.sig); }
    FrameValues LabelTypes(const CtrlFrame& frame) { return { m, frame.sig, frame.op != OP_loop }; }

    void PushCtrl(uint8_t op, varsint64 sig)
    {
      ctrls.Push({ op, sig, opds.Size(), false });
      PushOpds(StartTypes(sig));
    }

    CtrlFrame PopCtrl(const Instruction& ins)
    {
      if(ctrls.Size() < 1)
      {
        AppendError(env, env.errors, m, ERR_INVALID_TYPE, "[%u] Invalid control stack", ins.line);
        return { 0 };
      }

      auto& frame = ctrls[0];
      PopOpds(ins, EndTypes(frame));
      if(opds.Size() != frame.height)
        AppendError(env, env.errors, m, ERR_INVALID_VALUE_STACK, "[%u] Invalid value stack", ins.line);
      return ctrls.Pop();
    }

    void Unreachable()
    {
      opds.Resize(ctrls[0].height);
      ctrls[0].unreachable = true;
    }
  };

  template<typename T, WASM_TYPE_ENCODING PUSH>
  void ValidateLoad(const Instruction& ins, varuint32 align, ValidationStack& vstack, Environment& env, Module* m)
  {
    if(!ModuleMemory(*m, ins.immediates[2]._varuint32))
      AppendError(env, env.errors, m, ERR_INVALID_MEMORY_INDEX, "[%u] No default linear memory in module.", ins.line);
    if((1ULL << align) > sizeof(T))
      AppendError(env, env.errors, m, ERR_INVALID_MEMORY_ALIGNMENT,
                  "[%u] Alignment of %u exceeds number of accessed bytes %i", ins.line, (1 << align), sizeof(T));
    vstack.Op(ins, { TE_i32 }, { PUSH });
  }

  template<typename T, WASM_TYPE_ENCODING POP>
  void ValidateStore(const Instruction& ins, varuint32 align, ValidationStack& vstack, Environment& env, Module* m)
  {
    if(!ModuleMemory(*m, ins.immediates[2]._varuint32))
      AppendError(env, env.errors, m, ERR_INVALID_MEMORY_INDEX, "[%u] No default linear memory in module.", ins.line);
    if((1ULL << align) > sizeof(T))
      AppendError(env, env.errors, m, ERR_INVALID_MEMORY_ALIGNMENT,
                  "[%u] Alignment of %u exceeds number of accessed bytes %i", ins.line, (1 << align), sizeof(T));
    vstack.PopOpds(ins, { TE_i32, POP });
  }

  template<WASM_TYPE_ENCODING ARG1, WASM_TYPE_ENCODING RESULT>
  void ValidateUnaryOp(const Instruction& ins, ValidationStack& vstack)
  {
    vstack.Op(ins, { ARG1 }, { RESULT });
  }

  template<WASM_TYPE_ENCODING ARG1, WASM_TYPE_ENCODING ARG2, WASM_TYPE_ENCODING RESULT>
  void ValidateBinaryOp(const Instruction& ins, ValidationStack& vstack)
  {
    vstack.Op(ins, { ARG1, ARG2 }, { RESULT });
  }

  void ValidateFunctionSig(const Instruction& ins, ValidationStack& vstack, FunctionType& sig, Environment& env, Module* m)
  {
    for(varuint32 i = sig.n_params; i--;) // Pop in reverse order
      vstack.PopOpd(ins, sig.params[i]);

    if(!(env.features & ENV_FEATURE_MULTI_VALUE) && sig.n_returns > 1)
      AppendError(env, env.errors, m, ERR_INVALID_FUNCTION_SIG,
                  "[%u] Cannot return more than one value yet, tried to return %i.", ins.line, sig.n_returns);

    for(varuint32 i = 0; i < sig.n_returns; ++i)
      vstack.PushOpd(sig.returns[i]);
  }

  void ValidateIndirectCall(const Instruction& ins, ValidationStack& vstack, varuint32 sig, Environment& env, Module* m)
  {
    if(!ModuleTable(*m, 0))
      AppendError(env, env.errors, m, ERR_INVALID_TABLE_INDEX,
                  "[%u] 0 is not a valid table index because there are 0 tables.", ins.line);

    vstack.PopOpd(ins, TE_i32); // Pop callee
    if(sig < m->type.n_functypes)
      ValidateFunctionSig(ins, vstack, m->type.functypes[sig], env, m);
    else
      AppendError(env, env.errors, m, ERR_INVALID_FUNCTION_INDEX,
                  "[%u] signature index was %u, which is an invalid function signature index.", ins.line, sig);
  }

  void ValidateCall(const Instruction& ins, ValidationStack& vstack, varuint32 callee, Environment& env, Module* m)
  {
    FunctionType* sig = ModuleFunction(*m, callee);
    if(sig)
      ValidateFunctionSig(ins, vstack, *sig, env, m);
    else
      AppendError(env, env.errors, m, ERR_INVALID_FUNCTION_INDEX, "[%u] callee was %u, which is an invalid function index.",
                  ins.line, callee);
  }

  void ValidateMiscOp(const Instruction& ins, ValidationStack& vstack, Environment& env, Module* m)
  {
    switch(ins.opcode[1])
    {
    case OP_i32_trunc_sat_f32_s:
    case OP_i32_trunc_sat_f32_u:
      vstack.PopOpd(ins, TE_f32);
      vstack.PushOpd(TE_i32);
      break;
    case OP_i32_trunc_sat_f64_s:
    case OP_i32_trunc_sat_f64_u:
      vstack.PopOpd(ins, TE_f64);
      vstack.PushOpd(TE_i32);
      break;
    case OP_i64_trunc_sat_f32_s:
    case OP_i64_trunc_sat_f32_u:
      vstack.PopOpd(ins, TE_f32);
      vstack.PushOpd(TE_i64);
      break;
    case OP_i64_trunc_sat_f64_s:
    case OP_i64_trunc_sat_f64_u:
      vstack.PopOpd(ins, TE_f64);
      vstack.PushOpd(TE_i64);
      break;

    case OP_memory_init:
    {
      varuint32 dst_mem = ins.immediates[0]._varuint32;
      varuint32 src_seg = ins.immediates[1]._varuint32;

      if(!ModuleMemory(*m, dst_mem))
        AppendError(env, env.errors, m, ERR_INVALID_MEMORY_INDEX, "[%u] No default linear memory in module.", ins.line);

      if(!(m->knownsections & (1 << WASM_SECTION_DATA_COUNT)))
        AppendError(env, env.errors, m, ERR_MISSING_DATA_COUNT_SECTION, "[%u] missing DataCount section", ins.line);
      else if(src_seg >= m->data_count.count)
        AppendError(env, env.errors, m, ERR_INVALID_DATA_INDEX, "[%u] data index out of bounds", ins.line);

      vstack.PopOpds(ins, { TE_i32, TE_i32, TE_i32 });
      break;
    }
    case OP_data_drop:
    {
      varuint32 segment = ins.immediates[0]._varuint32;

      if(!(m->knownsections & (1 << WASM_SECTION_DATA_COUNT)))
        AppendError(env, env.errors, m, ERR_MISSING_DATA_COUNT_SECTION, "[%u] missing DataCount section", ins.line);
      else if(segment >= m->data_count.count)
        AppendError(env, env.errors, m, ERR_INVALID_DATA_INDEX, "[%u] data index out of bounds", ins.line);
      break;
    }
    case OP_memory_copy:
    {
      varuint32 dst_mem = ins.immediates[0]._varuint32;
      varuint32 src_mem = ins.immediates[1]._varuint32;

      if(!ModuleMemory(*m, dst_mem) || !ModuleMemory(*m, src_mem))
        AppendError(env, env.errors, m, ERR_INVALID_MEMORY_INDEX, "[%u] No default linear memory in module.", ins.line);

      vstack.PopOpds(ins, { TE_i32, TE_i32, TE_i32 });
      break;
    }
    case OP_memory_fill:
    {
      varuint32 mem = ins.immediates[0]._varuint32;

      if(!ModuleMemory(*m, mem))
        AppendError(env, env.errors, m, ERR_INVALID_MEMORY_INDEX, "[%u] No default linear memory in module.", ins.line);

      vstack.PopOpds(ins, { TE_i32, TE_i32, TE_i32 });
      break;
    }
    case OP_table_init:
    {
      varuint32 dst_table = ins.immediates[0]._varuint32;
      varuint32 src_seg   = ins.immediates[1]._varuint32;

      if(!ModuleTable(*m, dst_table))
        AppendError(env, env.errors, m, ERR_INVALID_TABLE_INDEX, "Invalid table index %u", dst_table);
      if(src_seg >= m->element.n_elements)
        AppendError(env, env.errors, m, ERR_INVALID_ELEMENT_INDEX, "Invalid element segment index %u", src_seg);

      vstack.PopOpds(ins, { TE_i32, TE_i32, TE_i32 });
      break;
    }
    case OP_elem_drop:
    {
      varuint32 segment = ins.immediates[0]._varuint32;

      if(segment >= m->element.n_elements)
        AppendError(env, env.errors, m, ERR_INVALID_ELEMENT_INDEX, "Invalid element segment index %u", segment);

      break;
    }
    case OP_table_copy:
    {
      varuint32 dst_table = ins.immediates[0]._varuint32;
      varuint32 src_table = ins.immediates[1]._varuint32;

      if(!ModuleTable(*m, dst_table))
        AppendError(env, env.errors, m, ERR_INVALID_TABLE_INDEX, "Invalid table index %u", dst_table);
      if(!ModuleTable(*m, src_table))
        AppendError(env, env.errors, m, ERR_INVALID_TABLE_INDEX, "Invalid table index %u", src_table);

      vstack.PopOpds(ins, { TE_i32, TE_i32, TE_i32 });
      break;
    }

    default:
      AppendError(env, env.errors, m, ERR_FATAL_UNKNOWN_INSTRUCTION, "[%u] Unknown instruction code %hhu", ins.line,
                  ins.opcode);
    }
  }

  void ValidateAtomicOp(const Instruction& ins, ValidationStack& vstack, Environment& env, Module* m)
  {
    namespace at = innative::atomic_details;

    unsigned correctAlign;
    auto opName                       = OP::NAMES[ins.opcode];
    auto atomicOp                     = ins.opcode[1];
    auto specifiedAlign               = ins.immediates[0]._varuint32;
    auto expectedArgs                 = 3;
    WASM_TYPE_ENCODING returnTy       = TE_i32;
    WASM_TYPE_ENCODING expectedTys[3] = { TE_i32, TE_i32, TE_i32 };

    switch(atomicOp)
    {
    case OP_atomic_notify:
      correctAlign = 2;
      expectedArgs = 2;
      break;
    case OP_atomic_wait32:
      correctAlign   = 2;
      expectedTys[2] = TE_i64;
      break;
    case OP_atomic_wait64:
      correctAlign   = 3;
      expectedTys[1] = TE_i64;
      expectedTys[2] = TE_i64;
      break;

    case OP_atomic_fence:
      correctAlign = 0;       // The fence instruction should have a 0 where the rest of them read `align`
      expectedArgs = 0;       // It takes no arguments
      returnTy     = TE_void; // This instruction pushes nothing onto the stack
      break;

    default:
      // The rest of the valid opcodes are all L/S/RMW ops
      if(!at::IsLSRMWOp(atomicOp))
      {
        AppendError(env, env.errors, m, ERR_FATAL_UNKNOWN_INSTRUCTION, "[%u] Unknown atomic instruction opcode %hhu%hhu",
                    ins.line, ins.opcode[0], ins.opcode[1]);
        return; // The rest of our checks don't make sense for an unknown instruction
      }

      expectedArgs = at::GetArgCount(atomicOp);

      // L/S/RMW ops can have the correct alignment value derived from their opcode
      correctAlign = at::GetValidAlignment(atomicOp);

      // L/S/RMW ops are all the same from here;
      // except the 2 last args and ret are different if it's an i64 op
      if(at::IsI64(at::GetOpType(atomicOp)))
      {
        expectedTys[1] = TE_i64;
        expectedTys[2] = TE_i64;
        returnTy       = TE_i64;
      }

      // Stores don't return anything
      if(at::GetOpGroup(atomicOp) == at::OpGroup::Store)
        returnTy = TE_void;
    }

    if(!ModuleMemory(*m, ins.immediates[2]._varuint32))
      AppendError(env, env.errors, m, ERR_INVALID_MEMORY_INDEX, "[%u] No default linear memory in module.", ins.line);

    if(specifiedAlign != correctAlign)
      AppendError(env, env.errors, m, ERR_INVALID_MEMORY_ALIGNMENT,
                  "[%u] Invalid memory alignment for %s: expected %u, found %u", ins.line, opName, correctAlign,
                  specifiedAlign);

    for(int i = expectedArgs - 1; i >= 0; --i)
      vstack.PopOpd(ins, expectedTys[i]);

    if(returnTy != TE_void)
      vstack.PushOpd(returnTy);
  }

  void ValidateInstruction(const Instruction& ins, ValidationStack& vstack, varuint32 n_locals, varsint7* locals,
                           Environment& env, Module* m)
  {
    switch(ins.opcode[0])
    {
    case OP_unreachable: vstack.Unreachable();
    case OP_nop: break;

    case OP_if: vstack.PopOpd(ins, TE_i32);
    case OP_block:
    case OP_loop:
    {
      auto sig = ins.immediates[0]._varsint64;
      vstack.PopOpds(ins, vstack.StartTypes(sig));
      vstack.PushCtrl(ins.opcode[0], sig);
      break;
    }
    case OP_end:
    {
      auto frame = vstack.PopCtrl(ins);
      if(frame.op == OP_if && !vstack.EndTypes(frame.sig).SigEq(vstack.StartTypes(frame.sig)))
        AppendError(env, env.errors, m, ERR_INVALID_BLOCK_SIGNATURE,
                    "[%u] If statement without else must have matching params and results, had %" PRId64 ".", ins.line,
                    frame.sig);
      vstack.PushOpds(vstack.EndTypes(frame));
      break;
    }
    case OP_else:
    {
      auto frame = vstack.PopCtrl(ins);
      if(frame.op != OP_if)
      {
        char buf[10];
        AppendError(env, env.errors, m, ERR_INVALID_FUNCTION_BODY,
                    "[%u] Expected else instruction to terminate if block, but found %s instead.", ins.line,
                    EnumToString(TYPE_ENCODING_MAP, (int)frame.sig, buf, 10));
      }
      else
      {
        vstack.PushCtrl(OP_else, frame.sig);
      }
      break;
    }
    case OP_br:
    {
      auto depth = ins.immediates[0]._varuint32;
      if(depth >= vstack.ctrls.Size())
      {
        AppendError(env, env.errors, m, ERR_INVALID_BRANCH_DEPTH, "[%u] Invalid branch depth: %u exceeds %zu", depth,
                    vstack.ctrls.Size(), ins.line);
        break;
      }
      vstack.PopOpds(ins, vstack.LabelTypes(vstack.ctrls[depth]));
      vstack.Unreachable();
      break;
    }
    case OP_br_if:
    {
      auto depth = ins.immediates[0]._varuint32;
      if(depth >= vstack.ctrls.Size())
      {
        AppendError(env, env.errors, m, ERR_INVALID_BRANCH_DEPTH, "[%u] Invalid branch depth: %u exceeds %zu", depth,
                    vstack.ctrls.Size(), ins.line);
        break;
      }
      vstack.PopOpd(ins, TE_i32);
      auto types = vstack.LabelTypes(vstack.ctrls[depth]);
      vstack.Op(ins, types, types);
      break;
    }
    case OP_br_table:
    {
      auto n_table = ins.immediates[0].n_table;
      auto table   = ins.immediates[0].table;
      auto depth   = ins.immediates[1]._varuint32;
      if(depth >= vstack.ctrls.Size())
      {
        AppendError(env, env.errors, m, ERR_INVALID_BRANCH_DEPTH, "[%u] Invalid branch depth: %u exceeds %zu", depth,
                    vstack.ctrls.Size(), ins.line);
        break;
      }
      auto def_types = vstack.LabelTypes(vstack.ctrls[depth]);
      for(varuint32 i = 0; i < n_table; ++i)
      {
        auto br_depth = table[i];
        if(br_depth >= vstack.ctrls.Size())
          AppendError(env, env.errors, m, ERR_INVALID_BRANCH_DEPTH, "[%u] Invalid branch depth: %u exceeds %zu", depth,
                      vstack.ctrls.Size(), ins.line);
        else if(!vstack.LabelTypes(vstack.ctrls[br_depth]).SigEq(def_types))
          AppendError(env, env.errors, m, ERR_INVALID_TYPE,
                      "[%u] Branch table target has type signature %" PRId64 ", but default branch has %" PRId64,
                      ins.line,
                      vstack.ctrls[br_depth].sig, vstack.ctrls[depth].sig);
      }
      vstack.PopOpd(ins, TE_i32);
      vstack.PopOpds(ins, def_types);
      vstack.Unreachable();
      break;
    }
    case OP_return:
    {
      if(!vstack.ctrls.Size())
        AppendError(env, env.errors, m, ERR_INVALID_FUNCTION_BODY, "[%u] Empty control stack at return statement.",
                    ins.line);
      else
      {
        auto frame = vstack.ctrls.Back(); // Func base frame
        vstack.PopOpds(ins, vstack.EndTypes(frame));
        vstack.Unreachable();
      }
      break;
    }

    // Call operators
    case OP_call: ValidateCall(ins, vstack, ins.immediates[0]._varuint32, env, m); break;
    case OP_call_indirect:
      ValidateIndirectCall(ins, vstack, ins.immediates[0]._varuint32, env, m);
      break;

      // Parametric operators
    case OP_drop: vstack.PopOpd(ins); break;
    case OP_select:
    {
      vstack.PopOpd(ins, TE_i32);
      auto t1 = vstack.PopOpd(ins);
      auto t2 = vstack.PopOpd(ins, t1);
      vstack.PushOpd(t2);
    }
    break;

    // Variable access
    case OP_local_get:
      if(ins.immediates[0]._varuint32 >= n_locals)
        AppendError(env, env.errors, m, ERR_INVALID_LOCAL_INDEX, "[%u] Invalid local index for get_local.", ins.line);
      else
        vstack.PushOpd(locals[ins.immediates[0]._varuint32]);
      break;
    case OP_local_set:
      if(ins.immediates[0]._varuint32 >= n_locals)
      {
        AppendError(env, env.errors, m, ERR_INVALID_LOCAL_INDEX, "[%u] Invalid local index for set_local.", ins.line);
        vstack.PopOpd(ins);
      }
      else
        vstack.PopOpd(ins, locals[ins.immediates[0]._varuint32]);
      break;
    case OP_local_tee:
      if(ins.immediates[0]._varuint32 >= n_locals)
      {
        AppendError(env, env.errors, m, ERR_INVALID_LOCAL_INDEX, "[%u] Invalid local index for set_local.", ins.line);
        vstack.PopOpd(ins);
      }
      else
      {
        vstack.PopOpd(ins, locals[ins.immediates[0]._varuint32]);
        vstack.PushOpd(locals[ins.immediates[0]._varuint32]);
      }
      break;
    case OP_global_get:
    {
      GlobalDesc* desc = ModuleGlobal(*m, ins.immediates[0]._varuint32);
      if(!desc)
        AppendError(env, env.errors, m, ERR_INVALID_GLOBAL_INDEX, "[%u] Invalid global index for get_global.", ins.line);
      else
        vstack.PushOpd(desc->type);
    }
    break;
    case OP_global_set:
    {
      GlobalDesc* desc = ModuleGlobal(*m, ins.immediates[0]._varuint32);
      if(!desc)
      {
        AppendError(env, env.errors, m, ERR_INVALID_GLOBAL_INDEX, "[%u] Invalid global index for set_global.", ins.line);
        vstack.PopOpd(ins);
      }
      else if(!desc->mutability)
      {
        AppendError(env, env.errors, m, ERR_IMMUTABLE_GLOBAL, "[%u] Cannot call set_global on an immutable global.",
                    ins.line);
        vstack.PopOpd(ins);
      }
      else
        vstack.PopOpd(ins, desc->type);
    }
    break;

    // Memory-related operators
    case OP_i32_load: ValidateLoad<int32_t, TE_i32>(ins, ins.immediates[0]._varuint32, vstack, env, m); break;
    case OP_i64_load: ValidateLoad<int64_t, TE_i64>(ins, ins.immediates[0]._varuint32, vstack, env, m); break;
    case OP_f32_load: ValidateLoad<float, TE_f32>(ins, ins.immediates[0]._varuint32, vstack, env, m); break;
    case OP_f64_load: ValidateLoad<double, TE_f64>(ins, ins.immediates[0]._varuint32, vstack, env, m); break;
    case OP_i32_load8_s:
    case OP_i32_load8_u: ValidateLoad<int8_t, TE_i32>(ins, ins.immediates[0]._varuint32, vstack, env, m); break;
    case OP_i32_load16_s:
    case OP_i32_load16_u: ValidateLoad<int16_t, TE_i32>(ins, ins.immediates[0]._varuint32, vstack, env, m); break;
    case OP_i64_load8_s:
    case OP_i64_load8_u: ValidateLoad<int8_t, TE_i64>(ins, ins.immediates[0]._varuint32, vstack, env, m); break;
    case OP_i64_load16_s:
    case OP_i64_load16_u: ValidateLoad<int16_t, TE_i64>(ins, ins.immediates[0]._varuint32, vstack, env, m); break;
    case OP_i64_load32_s:
    case OP_i64_load32_u: ValidateLoad<int32_t, TE_i64>(ins, ins.immediates[0]._varuint32, vstack, env, m); break;
    case OP_i32_store: ValidateStore<int32_t, TE_i32>(ins, ins.immediates[0]._varuint32, vstack, env, m); break;
    case OP_i64_store: ValidateStore<int64_t, TE_i64>(ins, ins.immediates[0]._varuint32, vstack, env, m); break;
    case OP_f32_store: ValidateStore<float, TE_f32>(ins, ins.immediates[0]._varuint32, vstack, env, m); break;
    case OP_f64_store: ValidateStore<double, TE_f64>(ins, ins.immediates[0]._varuint32, vstack, env, m); break;
    case OP_i32_store8: ValidateStore<int8_t, TE_i32>(ins, ins.immediates[0]._varuint32, vstack, env, m); break;
    case OP_i32_store16: ValidateStore<int16_t, TE_i32>(ins, ins.immediates[0]._varuint32, vstack, env, m); break;
    case OP_i64_store8: ValidateStore<int8_t, TE_i64>(ins, ins.immediates[0]._varuint32, vstack, env, m); break;
    case OP_i64_store16: ValidateStore<int16_t, TE_i64>(ins, ins.immediates[0]._varuint32, vstack, env, m); break;
    case OP_i64_store32: ValidateStore<int32_t, TE_i64>(ins, ins.immediates[0]._varuint32, vstack, env, m); break;
    case OP_memory_size:
      if(!ModuleMemory(*m, ins.immediates[0]._varuint32))
        AppendError(env, env.errors, m, ERR_INVALID_MEMORY_INDEX, "[%u] No default linear memory in module.", ins.line);
      if(!(env.features & ENV_FEATURE_MULTI_MEMORY))
        if(ins.immediates[0]._varuint32 != 0)
          AppendError(env, env.errors, m, ERR_INVALID_RESERVED_VALUE, "[%u] reserved must be 0.", ins.line);
      vstack.PushOpd(TE_i32);
      break;
    case OP_memory_grow:
      if(!ModuleMemory(*m, ins.immediates[0]._varuint32))
        AppendError(env, env.errors, m, ERR_INVALID_MEMORY_INDEX, "[%u] No default linear memory in module.", ins.line);
      if(!(env.features & ENV_FEATURE_MULTI_MEMORY))
        if(ins.immediates[0]._varuint32 != 0)
          AppendError(env, env.errors, m, ERR_INVALID_RESERVED_VALUE, "[%u] reserved must be 0.", ins.line);
      vstack.PopOpd(ins, TE_i32);
      vstack.PushOpd(TE_i32);
      break;

      // Constants
    case OP_i32_const: vstack.PushOpd(TE_i32); break;
    case OP_i64_const: vstack.PushOpd(TE_i64); break;
    case OP_f32_const: vstack.PushOpd(TE_f32); break;
    case OP_f64_const:
      vstack.PushOpd(TE_f64);
      break;

      // Comparison operators
    case OP_i32_eqz: ValidateUnaryOp<TE_i32, TE_i32>(ins, vstack); break;
    case OP_i32_eq:
    case OP_i32_ne:
    case OP_i32_lt_s:
    case OP_i32_lt_u:
    case OP_i32_gt_s:
    case OP_i32_gt_u:
    case OP_i32_le_s:
    case OP_i32_le_u:
    case OP_i32_ge_s:
    case OP_i32_ge_u: ValidateBinaryOp<TE_i32, TE_i32, TE_i32>(ins, vstack); break;
    case OP_i64_eqz: ValidateUnaryOp<TE_i64, TE_i32>(ins, vstack); break;
    case OP_i64_eq:
    case OP_i64_ne:
    case OP_i64_lt_s:
    case OP_i64_lt_u:
    case OP_i64_gt_s:
    case OP_i64_gt_u:
    case OP_i64_le_s:
    case OP_i64_le_u:
    case OP_i64_ge_s:
    case OP_i64_ge_u: ValidateBinaryOp<TE_i64, TE_i64, TE_i32>(ins, vstack); break;
    case OP_f32_eq:
    case OP_f32_ne:
    case OP_f32_lt:
    case OP_f32_gt:
    case OP_f32_le:
    case OP_f32_ge: ValidateBinaryOp<TE_f32, TE_f32, TE_i32>(ins, vstack); break;
    case OP_f64_eq:
    case OP_f64_ne:
    case OP_f64_lt:
    case OP_f64_gt:
    case OP_f64_le:
    case OP_f64_ge:
      ValidateBinaryOp<TE_f64, TE_f64, TE_i32>(ins, vstack);
      break;

      // Numeric operators
    case OP_i32_clz:
    case OP_i32_ctz:
    case OP_i32_popcnt: ValidateUnaryOp<TE_i32, TE_i32>(ins, vstack); break;
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
    case OP_i32_rotr: ValidateBinaryOp<TE_i32, TE_i32, TE_i32>(ins, vstack); break;
    case OP_i64_clz:
    case OP_i64_ctz:
    case OP_i64_popcnt: ValidateUnaryOp<TE_i64, TE_i64>(ins, vstack); break;
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
    case OP_i64_rotr: ValidateBinaryOp<TE_i64, TE_i64, TE_i64>(ins, vstack); break;
    case OP_f32_abs:
    case OP_f32_neg:
    case OP_f32_ceil:
    case OP_f32_floor:
    case OP_f32_trunc:
    case OP_f32_nearest:
    case OP_f32_sqrt: ValidateUnaryOp<TE_f32, TE_f32>(ins, vstack); break;
    case OP_f32_add:
    case OP_f32_sub:
    case OP_f32_mul:
    case OP_f32_div:
    case OP_f32_min:
    case OP_f32_max:
    case OP_f32_copysign: ValidateBinaryOp<TE_f32, TE_f32, TE_f32>(ins, vstack); break;
    case OP_f64_abs:
    case OP_f64_neg:
    case OP_f64_ceil:
    case OP_f64_floor:
    case OP_f64_trunc:
    case OP_f64_nearest:
    case OP_f64_sqrt: ValidateUnaryOp<TE_f64, TE_f64>(ins, vstack); break;
    case OP_f64_add:
    case OP_f64_sub:
    case OP_f64_mul:
    case OP_f64_div:
    case OP_f64_min:
    case OP_f64_max:
    case OP_f64_copysign:
      ValidateBinaryOp<TE_f64, TE_f64, TE_f64>(ins, vstack);
      break;

      // Conversions
    case OP_i32_wrap_i64: ValidateUnaryOp<TE_i64, TE_i32>(ins, vstack); break;
    case OP_i32_trunc_f32_s:
    case OP_i32_trunc_f32_u: ValidateUnaryOp<TE_f32, TE_i32>(ins, vstack); break;
    case OP_i32_trunc_f64_s:
    case OP_i32_trunc_f64_u: ValidateUnaryOp<TE_f64, TE_i32>(ins, vstack); break;
    case OP_i64_extend_i32_s:
    case OP_i64_extend_i32_u: ValidateUnaryOp<TE_i32, TE_i64>(ins, vstack); break;
    case OP_i64_trunc_f32_s:
    case OP_i64_trunc_f32_u: ValidateUnaryOp<TE_f32, TE_i64>(ins, vstack); break;
    case OP_i64_trunc_f64_s:
    case OP_i64_trunc_f64_u: ValidateUnaryOp<TE_f64, TE_i64>(ins, vstack); break;
    case OP_f32_convert_i32_s:
    case OP_f32_convert_i32_u: ValidateUnaryOp<TE_i32, TE_f32>(ins, vstack); break;
    case OP_f32_convert_i64_s:
    case OP_f32_convert_i64_u: ValidateUnaryOp<TE_i64, TE_f32>(ins, vstack); break;
    case OP_f32_demote_f64: ValidateUnaryOp<TE_f64, TE_f32>(ins, vstack); break;
    case OP_f64_convert_i32_s:
    case OP_f64_convert_i32_u: ValidateUnaryOp<TE_i32, TE_f64>(ins, vstack); break;
    case OP_f64_convert_i64_s:
    case OP_f64_convert_i64_u: ValidateUnaryOp<TE_i64, TE_f64>(ins, vstack); break;
    case OP_f64_promote_f32:
      ValidateUnaryOp<TE_f32, TE_f64>(ins, vstack);
      break;

      // Reinterpretations
    case OP_i32_reinterpret_f32: ValidateUnaryOp<TE_f32, TE_i32>(ins, vstack); break;
    case OP_i64_reinterpret_f64: ValidateUnaryOp<TE_f64, TE_i64>(ins, vstack); break;
    case OP_f32_reinterpret_i32: ValidateUnaryOp<TE_i32, TE_f32>(ins, vstack); break;
    case OP_f64_reinterpret_i64: ValidateUnaryOp<TE_i64, TE_f64>(ins, vstack); break;

    case OP_i32_extend8_s:
    case OP_i32_extend16_s: ValidateUnaryOp<TE_i32, TE_i32>(ins, vstack); break;
    case OP_i64_extend8_s:
    case OP_i64_extend16_s:
    case OP_i64_extend32_s:
      ValidateUnaryOp<TE_i64, TE_i64>(ins, vstack);
      break;

      // Bulk memory operations
    case OP_misc_ops_prefix:
      ValidateMiscOp(ins, vstack, env, m);
      break;

      // Atomics
    case OP_atomic_prefix: ValidateAtomicOp(ins, vstack, env, m); break;

    default:
      AppendError(env, env.errors, m, ERR_FATAL_UNKNOWN_INSTRUCTION, "[%u] Unknown instruction code %hhu", ins.line,
                  ins.opcode[0]);
    }
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
  switch(ins.opcode[0])
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
              "[%u] An initializer must be a get_global or const instruction, not %hhu", ins.line, ins.opcode[0]);
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
  switch(ins.opcode[0])
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
                OP::NAMES[ins.opcode]);
    break;
  default: break; // If this isn't even a valid instruction, don't bother emitting an error because it will be redundant.
  }

  return 0;
}

void innative::ValidateTableOffset(const TableInit& init, Environment& env, Module* m)
{
  if(init.flags & WASM_ELEM_INVALID_FLAGS)
  {
    AppendError(env, env.errors, m, ERR_INVALID_ELEMENT_SEGMENT, "Invalid element segment flags %x", init.flags);
    return; // The rest of this validation is junk if the flags are junk
  }

  // Not "Legacy Active"
  if((init.flags & 0b111) != 0)
  {
    if(init.flags & WASM_ELEM_CARRIES_ELEMEXPRS)
    {
      if(init.elem_type != TE_funcref)
        AppendError(env, env.errors, m, ERR_INVALID_TABLE_ELEMENT_TYPE, "Invalid element segment type %d", init.elem_type);
    }
    else
    {
      if(init.extern_kind != 0)
        AppendError(env, env.errors, m, ERR_INVALID_RESERVED_VALUE, "Invalid extern_kind reserved value %d (expected 0)",
                    init.extern_kind);
    }
  }

  // The rest of this validation doesn't matter for passive segments
  if(init.flags & WASM_ELEM_PASSIVE)
    return;

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

    for(varuint32 i = 0; i < init.n_elements; ++i)
    {
      varuint32 idx;
      if(init.flags & WASM_ELEM_CARRIES_ELEMEXPRS)
      {
        if(init.elemexprs[i].opcode[0] == OP_ref_null)
          continue;
        idx = init.elemexprs[i].immediates[0]._varuint32;
      }
      else
        idx = init.elements[i];
      if(!ModuleFunction(*m, idx))
        AppendError(env, env.errors, m, ERR_INVALID_FUNCTION_INDEX, "Invalid element initializer %u function index: %u", i,
                    init.elements[i]);
    }
  }
  else
  {
    char buf[10];
    AppendError(env, env.errors, m, ERR_INVALID_TABLE_ELEMENT_TYPE, "Invalid table element type %s",
                EnumToString(TYPE_ENCODING_MAP, table->element_type, buf, 10));
  }
}

void innative::ValidateFunctionBody(varuint32 idx, const FunctionBody& body, Environment& env, Module* m)
{
  assert(m);
  Instruction* cur = body.body;
  ValidationStack vstack{ env, m };
  const auto& sig = m->type.functypes[idx];
  if(!(env.features & ENV_FEATURE_MULTI_VALUE) && sig.n_returns > 1) // Don't pollute the output with more errors.
    return;

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

  vstack.ctrls.Push({ OP_block, idx, 0, false }); // Push the function body block with the function signature

  if(!body.n_body)
    return AppendError(env, env.errors, m, ERR_INVALID_FUNCTION_BODY, "Cannot have an empty function body!");

  for(varuint32 i = 0; i < body.n_body; ++i)
  {
    ValidateInstruction(cur[i], vstack, n_local, locals, env, m);
  }

  // Validate return values in reverse order
  for(varuint32 i = sig.n_returns; i-- > 0;)
    vstack.PopOpd(Instruction{ 0, 0, body.line, body.column }, sig.returns[i]);

  if(vstack.ctrls.Size() > 0)
    AppendError(env, env.errors, m, ERR_INVALID_FUNCTION_BODY, "Control stack not fully terminated, off by %zu",
                vstack.ctrls.Size());

  if(vstack.opds.Size() > 0)
    AppendError(env, env.errors, m, ERR_INVALID_VALUE_STACK, "Value stack not fully empty, off by %zu", vstack.opds.Size());

  if(cur[body.n_body - 1].opcode[0] != OP_end)
    AppendError(env, env.errors, m, ERR_INVALID_FUNCTION_BODY,
                "Expected end instruction to terminate function body, got %hhu instead.", cur[body.n_body - 1].opcode);
}

void innative::ValidateDataOffset(const DataInit& init, Environment& env, Module* m)
{
  if(init.flags > 2)
    AppendError(env, env.errors, m, ERR_INVALID_DATA_SEGMENT, "Invalid data segment flags %x", init.flags);

  if(init.flags & WASM_DATA_PASSIVE)
  {
    // None of the rest of this validation applies to passive segments
    return;
  }

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

    // Offset out of bounds is now a trap at module start instead of a validation error
    /*varsint32 offset = EvalInitializerI32(init.offset, env, m);
    if(offset < 0 || offset + (uint64_t)init.data.size() > (((uint64_t)memory->limits.minimum) << 16))
      AppendError(env, env.errors, m, ERR_INVALID_DATA_SEGMENT,
                  "Offset (%i) plus element count (%u) exceeds minimum memory length (%llu)", offset, init.data.size(),
                  (((uint64_t)memory->limits.minimum) << 16));*/
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
  if(!(env.features & ENV_FEATURE_MULTI_MEMORY))
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
        ValidateFunctionBody(m.function.funcdecl[j].type_index, m.code.funcbody[j], env, &m);
    }
  }

  if(m.knownsections & (1 << WASM_SECTION_DATA))
  {
    if(m.knownsections & (1 << WASM_SECTION_DATA_COUNT))
    {
      if(m.data.n_data != m.data_count.count)
        AppendError(env, env.errors, &m, ERR_INVALID_DATA_SEGMENT, "Data section count does not match DataCount section");
    }

    ValidateSection<DataInit, &ValidateDataOffset>(m.data.data, m.data.n_data, env, &m);
  }
  else if(m.knownsections & (1 << WASM_SECTION_DATA_COUNT))
  {
    // DataCount but no Data is only a problem if the count is not 0
    if(m.data_count.count != 0)
      AppendError(env, env.errors, &m, ERR_INVALID_DATA_SEGMENT, "Data section count does not match DataCount section");
  }
}

// Performs all post-load validation that couldn't be done during parsing
void innative::ValidateEnvironment(Environment& env)
{
  AppendIntrinsics(env);

  for(size_t i = 0; i < env.n_modules; ++i)
    ValidateModule(env, env.modules[i]);
}

bool innative::ValidateSectionOrder(const uint32& sections, varuint7 opcode)
{
  uint32 mask = 0;
  switch(opcode)
  {
    // Reverse order of the sections, sets which sections are allowed to already be known
  case WASM_SECTION_DATA: mask |= 1 << WASM_SECTION_DATA;
  case WASM_SECTION_CODE: mask |= 1 << WASM_SECTION_CODE;
  case WASM_SECTION_DATA_COUNT: mask |= 1 << WASM_SECTION_DATA_COUNT;
  case WASM_SECTION_ELEMENT: mask |= 1 << WASM_SECTION_ELEMENT;
  case WASM_SECTION_START: mask |= 1 << WASM_SECTION_START;
  case WASM_SECTION_EXPORT: mask |= 1 << WASM_SECTION_EXPORT;
  case WASM_SECTION_GLOBAL: mask |= 1 << WASM_SECTION_GLOBAL;
  case WASM_SECTION_MEMORY: mask |= 1 << WASM_SECTION_MEMORY;
  case WASM_SECTION_TABLE: mask |= 1 << WASM_SECTION_TABLE;
  case WASM_SECTION_FUNCTION: mask |= 1 << WASM_SECTION_FUNCTION;
  case WASM_SECTION_IMPORT: mask |= 1 << WASM_SECTION_IMPORT;
  case WASM_SECTION_TYPE: mask |= 1 << WASM_SECTION_TYPE;
  default: mask |= 1 << WASM_SECTION_CUSTOM;
  }

  return (sections & ~mask) == 0;
}
