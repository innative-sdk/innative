// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "parse.h"
#include "validate.h"
#include "stream.h"
#include "utility.h"
#include "dwarf_parser.h"
#include "serialize.h"
#include <assert.h>
#include <algorithm>
#include <fstream>

using namespace innative;
using namespace utility;
using namespace internal;

namespace innative {
  namespace internal {
    IN_FORCEINLINE IN_ERROR ParseVarUInt32(Stream& s, varuint32& target)
    {
      IN_ERROR err;
      target = static_cast<varuint32>(s.DecodeLEB128(err, 32, false));
      return err;
    }
    IN_FORCEINLINE IN_ERROR ParseVarSInt7(Stream& s, varsint7& target)
    {
      IN_ERROR err;
      target = static_cast<varsint7>(s.DecodeLEB128(err, 7, true));
      return err;
    }
    IN_FORCEINLINE IN_ERROR ParseVarUInt7(Stream& s, varuint7& target)
    {
      IN_ERROR err;
      target = static_cast<varuint7>(s.DecodeLEB128(err, 7, false));
      return err;
    }
    IN_FORCEINLINE IN_ERROR ParseVarUInt1(Stream& s, varuint1& target)
    {
      IN_ERROR err;
      target = static_cast<varuint1>(s.DecodeLEB128(err, 1, false));
      return err;
    }
    IN_FORCEINLINE IN_ERROR ParseByte(Stream& s, uint8_t& target)
    {
      return !s.Read(target) ? ERR_PARSE_UNEXPECTED_EOF : ERR_SUCCESS;
    }

    template<class T, typename... Args> struct Parse
    {
      template<IN_ERROR (*PARSE)(Stream&, T&, Args...)>
      static IN_ERROR Array(Stream& s, T*& ptr, varuint32& size, const Environment& env, Args... args)
      {
        IN_ERROR err = ParseVarUInt32(s, size);
        if(err < 0)
          return err;

        if(!size)
        {
          ptr = 0;
          return err;
        }

        T* r = tmalloc<T>(env, size);
        if(!r)
          return ERR_FATAL_OUT_OF_MEMORY;

        memset(r, 0, sizeof(T) * size);
        ptr = r;
        for(varuint32 i = 0; i < size && err >= 0; ++i)
          err = PARSE(s, r[i], args...);

        return err;
      }
    };

    IN_ERROR ParseFunctionLocal(Stream& s, FunctionLocal& entry)
    {
      entry.debug.line   = 1;
      entry.debug.column = static_cast<decltype(entry.debug.column)>(s.pos);
      IN_ERROR err       = ParseVarUInt32(s, entry.count);

      if(err >= 0)
        err = ParseVarSInt7(s, entry.type);

      return err;
    }
  }
}

IN_ERROR innative::ParseByteArray(Stream& s, ByteArray& section, bool terminator, const Environment& env)
{
  varuint32 n;
  IN_ERROR err = ParseVarUInt32(s, n);
  if(err < 0)
    return err;

  section.resize(n, terminator, env);
  if(n > 0)
  {
    if(!section.get())
      return ERR_FATAL_OUT_OF_MEMORY;

    uint32 bytes = static_cast<uint32>(s.ReadBytes(section.get(), section.size()));
    if(bytes != section.size())
      return ERR_PARSE_UNEXPECTED_EOF;
  }

  return ERR_SUCCESS;
}

IN_ERROR innative::ParseIdentifier(Stream& s, ByteArray& section, const Environment& env)
{
  // For identifiers, we allocate one extra null terminator byte that isn't included in the size
  return ParseByteArray(s, section, true, env);
}

IN_ERROR innative::ParseInitializer(Stream& s, Instruction& ins, const Environment& env)
{
  IN_ERROR err = ParseInstruction(s, ins, env);
  Instruction end;

  if(err >= 0)
    err = ParseInstruction(s, end, env);

  if(err >= 0 && end.opcode != OP_end)
    err = ERR_FATAL_EXPECTED_END_INSTRUCTION;

  return err;
}

IN_ERROR innative::ParseFunctionType(Stream& s, FunctionType& sig, const Environment& env)
{
  IN_ERROR err = ParseVarSInt7(s, sig.form);
  if(err < 0)
    return err;

  if(sig.form == TE_func)
  {
    err = Parse<varsint7>::template Array<&ParseVarSInt7>(s, sig.params, sig.n_params, env);

    if(err >= 0)
      err = Parse<varsint7>::template Array<&ParseVarSInt7>(s, sig.returns, sig.n_returns, env);
  }
  else
    err = ERR_FATAL_UNKNOWN_FUNCTION_SIGNATURE;

  return err;
}

IN_ERROR innative::ParseResizableLimits(Stream& s, ResizableLimits& limits)
{
  IN_ERROR err = ParseVarUInt32(s, limits.flags);

  if(err >= 0)
    err = ParseVarUInt32(s, limits.minimum);

  if(err >= 0 && (limits.flags & 0x1) != 0)
    err = ParseVarUInt32(s, limits.maximum);

  return err;
}

IN_ERROR innative::ParseFunctionDesc(utility::Stream& s, FunctionDesc& desc)
{
  desc.debug.line   = 1;
  desc.debug.column = static_cast<decltype(desc.debug.column)>(s.pos);
  return ParseVarUInt32(s, desc.type_index);
}

IN_ERROR innative::ParseMemoryDesc(Stream& s, MemoryDesc& mem)
{
  mem.debug = { 1, static_cast<decltype(mem.debug.line)>(s.pos) };
  return ParseResizableLimits(s, mem.limits);
}

IN_ERROR innative::ParseTableDesc(Stream& s, TableDesc& t)
{
  t.debug      = { 1, static_cast<decltype(t.debug.line)>(s.pos) };
  IN_ERROR err = ParseVarSInt7(s, t.element_type);

  if(err >= 0)
    err = ParseResizableLimits(s, t.resizable);

  return err;
}

IN_ERROR innative::ParseGlobalDesc(Stream& s, GlobalDesc& g)
{
  g.debug      = { 1, static_cast<decltype(g.debug.line)>(s.pos) };
  IN_ERROR err = ParseVarSInt7(s, g.type);

  if(err >= 0 && (err = ParseVarUInt1(s, g.mutability)))
    return ERR_INVALID_MUTABILITY; // Translate parse error to invalid mutability error

  return err;
}

IN_ERROR innative::ParseGlobalDecl(Stream& s, GlobalDecl& g, const Environment& env)
{
  IN_ERROR err = ParseGlobalDesc(s, g.desc);

  if(err >= 0)
    err = ParseInitializer(s, g.init, env);

  return err;
}

IN_ERROR innative::ParseImport(Stream& s, Import& i, const Environment& env)
{
  IN_ERROR err = ParseIdentifier(s, i.module_name, env);

  if(err < 0)
    return err;

  if(!ValidateIdentifier(i.module_name))
    return ERR_INVALID_UTF8_ENCODING;

  if(err = ParseIdentifier(s, i.export_name, env))
    return err;

  if(!ValidateIdentifier(i.export_name))
    return ERR_INVALID_UTF8_ENCODING;

  if(err = ParseVarUInt7(s, i.kind))
    return err;

  switch(i.kind)
  {
  case WASM_KIND_FUNCTION:
    i.func_desc.debug       = { 1, static_cast<decltype(i.func_desc.debug.line)>(s.pos) };
    i.func_desc.param_debug = 0;
    return ParseVarUInt32(s, i.func_desc.type_index);
  case WASM_KIND_TABLE: return ParseTableDesc(s, i.table_desc);
  case WASM_KIND_MEMORY: return ParseMemoryDesc(s, i.mem_desc);
  case WASM_KIND_GLOBAL: return ParseGlobalDesc(s, i.global_desc);
  default: err = ERR_FATAL_UNKNOWN_KIND;
  }

  return err;
}

IN_ERROR innative::ParseExport(Stream& s, Export& e, const Environment& env)
{
  IN_ERROR err = ParseIdentifier(s, e.name, env);

  if(err >= 0)
    err = ParseVarUInt7(s, e.kind);

  if(err >= 0)
    err = ParseVarUInt32(s, e.index);

  return err;
}

IN_ERROR innative::ParseInstruction(Stream& s, Instruction& ins, const Environment& env)
{
  ins.line     = 1;
  ins.column   = static_cast<decltype(ins.column)>(s.pos);
  IN_ERROR err = ParseByte(s, ins.opcode);
  if(err < 0)
    return err;

  switch(ins.opcode)
  {
  case OP_block:
  case OP_loop:
  case OP_if: ins.immediates[0]._varsint7 = s.ReadVarInt7(err); break;
  case OP_br:
  case OP_br_if:
  case OP_local_get:
  case OP_local_set:
  case OP_local_tee:
  case OP_global_get:
  case OP_global_set:
  case OP_call: ins.immediates[0]._varuint32 = s.ReadVarUInt32(err); break;
  case OP_i32_const: ins.immediates[0]._varsint32 = s.ReadVarInt32(err); break;
  case OP_i64_const: ins.immediates[0]._varsint64 = s.ReadVarInt64(err); break;
  case OP_f32_const: ins.immediates[0]._float32 = s.ReadFloat32(err); break;
  case OP_f64_const: ins.immediates[0]._float64 = s.ReadFloat64(err); break;
  case OP_memory_grow:
  case OP_memory_size:
    ins.immediates[0]._varuint1 = s.ReadVarUInt1(err);
    if(err < 0 || ins.immediates[0]._varuint1 !=
                    0) // We override any error here with ERR_INVALID_RESERVED_VALUE because that's what webassembly expects
      err = ERR_INVALID_RESERVED_VALUE;
    break;
  case OP_br_table:
    err = Parse<varuint32>::template Array<&ParseVarUInt32>(s, ins.immediates[0].table, ins.immediates[0].n_table, env);

    if(err >= 0)
      ins.immediates[1]._varuint32 = s.ReadVarUInt32(err);
    break;
  case OP_call_indirect:
    ins.immediates[0]._varuint32 = s.ReadVarUInt32(err);

    if(err >= 0)
    {
      ins.immediates[1]._varuint1 = s.ReadVarUInt1(err);

      if(err < 0 ||
         ins.immediates[1]._varuint1 !=
           0) // We override any error here with ERR_INVALID_RESERVED_VALUE because that's what webassembly expects
        err = ERR_INVALID_RESERVED_VALUE;
    }

    break;
  case OP_i32_load:
  case OP_i64_load:
  case OP_f32_load:
  case OP_f64_load:
  case OP_i32_store:
  case OP_i64_store:
  case OP_f32_store:
  case OP_f64_store:
  case OP_i32_load8_s:
  case OP_i32_load16_s:
  case OP_i64_load8_s:
  case OP_i64_load16_s:
  case OP_i64_load32_s:
  case OP_i32_load8_u:
  case OP_i32_load16_u:
  case OP_i64_load8_u:
  case OP_i64_load16_u:
  case OP_i64_load32_u:
  case OP_i32_store8:
  case OP_i32_store16:
  case OP_i64_store8:
  case OP_i64_store16:
  case OP_i64_store32:
    ins.immediates[0]._varuint32 = s.ReadVarUInt32(err);

    if(err >= 0)
      ins.immediates[1]._varuptr = s.ReadVarUInt32(err); // Currently 32-bit because all memories are 32-bit

    break;
  case OP_unreachable:
  case OP_nop:
  case OP_else:
  case OP_end:
  case OP_return:
  case OP_drop:
  case OP_select:
  case OP_i32_eqz:
  case OP_i32_eq:
  case OP_i32_ne:
  case OP_i32_lt_s:
  case OP_i32_lt_u:
  case OP_i32_gt_s:
  case OP_i32_gt_u:
  case OP_i32_le_s:
  case OP_i32_le_u:
  case OP_i32_ge_s:
  case OP_i32_ge_u:
  case OP_i64_eqz:
  case OP_i64_eq:
  case OP_i64_ne:
  case OP_i64_lt_s:
  case OP_i64_lt_u:
  case OP_i64_gt_s:
  case OP_i64_gt_u:
  case OP_i64_le_s:
  case OP_i64_le_u:
  case OP_i64_ge_s:
  case OP_i64_ge_u:
  case OP_f32_eq:
  case OP_f32_ne:
  case OP_f32_lt:
  case OP_f32_gt:
  case OP_f32_le:
  case OP_f32_ge:
  case OP_f64_eq:
  case OP_f64_ne:
  case OP_f64_lt:
  case OP_f64_gt:
  case OP_f64_le:
  case OP_f64_ge:
  case OP_i32_clz:
  case OP_i32_ctz:
  case OP_i32_popcnt:
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
  case OP_i32_rotr:
  case OP_i64_clz:
  case OP_i64_ctz:
  case OP_i64_popcnt:
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
  case OP_i64_rotr:
  case OP_f32_abs:
  case OP_f32_neg:
  case OP_f32_ceil:
  case OP_f32_floor:
  case OP_f32_trunc:
  case OP_f32_nearest:
  case OP_f32_sqrt:
  case OP_f32_add:
  case OP_f32_sub:
  case OP_f32_mul:
  case OP_f32_div:
  case OP_f32_min:
  case OP_f32_max:
  case OP_f32_copysign:
  case OP_f64_abs:
  case OP_f64_neg:
  case OP_f64_ceil:
  case OP_f64_floor:
  case OP_f64_trunc:
  case OP_f64_nearest:
  case OP_f64_sqrt:
  case OP_f64_add:
  case OP_f64_sub:
  case OP_f64_mul:
  case OP_f64_div:
  case OP_f64_min:
  case OP_f64_max:
  case OP_f64_copysign:
  case OP_i32_wrap_i64:
  case OP_i32_trunc_f32_s:
  case OP_i32_trunc_f32_u:
  case OP_i32_trunc_f64_s:
  case OP_i32_trunc_f64_u:
  case OP_i64_extend_i32_s:
  case OP_i64_extend_i32_u:
  case OP_i64_trunc_f32_s:
  case OP_i64_trunc_f32_u:
  case OP_i64_trunc_f64_s:
  case OP_i64_trunc_f64_u:
  case OP_f32_convert_i32_s:
  case OP_f32_convert_i32_u:
  case OP_f32_convert_i64_s:
  case OP_f32_convert_i64_u:
  case OP_f32_demote_f64:
  case OP_f64_convert_i32_s:
  case OP_f64_convert_i32_u:
  case OP_f64_convert_i64_s:
  case OP_f64_convert_i64_u:
  case OP_f64_promote_f32:
  case OP_i32_reinterpret_f32:
  case OP_i64_reinterpret_f64:
  case OP_f32_reinterpret_i32:
  case OP_f64_reinterpret_i64: break;
  default: err = ERR_FATAL_UNKNOWN_INSTRUCTION;
  }

  return err;
}

IN_ERROR innative::ParseTableInit(Stream& s, TableInit& init, Module& m, const Environment& env)
{
  IN_ERROR err = ParseVarUInt32(s, init.index);

  if(err >= 0)
    err = ParseInitializer(s, init.offset, env);

  if(err >= 0)
  {
    TableDesc* desc = ModuleTable(m, init.index);
    if(!desc)
      err = ERR_INVALID_TABLE_INDEX;
    else if(desc->element_type == TE_funcref)
      err = Parse<varuint32>::template Array<&ParseVarUInt32>(s, init.elements, init.n_elements, env);
    else
      err = ERR_FATAL_BAD_ELEMENT_TYPE;
  }

  return err;
}

IN_ERROR innative::ParseFunctionBody(Stream& s, FunctionBody& f, Module& m, const Environment& env)
{
  f.line        = 1;
  f.column      = static_cast<decltype(f.column)>(s.pos);
  IN_ERROR err  = ParseVarUInt32(s, f.body_size);
  size_t end    = s.pos + f.body_size; // body_size is the size of both local_entries and body in bytes.
  ptrdiff_t idx = &f - m.code.funcbody;
  if(idx >= static_cast<ptrdiff_t>(m.function.n_funcdecl))
    return ERR_FUNCTION_BODY_MISMATCH;

  auto& sig = m.type.functypes[m.function.funcdecl[idx].type_index];

  if(err >= 0)
  {
    err = Parse<FunctionLocal>::template Array<&ParseFunctionLocal>(s, f.locals, f.n_locals, env);
    if(err < 0)
      return err;

    f.local_size = 0;
    for(varuint32 i = 0; i < f.n_locals; ++i)
    {
      if(f.locals[i].count > (std::numeric_limits<uint32_t>::max() - f.local_size))
        return ERR_FATAL_TOO_MANY_LOCALS; // Ensure we don't overflow the local count
      f.local_size += f.locals[i].count;
    }
  }

  f.body = 0;
  if(err >= 0 && f.body_size > 0)
  {
    f.body = tmalloc<Instruction>(env, f.body_size); // Overallocate maximum number of possible instructions we might have
    if(!f.body)
      return ERR_FATAL_OUT_OF_MEMORY;

    for(f.n_body = 0; s.pos < end && err >= 0; ++f.n_body)
      err = ParseInstruction(s, f.body[f.n_body], env);
  }

  return err;
}

IN_ERROR innative::ParseDataInit(Stream& s, DataInit& data, const Environment& env)
{
  IN_ERROR err = ParseVarUInt32(s, data.index);

  if(err >= 0)
    err = ParseInitializer(s, data.offset, env);

  if(err >= 0)
    err = ParseByteArray(s, data.data, false, env);

  return err;
}

IN_ERROR innative::ParseNameSectionParam(Stream& s, size_t num, Module& m, FunctionDesc& desc,
                                         DebugInfo* (*fn)(Stream&, Module&, FunctionDesc&, varuint32, const Environment&),
                                         const Environment& env)
{
  IN_ERROR err;
  varuint32 n_params = m.type.functypes[desc.type_index].n_params;
  desc.param_debug   = tmalloc<DebugInfo>(env, n_params);
  memset(desc.param_debug, 0, sizeof(DebugInfo) * n_params);

  for(varuint32 i = 0; i < num; ++i)
  {
    auto index = s.ReadVarUInt32(err);
    if(err < 0)
      return err;
    DebugInfo* debug = (index >= n_params) ? (*fn)(s, m, desc, index - n_params, env) : &desc.param_debug[index];
    if(!debug)
      return ERR_INVALID_LOCAL_INDEX;

    debug->line   = 1;
    debug->column = static_cast<decltype(debug->column)>(s.pos);

    err = ParseByteArray(s, debug->name, true, env);
  }

  return err;
}

IN_ERROR innative::ParseNameSection(Stream& s, size_t end, Module& m, const Environment& env)
{
  IN_ERROR err = ERR_SUCCESS;

  while(s.pos < end && err >= ERR_SUCCESS)
  {
    auto type = s.ReadVarUInt7(err);
    if(err < 0)
      return err;
    auto len = s.ReadVarUInt32(err);
    if(err < 0)
      return err;

    switch(type)
    {
    case 0: // module
      err = ParseByteArray(s, m.name, true, env);
      break;
    case 1: // functions
    {
      varuint32 count = s.ReadVarUInt32(err);

      for(varuint32 i = 0; i < count && err >= 0; ++i)
      {
        auto index = s.ReadVarUInt32(err);
        if(err < 0)
          return err;

        if(index < m.importsection.functions)
        {
          err = ParseByteArray(s, m.importsection.imports[index].func_desc.debug.name, true, env);
          continue;
        }
        index -= m.importsection.functions;
        if(index >= m.function.n_funcdecl)
          return ERR_INVALID_FUNCTION_INDEX;

        err = ParseByteArray(s, m.function.funcdecl[index].debug.name, true, env);
      }
    }
    break;
    case 2: // locals
    {
      varuint32 count = s.ReadVarUInt32(err);

      for(varuint32 k = 0; k < count && err >= 0; ++k)
      {
        auto fn = s.ReadVarUInt32(err);
        if(err < 0)
          return err;

        varuint32 num = s.ReadVarUInt32(err);
        if(fn < m.importsection.functions)
        {
          auto& desc = m.importsection.imports[fn].func_desc;
          if(desc.type_index >= m.type.n_functypes)
            return ERR_INVALID_TYPE_INDEX;

          if(num > 0)
            err = ParseNameSectionParam(
              s, num, m, desc,
              [](Stream& s, Module& m, FunctionDesc& desc, varuint32, const Environment&) -> DebugInfo* { return nullptr; },
              env);

          continue;
        }

        fn -= m.importsection.functions;
        if(fn >= m.code.n_funcbody)
          return ERR_INVALID_FUNCTION_INDEX;

        auto& desc = m.function.funcdecl[fn];
        if(desc.type_index >= m.type.n_functypes)
          return ERR_INVALID_TYPE_INDEX;

        // We count the maximum possible number of splits (summation of all counts minus one) and limit num to that
        varuint32 worst_case = 0;
        for(varuint32 i = 0; i < m.code.funcbody[desc.type_index].n_locals; ++i)
          worst_case += m.code.funcbody[desc.type_index].locals[i].count;
        worst_case = std::min(worst_case - m.code.funcbody[desc.type_index].n_locals, num);

        // Then we over-commit by allocating the worst possible scenario (which in practice usually isn't that much)
        auto locals = tmalloc<FunctionLocal>(env, worst_case);
        tmemcpy(locals, worst_case, m.code.funcbody[desc.type_index].locals, m.code.funcbody[desc.type_index].n_locals);
        m.code.funcbody[desc.type_index].locals = locals;

        err = ParseNameSectionParam(
          s, num, m, desc,
          [](Stream& s, Module& m, FunctionDesc& desc, varuint32 index, const Environment& env) -> DebugInfo* {
            if(index >= m.code.funcbody[desc.type_index].local_size)
              return nullptr;

            FunctionLocal* p = m.code.funcbody[desc.type_index].locals;
            while(index >= p->count)
            {
              index -= p->count;
              ++p;
              assert(p < (p + m.code.funcbody[desc.type_index].n_locals));
            }

            if(index > 0) // If this isn't an exact match, split the node
            {
              FunctionLocal* last = p++;
              memmove(p + 1, p, m.code.funcbody[desc.type_index].n_locals - (p - m.code.funcbody[desc.type_index].locals));
              p->type     = last->type;
              p->count    = last->count - index;
              last->count = index;
              ++m.code.funcbody[desc.type_index].n_locals;
            }
            return &p->debug; // Now return that new debug value
          },
          env);

        if(err < 0)
          return err;
      }
    }
    break;
    default: s.pos += len;
    }
  }

  return err;
}

IN_ERROR innative::ParseModule(Stream& s, const char* file, const Environment& env, Module& m, ByteArray name,
                               ValidationError*& errors)
{
  m = { 0 };

  IN_ERROR err   = ERR_SUCCESS;
  m.magic_cookie = s.ReadUInt32(err);

  if(err < 0)
    return err;
  if(m.magic_cookie != WASM_MAGIC_COOKIE)
    return ERR_PARSE_INVALID_MAGIC_COOKIE;

  m.version = s.ReadUInt32(err);

  if(err < 0)
    return err;
  if(m.version != WASM_MAGIC_VERSION)
    return ERR_PARSE_INVALID_VERSION;

  size_t begin = s.pos;
  m.n_custom   = 0; // Count the custom sections so we can preallocate them
  while(!s.End())
  {
    varuint7 op = s.ReadVarUInt7(err);
    if(err < 0)
      return err;

    if(op > WASM_SECTION_DATA) // require valid opcode to continue
      return ERR_FATAL_UNKNOWN_SECTION;
    if(op == WASM_SECTION_CUSTOM)
      ++m.n_custom;
    s.pos += s.ReadVarUInt32(err);

    if(err < 0)
      return err;
  }

  if(s.pos != s.size)
    return ERR_PARSE_INVALID_FILE_LENGTH;

  size_t curcustom = 0;
  s.pos            = begin;
  m.exports        = kh_init_exports();
  m.filepath       = utility::AllocString(const_cast<Environment&>(env), file);

  if(m.n_custom > 0)
  {
    m.custom = tmalloc<CustomSection>(env, m.n_custom);
    if(!m.custom)
      return ERR_FATAL_OUT_OF_MEMORY;
  }

  varuint32 funcbody = 0;

  while(err >= 0 && !s.End())
  {
    varuint7 opcode = s.ReadVarUInt7(err);
    if(err < 0)
      break;
    varuint32 payload = s.ReadVarUInt32(err);
    if(err < 0)
      break;
    if(opcode != WASM_SECTION_CUSTOM) // Section order only applies to known sections
    {
      if(!ValidateSectionOrder(m.knownsections, opcode))
        return ERR_FATAL_INVALID_WASM_SECTION_ORDER; // This has to be a fatal error because some sections rely on others
                                                     // being loaded
      m.knownsections |= (1 << opcode);
    }

    switch(opcode)
    {
    case WASM_SECTION_TYPE:
      err = Parse<FunctionType, const Environment&>::template Array<&ParseFunctionType>(s, m.type.functypes,
                                                                                        m.type.n_functypes, env, env);
      break;
    case WASM_SECTION_IMPORT:
    {
      if(err = Parse<Import, const Environment&>::template Array<&ParseImport>(s, m.importsection.imports,
                                                                               m.importsection.n_import, env, env))
        return err;
      std::stable_sort(m.importsection.imports, m.importsection.imports + m.importsection.n_import,
                       [](const Import& a, const Import& b) -> bool { return a.kind < b.kind; });

      varuint32 num           = m.importsection.n_import;
      m.importsection.globals = 0;
      for(varuint32 i = 0; i < num; ++i)
      {
        switch(m.importsection.imports[i].kind)
        {
        case WASM_KIND_FUNCTION: ++m.importsection.functions;
        case WASM_KIND_TABLE: ++m.importsection.tables;
        case WASM_KIND_MEMORY: ++m.importsection.memories;
        case WASM_KIND_GLOBAL: ++m.importsection.globals; break;
        default: return ERR_FATAL_UNKNOWN_KIND;
        }
      }

      if(m.importsection.n_import != num) // n_import is the same as globals, check to make sure we derived it properly
        return ERR_FATAL_INVALID_MODULE;
    }
    break;
    case WASM_SECTION_FUNCTION:
      err = Parse<FunctionDesc>::template Array<&ParseFunctionDesc>(s, m.function.funcdecl, m.function.n_funcdecl, env);
      break;
    case WASM_SECTION_TABLE:
      err = Parse<TableDesc>::template Array<&ParseTableDesc>(s, m.table.tables, m.table.n_tables, env);
      break;
    case WASM_SECTION_MEMORY:
      err = Parse<MemoryDesc>::template Array<&ParseMemoryDesc>(s, m.memory.memories, m.memory.n_memories, env);
      break;
    case WASM_SECTION_GLOBAL:
      err = Parse<GlobalDecl, const Environment&>::template Array<&ParseGlobalDecl>(s, m.global.globals, m.global.n_globals,
                                                                                    env, env);
      break;
    case WASM_SECTION_EXPORT:
      err = Parse<Export, const Environment&>::template Array<&ParseExport>(s, m.exportsection.exports,
                                                                            m.exportsection.n_exports, env, env);
      break;
    case WASM_SECTION_START: m.start = s.ReadVarUInt32(err); break;
    case WASM_SECTION_ELEMENT:
      err = Parse<TableInit, Module&, const Environment&>::template Array<&ParseTableInit>(s, m.element.elements,
                                                                                           m.element.n_elements, env, m,
                                                                                           env);
      break;
    case WASM_SECTION_CODE:
      err = Parse<FunctionBody, Module&, const Environment&>::template Array<&ParseFunctionBody>(s, m.code.funcbody,
                                                                                                 m.code.n_funcbody, env, m,
                                                                                                 env);
      break;
    case WASM_SECTION_DATA:
      err = Parse<DataInit, const Environment&>::template Array<&ParseDataInit>(s, m.data.data, m.data.n_data, env, env);
      break;
    case WASM_SECTION_CUSTOM:
      if(payload < 1) // A custom section MUST have an identifier, which itself must take up at least 1 byte, so a payload
                      // of 0 bytes is impossible.
        return ERR_PARSE_INVALID_FILE_LENGTH;
      else
      {
        assert(curcustom < m.n_custom);
        m.custom[curcustom].payload = payload;
        m.custom[curcustom].data    = s.data + s.pos;
        size_t custom               = s.pos + payload;
        err                         = ParseIdentifier(s, m.custom[curcustom].name, env);
        if(err == ERR_SUCCESS && !ValidateIdentifier(m.custom[curcustom].name))
          return ERR_INVALID_UTF8_ENCODING; // An invalid UTF8 encoding for the name is an actual parse error for some reason
        if(err == ERR_SUCCESS && !strcmp(m.custom[curcustom].name.str(), "name"))
          ParseNameSection(s, custom, m, env);
        else if(err == ERR_SUCCESS && !strcmp(m.custom[curcustom].name.str(), "sourceMappingURL"))
        {
          Identifier sourceMappingURL;
          ParseIdentifier(s, sourceMappingURL, env);
          m.sourcemap = tmalloc<SourceMap>(env, 1);
          if(!m.sourcemap)
            return ERR_FATAL_OUT_OF_MEMORY;
          err = ParseSourceMap(&env, m.sourcemap, sourceMappingURL.str(), 0);

          if(err == ERR_FATAL_FILE_ERROR && m.filepath != nullptr)
            err = ParseSourceMap(&env, m.sourcemap,
                                 (GetPath(m.filepath).parent_path() / sourceMappingURL.str()).u8string().c_str(), 0);
          if(err)
            return err;
        }
        else if(err == ERR_SUCCESS && !strcmp(m.custom[curcustom].name.str(), "external_debug_info"))
        {
          Identifier externalDebugURL;
          ParseIdentifier(s, externalDebugURL, env);
          m.sourcemap = tmalloc<SourceMap>(env, 1);
          if(!m.sourcemap)
            return ERR_FATAL_OUT_OF_MEMORY;
          *m.sourcemap = { 0 };

          DWARFParser parser(const_cast<Environment*>(&env), m.sourcemap);
          err = parser.ParseDWARF(externalDebugURL.str(), 0);
          if(err)
            return err;
        }
        else if(err == ERR_SUCCESS && !strcmp(m.custom[curcustom].name.str(), ".debug_line"))
        {
          m.sourcemap = tmalloc<SourceMap>(env, 1);
          if(!m.sourcemap)
            return ERR_FATAL_OUT_OF_MEMORY;
          *m.sourcemap = { 0 };

          DWARFParser parser(const_cast<Environment*>(&env), m.sourcemap);
          err = parser.ParseDWARF(reinterpret_cast<const char*>(s.data), s.size);
          if(m.filepath)
            m.sourcemap->file = m.filepath;
          if(err)
            return err;
          s.pos = custom;
        }
        else
          s.pos = custom; // Skip over the custom payload, minus the name
        break;
      }
    default: return ERR_FATAL_UNKNOWN_SECTION;
    }
  }

  if(err < 0)
    return err;

  if(!m.name.size())
  {
    m.name.resize(name.size(), true, env);
    if(!m.name.get())
      return ERR_FATAL_OUT_OF_MEMORY;
    tmemcpy(m.name.get(), m.name.size(), name.get(), name.size());
  }
  if(!m.name.size() || !ValidateIdentifier(m.name))
    return ERR_PARSE_INVALID_NAME;

  if(m.code.n_funcbody != m.function.n_funcdecl)
    return ERR_FUNCTION_BODY_MISMATCH;

  // If we are requesting debug information but none exists, generate a .wat file
  if(!m.sourcemap && (env.flags & ENV_DEBUG) != 0)
  {
    auto path = temp_directory_path() / m.name.str();
    path += ".wat";
    m.filepath = utility::AllocString(const_cast<Environment&>(env), path.generic_u8string());
    std::ofstream f(m.filepath, std::ios_base::binary | std::ios_base::out | std::ios_base::trunc);
    if(!f.bad())
    {
      Serializer serializer(env, m, &f);
      serializer.TokenizeModule(false);
      f << std::endl;
    }
  }

  return ParseExportFixup(m, errors, env);
}

IN_ERROR innative::ParseExportFixup(Module& m, ValidationError*& errors, const Environment& env)
{
  for(varuint32 i = 0; i < m.exportsection.n_exports; ++i)
  {
    int r        = 0;
    khint_t iter = kh_put_exports(m.exports, m.exportsection.exports[i].name, &r);
    if(r < 0)
      return ERR_FATAL_BAD_HASH;
    if(!r)
      AppendError(env, errors, &m, ERR_FATAL_DUPLICATE_EXPORT, "Duplicate export name %s",
                  m.exportsection.exports[i].name.str());
    kh_value(m.exports, iter) = i;
  }

  return ERR_SUCCESS;
}
