// Copyright (c)2021 Fundament Software
// For conditions of distribution and use, see copyright notice in innative.h

#include "wat.h"
#include "utility.h"
#include "parse.h"
#include "atomic_instructions.h"
#include <limits>

using std::numeric_limits;
using std::string;

using namespace innative;
using namespace utility;
using namespace wat;

namespace innative {
  namespace wat {
    __KHASH_IMPL(indexname, , StringSpan, varuint32, 1, internal::__ac_X31_hash_stringrefins, kh_int_hash_equal);
  }
}

WatParser::WatParser(Environment& e, Module& mod) : m(mod), env(e)
{
  typehash   = kh_init_indexname();
  funchash   = kh_init_indexname();
  tablehash  = kh_init_indexname();
  memoryhash = kh_init_indexname();
  globalhash = kh_init_indexname();
  datahash   = kh_init_indexname();
  elemhash   = kh_init_indexname();
}
WatParser::~WatParser()
{
  kh_destroy_indexname(typehash);
  kh_destroy_indexname(funchash);
  kh_destroy_indexname(tablehash);
  kh_destroy_indexname(memoryhash);
  kh_destroy_indexname(globalhash);
  kh_destroy_indexname(datahash);
  kh_destroy_indexname(elemhash);
}

varuint32 WatParser::GetJump(WatToken var)
{
  if(var.id == WatTokens::NUMBER)
    return ResolveInlineToken<varuint32, &ResolveTokenu32>(var);
  if(var.id == WatTokens::NAME)
  {
    utility::StringSpan err = { var.pos, var.len };
    for(varuint32 i = 0; i < stack.Size(); ++i)
      if(stack[i] == err)
        return i;
  }

  return (varuint32)~0;
}

void WatParser::WriteUTF32(uint32_t ch, ByteArray& str, varuint32& index)
{
  static const uint32_t UNICODE_REPLACEMENT_CHAR = 0x0000FFFD;
  static const uint32_t UNICODE_MAX_LEGAL_UTF32  = 0x0010FFFF;
  static const uint8_t firstByteMark[7]          = { 0x00, 0x00, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC };
  static const uint32_t byteMask                 = 0xBF;
  static const uint32_t byteMark                 = 0x80;

  int bytesToWrite;
  if(ch < 0x80)
    bytesToWrite = 1;
  else if(ch < 0x800)
    bytesToWrite = 2;
  else if(ch < 0x10000)
    bytesToWrite = 3;
  else if(ch <= UNICODE_MAX_LEGAL_UTF32)
    bytesToWrite = 4;
  else
  {
    bytesToWrite = 3;
    ch           = UNICODE_REPLACEMENT_CHAR;
  }

  varuint32 target = index + bytesToWrite;
  switch(bytesToWrite)
  { /* note: everything falls through. */
  case 4: str[--target] = (uint8_t)((ch | byteMark) & byteMask); ch >>= 6;
  case 3: str[--target] = (uint8_t)((ch | byteMark) & byteMask); ch >>= 6;
  case 2: str[--target] = (uint8_t)((ch | byteMark) & byteMask); ch >>= 6;
  case 1: str[--target] = (uint8_t)(ch | firstByteMark[bytesToWrite]);
  }
  index += bytesToWrite;
}

int WatParser::WatString(const Environment& env, ByteArray& str, StringSpan t)
{
  if(!t.s)
    return ERR_PARSE_INVALID_NAME;

  varuint32 index = 0;
  if(str.get())
  {
    index       = str.size();
    varuint32 n = str.size() + static_cast<varuint32>(t.len);
    uint8_t* b  = tmalloc<uint8_t>(env, static_cast<size_t>(n) + 1);
    if(!b)
      return ERR_FATAL_OUT_OF_MEMORY;

    tmemcpy(b, n, str.get(), str.size());
    new(&str) ByteArray(b, n);
  }
  else
    str.resize(static_cast<varuint32>(t.len), true, env);

  if(!t.len)
    return ERR_SUCCESS;

  if(!str.get())
    return ERR_FATAL_OUT_OF_MEMORY;

  for(size_t i = 0; i < t.len; ++i)
  {
    if(t.s[i] == '\\')
    {
      switch(t.s[++i])
      {
      case 'n': str[index++] = '\n'; break;
      case 't': str[index++] = '\t'; break;
      case '\\': str[index++] = '\\'; break;
      case '\'': str[index++] = '\''; break;
      case '"': str[index++] = '"'; break;
      case 'u':
      {
        char* end;
        long u;
        errno = 0;
        u     = strtol(t.s + i + 1, &end, 16);
        if(errno == ERANGE)
          return ERR_WAT_OUT_OF_RANGE;

        WriteUTF32(u, str, index);
        i += end - (t.s + i + 1);
        break;
      }
      default:
        if((t.s[i] >= '0' && t.s[i] <= '9') || (t.s[i] >= 'A' && t.s[i] <= 'F') || (t.s[i] >= 'a' && t.s[i] <= 'f'))
        {
          if((t.s[i + 1] >= '0' && t.s[i + 1] <= '9') || (t.s[i + 1] >= 'A' && t.s[i + 1] <= 'F') ||
             (t.s[i + 1] >= 'a' && t.s[i + 1] <= 'f'))
          {
            char buf[3]  = { t.s[i], t.s[i + 1], 0 };
            str[index++] = (uint8_t)strtol(buf, 0, 16);
            ++i;
            break;
          }
        }
        return ERR_WAT_BAD_ESCAPE;
      }
    }
    else
      str[index++] = t.s[i];
  }
  str.discard(index, true);

  return ERR_SUCCESS;
}

int WatParser::ParseName(Environment& env, ByteArray& name, const WatToken& t)
{
  if(t.id != WatTokens::NAME || !t.pos || !t.len || t.len > numeric_limits<varuint32>::max())
    return ERR_PARSE_INVALID_NAME;

  name.from((uint8_t*)t.pos, t.len, env);
  return !name.get() ? ERR_FATAL_OUT_OF_MEMORY : ERR_SUCCESS;
}

varsint7 WatParser::WatValType(WatTokens id)
{
  switch(id)
  {
  case WatTokens::i32: return TE_i32;
  case WatTokens::i64: return TE_i64;
  case WatTokens::f32: return TE_f32;
  case WatTokens::f64: return TE_f64;
  case WatTokens::CREF: return TE_cref;
  }

  return 0;
}

int WatParser::AddWatValType(Environment& env, WatTokens id, varsint7*& a, varuint32& n)
{
  varsint7 ty = WatValType(id);
  if(!ty)
    return ERR_WAT_EXPECTED_VALTYPE;
  return AppendArray<varsint7>(env, ty, a, n);
}

int WatParser::ParseFunctionTypeInner(Environment& env, Queue<WatToken>& tokens, FunctionType& sig, DebugInfo** info,
                                      varuint32* n_info, bool anonymous)
{
  sig.form = TE_func;
  int err;
  varuint32 sz = sig.n_params;
  if(!n_info)
    n_info = &sz;

  while(tokens.Size() > 1 && tokens[0].id == WatTokens::OPEN && tokens[1].id == WatTokens::PARAM)
  {
    WatToken src = tokens[1];
    EXPECTED(env, tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
    EXPECTED(env, tokens, WatTokens::PARAM, ERR_WAT_EXPECTED_TOKEN);

    if(tokens.Peek().id == WatTokens::NAME)
    {
      if(anonymous)
        return ERR_WAT_UNEXPECTED_NAME;
      if(info) // You are legally allowed to put parameter names in typedefs in WAT, but the names are thrown away.
      {
        if(tokens.Peek().len >= numeric_limits<varuint32>::max())
          return ERR_WAT_OUT_OF_RANGE;
        DebugInfo debug = { src.line, src.column };
        ParseName(env, debug.name, tokens.Peek());

        // Check for duplicate param names
        for(varuint32 i = 0; i < *n_info; ++i)
        {
          auto& other = (*info)[i];
          if(debug.name == other.name)
            return ERR_WAT_DUPLICATE_NAME;
        }

        while(*n_info < sig.n_params)
          if(err = AppendArray<DebugInfo>(env, { 0 }, *info, *n_info))
            return err;

        if(err = AppendArray<DebugInfo>(env, debug, *info, *n_info))
          return err;
      }
      tokens.Pop();
      if(err = AddWatValType(env, tokens.Pop().id, sig.params, sig.n_params))
        return err;
    }
    else
    {
      while(tokens.Peek().id != WatTokens::CLOSE)
      {
        if(info)
        {
          while(*n_info < sig.n_params)
            if(err = AppendArray<DebugInfo>(env, { 0 }, *info, *n_info))
              return err;
          if(err = AppendArray<DebugInfo>(env, DebugInfo{ src.line, src.column }, *info, *n_info))
            return err;
        }
        if(err = AddWatValType(env, tokens.Pop().id, sig.params, sig.n_params))
          return err;
      }
    }

    EXPECTED(env, tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }

  while(tokens.Size() > 1 && tokens[0].id == WatTokens::OPEN && tokens[1].id == WatTokens::RESULT)
  {
    EXPECTED(env, tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
    EXPECTED(env, tokens, WatTokens::RESULT, ERR_WAT_EXPECTED_TOKEN);

    while(tokens.Peek().id != WatTokens::CLOSE)
      if(err = AddWatValType(env, tokens.Pop().id, sig.returns, sig.n_returns))
        return err;

    EXPECTED(env, tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }

  // We detect this special case because otherwise it can turn into a "type mismatch" error, which is very confusing
  if(tokens.Size() > 1 && tokens[0].id == WatTokens::OPEN && tokens[1].id == WatTokens::PARAM)
    return ERR_WAT_PARAM_AFTER_RESULT;
  return ERR_SUCCESS;
}

int WatParser::ParseFunctionType(Queue<WatToken>& tokens, WatToken token)
{
  EXPECTED(env, tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
  EXPECTED(env, tokens, WatTokens::FUNC, ERR_WAT_EXPECTED_FUNC);

  FunctionType sig = { 0 };
  int err          = ParseFunctionTypeInner(env, tokens, sig, 0, 0, false);
  if(err != 0)
    return err;
  m.knownsections |= (1 << WASM_SECTION_TYPE);
  if(err = AddName(typehash, token, m.type.n_functypes, 0))
    return err;
  if(err = AppendArray<FunctionType>(env, sig, m.type.functypes, m.type.n_functypes))
    return err;

  EXPECTED(env, tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
  return ERR_SUCCESS;
}

int WatParser::AppendImport(const Import& i, varuint32* index)
{
  if(m.table.n_tables > 0 || m.function.n_funcdecl > 0 || m.global.n_globals > 0 || m.memory.n_memories > 0)
    return ERR_WAT_INVALID_IMPORT_ORDER; // If we're trying to insert an import after declaring a table/func/global/memory,
                                         // fail.

  *index = 0;
  if(utility::ReallocArray(env, m.importsection.imports, m.importsection.n_import) != ERR_SUCCESS)
    return ERR_FATAL_OUT_OF_MEMORY;

  // Find the correct index to insert into
  for(varuint32 j = 0; j < m.importsection.n_import - 1; ++j)
  {
    if(i.kind >= m.importsection.imports[j].kind)
      *index = j + 1;
    else
      break;
  }

  m.knownsections |= (1 << WASM_SECTION_IMPORT);
  if((m.importsection.n_import - *index - 1) > 0) // Move things out of the way if we aren't at the end of the array
    memmove(m.importsection.imports + *index + 1, m.importsection.imports + *index,
            (m.importsection.n_import - *index - 1) * sizeof(Import));

  m.importsection.imports[*index] = i; // Set the value

  // Properly increment counts based on kind
  switch(i.kind)
  {
  case WASM_KIND_FUNCTION: ++m.importsection.functions;
  case WASM_KIND_TABLE: ++m.importsection.tables;
  case WASM_KIND_MEMORY: ++m.importsection.memories;
  case WASM_KIND_GLOBAL: // Skip incrementing the globals count, because we already did it when incrementing n_import
    break;
  }

  switch(i.kind) // Fix the index
  {
  case WASM_KIND_TABLE: *index -= m.importsection.functions; break;
  case WASM_KIND_MEMORY: *index -= m.importsection.tables; break;
  case WASM_KIND_GLOBAL: *index -= m.importsection.memories; break;
  }

  // ValidateImportOrder(m);
  return ERR_SUCCESS;
}

varuint32 WatParser::GetFromHash(kh_indexname_t* hash, const WatToken& t)
{
  if(t.id == WatTokens::NUMBER)
    return ResolveInlineToken<varuint32, &ResolveTokenu32>(t);
  else if(t.id == WatTokens::NAME)
  {
    khiter_t iter = kh_get_indexname(hash, StringSpan{ t.pos, t.len });

    if(kh_exist2(hash, iter))
      return kh_val(hash, iter);
  }

  return (varuint32)~0;
}

// This looks for an identical existing type and returns that ID if it exists, or inserts the signature as a new type
int WatParser::MergeFunctionType(const FunctionType& ftype, varuint32& out)
{
  for(varuint32 i = 0; i < m.type.n_functypes; ++i) // The WASM spec requires we look for the lowest possible matching index
  {
    if(MatchFunctionType(m.type.functypes[i], ftype))
    {
      out = i;
      return ERR_SUCCESS;
    }
  }

  out = m.type.n_functypes;
  m.knownsections |= (1 << WASM_SECTION_TYPE);
  return AppendArray<FunctionType>(env, ftype, m.type.functypes, m.type.n_functypes);
}

int WatParser::ParseType(Queue<WatToken>& tokens, varuint32& sig)
{
  if(tokens.Size() > 1 && tokens[0].id == WatTokens::OPEN && tokens[1].id == WatTokens::TYPE)
  {
    EXPECTED(env, tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
    EXPECTED(env, tokens, WatTokens::TYPE, ERR_WAT_EXPECTED_TYPE);

    if(tokens.Peek().id != WatTokens::NUMBER && tokens.Peek().id != WatTokens::NAME)
      return ERR_WAT_EXPECTED_VAR;

    sig = GetFromHash(typehash, tokens.Pop());

    if(sig >= m.type.n_functypes)
      AppendError(env, env.errors, &m, ERR_WAT_UNKNOWN_TYPE, "Invalid type signature %u", sig);

    EXPECTED(env, tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }

  return ERR_SUCCESS;
}

int WatParser::ParseTypeUse(Queue<WatToken>& tokens, varuint32& sig, DebugInfo** info, varuint32* n_info, bool anonymous)
{
  sig = (varuint32)~0;
  if(int err = ParseType(tokens, sig))
    return err;

  if(tokens.Size() > 1 && tokens[0].id == WatTokens::OPEN &&
     (tokens[1].id == WatTokens::PARAM || tokens[1].id == WatTokens::RESULT))
  {
    // Create a type to match this function signature
    FunctionType func = { 0 };

    int err = ParseFunctionTypeInner(env, tokens, func, info, n_info, anonymous);
    if(err)
      return err;

    if(sig != (varuint32)~0) // If we already have a type, compare the two types and make sure they are identical
    {
      if(sig < m.type.n_functypes && !MatchFunctionType(m.type.functypes[sig], func))
        return ERR_WAT_TYPE_MISMATCH;
    }
    else
    {
      sig = m.type.n_functypes;
      return MergeFunctionType(func, sig);
    }
  }

  if(sig == (varuint32)~0) // If we still don't have a type, this is an empty type we need to add
  {
    return MergeFunctionType(FunctionType{ TE_func }, sig);
  }

  return ERR_SUCCESS;
}

varuint32 WatParser::GetLocal(FunctionBody& f, FunctionDesc& desc, varuint32 n_params, const WatToken& t)
{
  if(t.id == WatTokens::NUMBER)
    return ResolveInlineToken<varuint32, &ResolveTokenu32>(t);
  else if(t.id == WatTokens::NAME)
  {
    ByteArray n = ByteArray::Identifier(t.pos, t.len);

    if(desc.param_debug)
    {
      for(varuint32 i = 0; i < n_params; ++i)
        if(n == desc.param_debug[i].name)
          return i;
    }

    varuint32 index = n_params;
    for(varuint32 i = 0; i < f.n_locals; ++i)
    {
      if(n == f.locals[i].debug.name)
        return index;
      index += f.locals[i].count;
    }
  }

  return (varuint32)~0;
}

varuint32 WatParser::GetMemory(const WatToken& t)
{
  if(t.id == WatTokens::NUMBER)
    return ResolveInlineToken<varuint32, &ResolveTokenu32>(t);
  else if(t.id == WatTokens::NAME)
  {
    ByteArray n = ByteArray::Identifier(t.pos, t.len);

    varuint32 i = 0;
    while(auto mem = ModuleMemory(m, i))
    {
      if(mem->debug.name == n)
        return i;
      ++i;
    }
  }

  return (varuint32)~0;
}

int WatParser::ParseConstantOperator(Queue<WatToken>& tokens, Instruction& op)
{
  int err = ERR_SUCCESS;
  switch(op.opcode[0])
  {
  case OP_i32_const: err = ResolveTokeni32(tokens.Pop(), numbuf, op.immediates[0]._varsint32); break;
  case OP_i64_const: err = ResolveTokeni64(tokens.Pop(), numbuf, op.immediates[0]._varsint64); break;
  case OP_f32_const: err = ResolveTokenf32(tokens.Pop(), numbuf, op.immediates[0]._float32); break;
  case OP_f64_const: err = ResolveTokenf64(tokens.Pop(), numbuf, op.immediates[0]._float64); break;
  case OP_global_get: // For constant initializers, this has to be an import, and thus must always already exist by the
                      // time we reach it.
    op.immediates[0]._varuint32 = GetFromHash(globalhash, tokens.Pop());
    if(op.immediates[0]._varuint32 == (varuint32)~0)
      return ERR_WAT_INVALID_VAR;
    break;
  default: return ERR_INVALID_INITIALIZER;
  }

  return err;
}
int WatParser::ParseOperator(Queue<WatToken>& tokens, Instruction& op, FunctionBody& f, FunctionDesc& desc,
                             FunctionType& sig, WatParser::DeferWatAction& defer)
{
  if(tokens.Peek().id != WatTokens::OPERATOR)
    EXPECTED(env, tokens, WatTokens::OPERATOR, ERR_WAT_EXPECTED_OPERATOR);

  int err;
  if(tokens.Peek().i == 0xFF)
    return ERR_WAT_OUT_OF_RANGE;
  op           = { 0 };
  op.opcode[0] = (uint8_t)tokens.Peek().i;
  op.opcode[1] = (uint8_t)(tokens.Peek().i >> 8);
  op.line      = tokens.Peek().line;
  op.column    = tokens.Pop().column;

  switch(op.opcode[0])
  {
  case 0xFF: return ERR_FATAL_UNKNOWN_INSTRUCTION;
  case OP_br:
  case OP_br_if:
    op.immediates[0]._varuint32 = GetJump(tokens.Pop());
    if(op.immediates[0]._varuint32 == (varuint32)~0)
      return ERR_WAT_EXPECTED_VAR;
    break;
  case OP_local_get:
  case OP_local_set:
  case OP_local_tee:
    op.immediates[0]._varuint32 = GetLocal(f, desc, sig.n_params, tokens.Pop());
    if(op.immediates[0]._varuint32 == (varuint32)~0)
      return ERR_WAT_INVALID_LOCAL;
    break;
  case OP_global_get:
    if(!sig.form) // If this is zero, this is an initializer
    {
      if(err = ParseConstantOperator(tokens, op))
        return err;
      break;
    }
  case OP_global_set:
  case OP_call: defer = WatParser::DeferWatAction{ op.opcode[0], tokens.Pop(), 0, 0 }; break;
  case OP_i32_const:
  case OP_i64_const:
  case OP_f32_const:
  case OP_f64_const:
    if(err = ParseConstantOperator(tokens, op))
      return err;
    break;
  case OP_br_table:
    do
    {
      varuint32 jump = GetJump(tokens.Pop());
      if(jump == (varuint32)~0)
        return ERR_WAT_EXPECTED_VAR;

      if(err = AppendArray<varuint32>(env, jump, op.immediates[0].table, op.immediates[0].n_table))
        return err;
    } while(tokens.Peek().id == WatTokens::NAME || tokens.Peek().id == WatTokens::NUMBER);

    op.immediates[1]._varuint32 =
      op.immediates[0].table[--op.immediates[0].n_table]; // Remove last jump from table and make it the default
    break;
  case OP_call_indirect:
    if(err = ParseTypeUse(tokens, op.immediates[0]._varuint32, 0, 0, true))
      return err;
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
    if(err = ParseMemarg(tokens, op))
      return err;
    break;
  case OP_memory_size:
  case OP_memory_grow:
    if(tokens.Peek().id == WatTokens::NAME || tokens.Peek().id == WatTokens::NUMBER)
    {
      op.immediates[0]._varuint32 = GetMemory(tokens.Pop());
      if(op.immediates[0]._varuint32 == (varuint32)~0)
        return ERR_WAT_INVALID_MEMORY;
    }
    break;
  case OP_misc_ops_prefix:
    op.immediates[0]._varuint32 = 0;
    op.immediates[1]._varuint32 = 0;
    switch(op.opcode[1])
    {
    case OP_memory_init:
    case OP_table_init:
    case OP_data_drop:
    case OP_elem_drop: defer = WatParser::DeferWatAction{ op.opcode[1], tokens.Pop(), 0, 0 }; break;
    }
    break;
  case OP_atomic_prefix:
    if(op.opcode[1] == OP_atomic_fence) // The only atomic op that doesn't take a memarg
      break;
    // Set the default memargs for when they aren't specified
    op.immediates[0]._varuint32 = innative::atomic_details::GetValidAlignment(op.opcode[1]);
    op.immediates[1]._varuint32 = 0;
    if(err = ParseMemarg(tokens, op))
      return err;
    break;
  case OP_ref_null:
    EXPECTED(env, tokens, WatTokens::FUNC, ERR_WAT_EXPECTED_FUNC);
    op.immediates[0]._varuint32 = 0x70;
    break;
  case OP_ref_func: op.immediates[0]._varuint32 = GetFromHash(funchash, tokens.Pop()); break;
  }

  return ERR_SUCCESS;
}

int innative::WatParser::ParseMemarg(Queue<WatToken>& tokens, Instruction& op)
{
  int err;
  op.immediates[2]._varuint32 = 0; // default memidx to 0
  if(tokens.Peek().id == WatTokens::NAME || tokens.Peek().id == WatTokens::NUMBER)
  {
    op.immediates[2]._varuint32 = GetMemory(tokens.Pop());
    if(op.immediates[2]._varuint32 == (varuint32)~0)
      return ERR_WAT_INVALID_MEMORY;
  }

  if(tokens.Peek().id == WatTokens::OFFSET)
  {
    tokens.Pop();
    if(err = ResolveTokenu32(tokens.Pop(), numbuf, op.immediates[1]._varuint32))
      // if(err = ResolveTokenu64(tokens.Pop(), op.immediates[1]._varuptr)) // We can't do this until webassembly actually
      // supports 64-bit
      return err;
  }
  if(tokens.Peek().id == WatTokens::ALIGN)
  {
    tokens.Pop();
    if(err = ResolveTokenu32(tokens.Pop(), numbuf, op.immediates[0]._varuint32))
      return err;
    if(op.immediates[0]._varuint32 == 0 ||
       !IsPowerOfTwo(op.immediates[0]._varuint32)) // Ensure this alignment is exactly a power of two
      return ERR_WAT_INVALID_ALIGNMENT;
    op.immediates[0]._varuint32 = Power2Log2(op.immediates[0]._varuint32); // Calculate proper power of two
  }
  return ERR_SUCCESS;
}

void WatParser::ParseLabel(Queue<WatToken>& tokens)
{
  if(tokens.Peek().id == WatTokens::NAME)
  {
    stack.Push(StringSpan{ tokens.Peek().pos, tokens.Peek().len });
    tokens.Pop();
  }
  else
    stack.Push(StringSpan{ 0, 0 });
}

bool WatParser::CheckLabel(Queue<WatToken>& tokens)
{
  if(tokens.Peek().id == WatTokens::NAME)
  {
    WatToken t = tokens.Pop();
    return stack.Peek() == StringSpan{ t.pos, t.len };
  }

  return true;
}

int WatParser::ParseBlockType(Queue<WatToken>& tokens, varsint64& out)
{
  varuint32 idx = ~0;
  int err       = ParseType(tokens, idx);
  if(err < 0)
    return err;
  FunctionType sig = { 0 };
  err              = ParseFunctionTypeInner(env, tokens, sig, 0, 0, true);
  if(err < 0)
    return err;

  if(idx != ~0)
  {
    // If we have both a type index and a signature, ensure they are identical.
    if(sig.n_returns || sig.n_params)
    {
      if(idx < m.type.n_functypes && !MatchFunctionType(m.type.functypes[idx], sig))
        return ERR_WAT_TYPE_MISMATCH;
    }
    out = idx;
  }
  else if(!sig.n_returns && !sig.n_params)
    out = TE_void;
  else if(sig.n_returns == 1 && !sig.n_params)
    out = sig.returns[0];
  else
  {
    out = m.type.n_functypes;
    if(err = AppendArray<FunctionType>(env, sig, m.type.functypes, m.type.n_functypes))
      return err;
  }
  return ERR_SUCCESS;
}

int WatParser::ParseExpression(Queue<WatToken>& tokens, FunctionBody& f, FunctionDesc& desc, FunctionType& sig,
                               varuint32 index)
{
  EXPECTED(env, tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);

  int err;
  varsint64 blocktype;
  switch(tokens[0].id)
  {
  case WatTokens::BLOCK:
  case WatTokens::LOOP:
  {
    WatToken t = tokens.Pop();
    ParseLabel(tokens);
    if(err = ParseBlockType(tokens, blocktype))
      return err;

    {
      Instruction op = { t.id == WatTokens::BLOCK ? (uint8_t)OP_block : (uint8_t)OP_loop };

      op.immediates[0]._varsint64 = blocktype;
      op.line                     = t.line;
      op.column                   = t.column;
      if(err = AppendArray<Instruction>(env, op, f.body, f.n_body))
        return err;
    }

    while(tokens.Peek().id != WatTokens::CLOSE)
      if(err = ParseInstruction(tokens, f, desc, sig, index))
        return err;

    Instruction op = { OP_end };
    op.line        = tokens.Peek().line;
    op.column      = tokens.Peek().column;
    if(err = AppendArray<Instruction>(env, op, f.body, f.n_body))
      return err;

    stack.Pop();
    break;
  }
  case WatTokens::IF:
  {
    WatToken t = tokens.Pop();
    ParseLabel(tokens);
    if(err = ParseBlockType(tokens, blocktype))
      return err;

    while(tokens.Size() > 1 && tokens[0].id == WatTokens::OPEN && tokens[1].id != WatTokens::THEN)
      if(err = ParseExpression(tokens, f, desc, sig, index))
        return err;

    {
      Instruction op = { OP_if };

      op.immediates[0]._varsint64 = blocktype;
      op.line                     = t.line;
      op.column                   = t.column;
      if(err = AppendArray<Instruction>(env, op, f.body,
                                        f.n_body)) // We append the if instruction _after_ the optional condition expression
        return err;
    }
  }

    EXPECTED(env, tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN); // There must always be a Then branch
    EXPECTED(env, tokens, WatTokens::THEN, ERR_WAT_EXPECTED_THEN);

    while(tokens.Peek().id != WatTokens::CLOSE)
      if(err = ParseInstruction(tokens, f, desc, sig, index))
        return err;

    EXPECTED(env, tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);

    if(tokens.Peek().id == WatTokens::OPEN) // Must be an else branch if it exists
    {
      EXPECTED(env, tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);

      WatToken t     = tokens.Peek();
      Instruction op = { OP_else };
      EXPECTED(env, tokens, WatTokens::ELSE, ERR_WAT_EXPECTED_ELSE);

      op.line   = t.line;
      op.column = t.column;
      if(err = AppendArray<Instruction>(env, op, f.body, f.n_body))
        return err;

      while(tokens.Peek().id != WatTokens::CLOSE)
        if(err = ParseInstruction(tokens, f, desc, sig, index))
          return err;

      EXPECTED(env, tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
    }

    {
      Instruction op = { OP_end };

      op.line   = tokens.Peek().line;
      op.column = tokens.Peek().column;
      if(err = AppendArray<Instruction>(env, op, f.body, f.n_body))
        return err;
    }

    stack.Pop();
    break;
  default:
  {
    Instruction op;
    WatParser::DeferWatAction defer = { 0 };
    if(err = ParseOperator(tokens, op, f, desc, sig, defer))
      return err;

    // Expressions are folded instructions, so we must unfold them before inserting the operator
    while(tokens[0].id != WatTokens::CLOSE)
      if(err = ParseExpression(tokens, f, desc, sig, index))
        return err;

    if(defer.id) // Only perform the defer after we evaluate the folded instructions, so f.n_body is correct
      deferred.Push(WatParser::DeferWatAction{ defer.id, defer.t, index, f.n_body });
    if(err = AppendArray<Instruction>(env, op, f.body, f.n_body)) // Now we append the operator
      return err;
    break;
  }
  }

  EXPECTED(env, tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
  return ERR_SUCCESS;
}

int WatParser::ParseInstruction(Queue<WatToken>& tokens, FunctionBody& f, FunctionDesc& desc, FunctionType& sig,
                                varuint32 index)
{
  int err;
  varsint64 blocktype;
  switch(tokens[0].id)
  {
  case WatTokens::OPEN: // This must be an expression
    return ParseExpression(tokens, f, desc, sig, index);
  case WatTokens::BLOCK:
  case WatTokens::LOOP:
  {
    WatToken t = tokens.Pop();
    ParseLabel(tokens);
    if(err = ParseBlockType(tokens, blocktype))
      return err;

    {
      Instruction op = { t.id == WatTokens::BLOCK ? (uint8_t)OP_block : (uint8_t)OP_loop };

      op.immediates[0]._varsint64 = blocktype;
      op.line                     = t.line;
      op.column                   = t.column;
      if(err = AppendArray<Instruction>(env, op, f.body, f.n_body))
        return err;
    }

    while(tokens.Peek().id != WatTokens::END)
      if(err = ParseInstruction(tokens, f, desc, sig, index))
        return err;

    EXPECTED(env, tokens, WatTokens::END, ERR_WAT_EXPECTED_END);

    if(!CheckLabel(tokens))
      return ERR_WAT_LABEL_MISMATCH;

    {
      Instruction op = { OP_end };

      op.line   = tokens.Peek().line;
      op.column = tokens.Peek().column;
      if(err = AppendArray<Instruction>(env, op, f.body, f.n_body))
        return err;
    }

    stack.Pop();
    break;
  }
  case WatTokens::IF:
  {
    WatToken t = tokens.Pop();
    ParseLabel(tokens);
    if(err = ParseBlockType(tokens, blocktype))
      return err;

    {
      Instruction op = { OP_if };

      op.immediates[0]._varsint64 = blocktype;
      op.line                     = t.line;
      op.column                   = t.column;
      if(err = AppendArray<Instruction>(env, op, f.body,
                                        f.n_body)) // We append the if instruction _after_ the optional condition expression
        return err;
    }

    while(tokens.Peek().id != WatTokens::ELSE && tokens.Peek().id != WatTokens::END)
      if(err = ParseInstruction(tokens, f, desc, sig, index))
        return err;

    t = tokens.Pop();
    if(t.id == WatTokens::ELSE) // Handle else branch
    {
      if(!CheckLabel(tokens))
        return ERR_WAT_LABEL_MISMATCH;

      Instruction op = { OP_else };

      op.line   = t.line;
      op.column = t.column;
      if(err = AppendArray<Instruction>(env, op, f.body, f.n_body))
        return err;

      while(tokens.Peek().id != WatTokens::END)
        if(err = ParseInstruction(tokens, f, desc, sig, index))
          return err;

      EXPECTED(env, tokens, WatTokens::END, ERR_WAT_EXPECTED_END);
    }
  }

    if(!CheckLabel(tokens))
      return ERR_WAT_LABEL_MISMATCH;

    {
      Instruction op = { OP_end };

      op.line   = tokens.Peek().line;
      op.column = tokens.Peek().column;
      if(err = AppendArray<Instruction>(env, op, f.body, f.n_body))
        return err;
    }

    stack.Pop();
    break;
  default:
  {
    Instruction op;
    WatParser::DeferWatAction defer = { 0 };
    if(err = ParseOperator(tokens, op, f, desc, sig, defer))
      return err;

    if(defer.id)
      deferred.Push(WatParser::DeferWatAction{ defer.id, defer.t, index, f.n_body });
    return AppendArray<Instruction>(env, op, f.body, f.n_body);
  }
  }

  return ERR_SUCCESS;
}

int WatParser::InlineImportExport(Queue<WatToken>& tokens, varuint32* index, varuint7 kind, Import** out)
{
  int err;
  while(tokens.Size() > 1 && tokens[0].id == WatTokens::OPEN && tokens[1].id == WatTokens::EXPORT)
  {
    EXPECTED(env, tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
    EXPECTED(env, tokens, WatTokens::EXPORT, ERR_WAT_EXPECTED_EXPORT);

    Export e = {};
    e.kind   = kind;
    e.index  = *index; // This is fine because you can only import OR export on a declaration statement

    if(err = WatString(env, e.name, tokens.Pop()))
      return err;
    if(!ValidateIdentifier(e.name))
      return ERR_INVALID_UTF8_ENCODING;
    m.knownsections |= (1 << WASM_SECTION_EXPORT);
    if(err = AppendArray<Export>(env, e, m.exportsection.exports, m.exportsection.n_exports))
      return err;
    EXPECTED(env, tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }
  if(tokens.Size() > 1 && tokens[0].id == WatTokens::OPEN && tokens[1].id == WatTokens::IMPORT)
  {
    EXPECTED(env, tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
    EXPECTED(env, tokens, WatTokens::IMPORT, ERR_WAT_EXPECTED_IMPORT);

    Import i = {};
    if(err = WatString(env, i.module_name, tokens.Pop()))
      return err;
    if(!ValidateIdentifier(i.module_name))
      return ERR_INVALID_UTF8_ENCODING;
    if(err = WatString(env, i.export_name, tokens.Pop()))
      return err;
    if(!ValidateIdentifier(i.export_name))
      return ERR_INVALID_UTF8_ENCODING;

    i.kind = kind;
    if(err = AppendImport(i, index))
      return err;

    switch(i.kind) // Fix the index
    {
    case WASM_KIND_FUNCTION: *out = m.importsection.imports + *index; break;
    case WASM_KIND_TABLE: *out = m.importsection.imports + m.importsection.functions + *index; break;
    case WASM_KIND_MEMORY: *out = m.importsection.imports + m.importsection.tables + *index; break;
    case WASM_KIND_GLOBAL: *out = m.importsection.imports + m.importsection.memories + *index; break;
    }

    EXPECTED(env, tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }

  return ERR_SUCCESS;
}

int WatParser::ParseLocalAppend(const Environment& env, FunctionLocal& local, FunctionBody& body, Queue<WatToken>& tokens)
{
  local.type = WatValType(tokens.Pop().id);
  if(!local.type)
    return ERR_WAT_EXPECTED_VALTYPE;
  return AppendArray<FunctionLocal>(env, local, body.locals, body.n_locals);
}

int WatParser::ParseFunction(Queue<WatToken>& tokens, varuint32* index, StringSpan name)
{
  int err;
  FunctionBody body = { 0 };
  body.line         = tokens.Peek().line;
  body.column       = tokens.Peek().column;

  *index      = m.function.n_funcdecl + m.importsection.functions;
  Import* imp = 0;
  if(err = InlineImportExport(tokens, index, WASM_KIND_FUNCTION, &imp))
    return err;

  if(imp) // If this is an import, assemble the aux information and abort.
    return ParseTypeUse(tokens, imp->func_desc.type_index, &imp->func_desc.param_debug, 0, false);

  FunctionDesc desc     = { 0 };
  varuint32 n_paraminfo = 0;
  desc.debug.line       = body.line;
  desc.debug.column     = body.column;
  if(err = ParseTypeUse(tokens, desc.type_index, &desc.param_debug, &n_paraminfo, false))
    return err;

  FunctionType& functy = m.type.functypes[desc.type_index];
  if(name.len > 0)
    if(err = WatString(env, desc.debug.name, name))
      return err;

  // Read in all the locals
  while(tokens.Size() > 1 && tokens[0].id == WatTokens::OPEN && tokens[1].id == WatTokens::LOCAL)
  {
    WatToken src = tokens[1];
    EXPECTED(env, tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
    EXPECTED(env, tokens, WatTokens::LOCAL, ERR_WAT_EXPECTED_LOCAL);

    FunctionLocal local;
    local.debug = { src.line, src.column };
    local.count = 1;

    if(tokens.Peek().id == WatTokens::NAME)
    {
      if(tokens.Peek().len > numeric_limits<varuint32>::max())
        return ERR_WAT_OUT_OF_RANGE;
      ParseName(env, local.debug.name, tokens.Pop());

      // Check for name collisions, params first
      for(varuint32 i = 0; i < n_paraminfo; ++i)
        if(desc.param_debug[i].name == local.debug.name)
          return ERR_WAT_DUPLICATE_NAME;
      for(varuint32 i = 0; i < body.n_locals; ++i)
        if(body.locals[i].debug.name == local.debug.name)
          return ERR_WAT_DUPLICATE_NAME;

      if(err = ParseLocalAppend(env, local, body, tokens))
        return err;
    }
    else // Otherwise can have zero or more val_types
    {
      while(tokens[0].id != WatTokens::CLOSE)
      {
        local.debug = { tokens[0].line, tokens[0].column };
        if(err = ParseLocalAppend(env, local, body, tokens))
          return err;
      }
    }

    EXPECTED(env, tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }

  // Read in all instructions
  assert(stack.Size() == 0);
  while(tokens.Peek().id != WatTokens::CLOSE)
  {
    if(err = ParseInstruction(tokens, body, desc, functy, *index))
      return err;
  }
  assert(stack.Size() == 0);
  Instruction op = { OP_end };
  op.line        = tokens.Peek().line;
  op.column      = tokens.Peek().column;
  if(err = AppendArray(env, op, body.body, body.n_body))
    return err;

  m.knownsections |= (1 << WASM_SECTION_FUNCTION);
  if(err = AppendArray(env, desc, m.function.funcdecl, m.function.n_funcdecl))
    return err;

  m.knownsections |= (1 << WASM_SECTION_CODE);
  return AppendArray(env, body, m.code.funcbody, m.code.n_funcbody);
}

int WatParser::ParseResizableLimits(ResizableLimits& limits, Queue<WatToken>& tokens)
{
  int err = ResolveTokenu32(tokens.Pop(), numbuf, limits.minimum);
  if(err)
    return err;
  if(tokens.Peek().id == WatTokens::NUMBER)
  {
    if(err = ResolveTokenu32(tokens.Pop(), numbuf, limits.maximum))
      return err;
    limits.flags |= WASM_LIMIT_HAS_MAXIMUM;
  }

  auto shareToken = tokens.Peek().id;
  if(shareToken == WatTokens::SHARED || shareToken == WatTokens::UNSHARED)
  {
    tokens.Pop();
    if(shareToken == WatTokens::SHARED)
      limits.flags |= WASM_LIMIT_SHARED;
  }

  return ERR_SUCCESS;
}

int WatParser::ParseTableDesc(TableDesc& t, Queue<WatToken>& tokens)
{
  int err;
  if(err = ParseResizableLimits(t.resizable, tokens))
    return err;

  EXPECTED(env, tokens, WatTokens::FUNCREF, ERR_WAT_EXPECTED_FUNCREF);

  t.element_type = TE_funcref;
  return ERR_SUCCESS;
}

int WatParser::ParseTable(Queue<WatToken>& tokens, WatToken token)
{
  int err;
  varuint32 index = m.table.n_tables + m.importsection.tables - m.importsection.functions;
  Import* i       = 0;
  if(err = InlineImportExport(tokens, &index, WASM_KIND_TABLE, &i))
    return err;

  if(i) // If this is an import, assemble the aux information and abort.
  {
    if(err = ParseTableDesc(i->table_desc, tokens))
      return err;
    return AddName(tablehash, token, index, &i->table_desc.debug);
  }

  TableDesc table = { 0 };
  switch(tokens.Peek().id)
  {
  case WatTokens::NUMBER:
    if(err = ParseTableDesc(table, tokens))
      return err;
    break;
  default:
    EXPECTED(env, tokens, WatTokens::FUNCREF, ERR_WAT_EXPECTED_FUNCREF);

    table.element_type    = TE_funcref;
    table.resizable.flags = 0;
    deferred.Push(
      WatParser::DeferWatAction{ -static_cast<int>(WatTokens::ELEM), { WatTokens::NONE }, tokens.GetPosition(), index });
    WatSkipSection(tokens); // Defer element section to after we know we've loaded everything.
  }

  m.knownsections |= (1 << WASM_SECTION_TABLE);
  if(err = AddName(tablehash, token, index, &table.debug))
    return err;
  return AppendArray(env, table, m.table.tables, m.table.n_tables);
}

int WatParser::ParseInitializerInstruction(Queue<WatToken>& tokens, Instruction& op, bool expr)
{
  int err;
  FunctionBody blank     = { 0 };
  FunctionDesc descblank = { 0 };
  FunctionType typeblank = { 0 };

  if(expr)
  {
    if(err = ParseExpression(tokens, blank, descblank, typeblank, 0))
      return err;
  }
  else
  {
    while(tokens.Peek().id != WatTokens::CLOSE)
    {
      if(err = ParseInstruction(tokens, blank, descblank, typeblank, 0))
        return err;
    }
  }

  if(blank.n_body == 0)
    AppendError(env, env.errors, 0, ERR_INVALID_INITIALIZER_TYPE, "Only one instruction is allowed as an initializer");

  if(blank.n_body > 1)
  {
    varuint32 i = 0; // For some reason, webassembly wants a type mismatch error if there are multiple constant
                     // instructions that would otherwise be valid.
    for(; i < blank.n_body; ++i)
    {
      switch(blank.body[i].opcode[0])
      {
      case OP_i32_const:
      case OP_i64_const:
      case OP_f32_const:
      case OP_f64_const:
      case OP_global_get: continue;
      }
      break;
    }
    AppendError(env, env.errors, 0, (i == blank.n_body) ? ERR_INVALID_INITIALIZER_TYPE : ERR_INVALID_INITIALIZER,
                "Only one instruction is allowed as an initializer");
  }

  if(blank.n_body > 0)
    op = blank.body[0];

  return ERR_SUCCESS;
}

int WatParser::ParseInitializer(Queue<WatToken>& tokens, Instruction& op)
{
  int err = ParseInitializerInstruction(tokens, op, false);
  if(err < 0)
    return err;

  if(tokens.Peek().id != WatTokens::CLOSE)
    return ERR_INVALID_INITIALIZER;

  return ERR_SUCCESS;
}

int WatParser::ParseGlobalDesc(GlobalDesc& g, Queue<WatToken>& tokens)
{
  if(tokens.Peek().id == WatTokens::OPEN)
  {
    EXPECTED(env, tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
    EXPECTED(env, tokens, WatTokens::MUT, ERR_WAT_EXPECTED_MUT);
    g.mutability = true;
    if(!(g.type = WatValType(tokens.Pop().id)))
      return ERR_WAT_EXPECTED_VALTYPE;

    EXPECTED(env, tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }
  else
  {
    g.mutability = false;
    if(!(g.type = WatValType(tokens.Pop().id)))
      return ERR_WAT_EXPECTED_VALTYPE;
  }

  return ERR_SUCCESS;
}

int WatParser::ParseGlobal(Queue<WatToken>& tokens, WatToken token)
{
  int err;
  varuint32 index = m.global.n_globals + m.importsection.globals - m.importsection.memories;
  Import* i       = 0;
  if(err = InlineImportExport(tokens, &index, WASM_KIND_GLOBAL, &i))
    return err;

  if(i) // If this is an import, assemble the aux information and abort.
  {
    if(err = ParseGlobalDesc(i->global_desc, tokens))
      return err;
    return AddName(globalhash, token, index, &i->global_desc.debug);
  }
  GlobalDecl g = { 0 };
  if(err = ParseGlobalDesc(g.desc, tokens))
    return err;

  if(err = ParseInitializer(tokens, g.init))
    return err;

  m.knownsections |= (1 << WASM_SECTION_GLOBAL);
  if(err = AddName(globalhash, token, index, &g.desc.debug))
    return err;
  return AppendArray(env, g, m.global.globals, m.global.n_globals);
}

int WatParser::ParseMemoryDesc(MemoryDesc& mem, Queue<WatToken>& tokens)
{
  return ParseResizableLimits(mem.limits, tokens);
}

int WatParser::ParseMemory(Queue<WatToken>& tokens, WatToken token)
{
  int err;
  varuint32 index = m.memory.n_memories + m.importsection.memories - m.importsection.tables;
  Import* i       = 0;
  if(err = InlineImportExport(tokens, &index, WASM_KIND_MEMORY, &i))
    return err;

  if(i) // If this is an import, assemble the aux information and abort.
  {
    if(err = ParseMemoryDesc(i->mem_desc, tokens))
      return err;
    return AddName(memoryhash, token, index, &i->mem_desc.debug);
  }
  MemoryDesc mem = { 0 };

  if(tokens.Size() > 1 && tokens[0].id == WatTokens::OPEN && tokens[1].id == WatTokens::DATA)
  {
    EXPECTED(env, tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
    EXPECTED(env, tokens, WatTokens::DATA, ERR_WAT_EXPECTED_TOKEN);
    DataInit init = { 0 };
    init.index    = index;
    init.offset   = Instruction{ OP_i32_const, 0 };

    while(tokens[0].id != WatTokens::CLOSE)
    {
      if(tokens[0].id != WatTokens::STRING)
        return ERR_WAT_EXPECTED_STRING;
      if(err = WatString(env, init.data, tokens.Pop()))
        return err;
    }

    m.knownsections |= (1 << WASM_SECTION_DATA);
    if(err = AppendArray(env, init, m.data.data, m.data.n_data))
      return err;

    mem.limits.flags   = 0;
    mem.limits.minimum = (init.data.size() + 0xFFFF) / 0x10000;
    EXPECTED(env, tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }
  else if(err = ParseMemoryDesc(mem, tokens))
    return err;

  m.knownsections |= (1 << WASM_SECTION_MEMORY);
  if(err = AddName(memoryhash, token, index, &mem.debug))
    return err;
  return AppendArray(env, mem, m.memory.memories, m.memory.n_memories);
}

int WatParser::AddName(kh_indexname_t* h, WatToken t, varuint32 index, DebugInfo* info)
{
  if(t.id == WatTokens::NAME)
  {
    int r;
    khiter_t iter = kh_put_indexname(h, StringSpan{ t.pos, t.len }, &r);
    if(!r)
      return ERR_WAT_DUPLICATE_NAME;
    if(iter != kh_end(h))
      kh_val(h, iter) = index;
    if(info)
      info->name.from((uint8_t*)t.pos, t.len, env);
  }

  return ERR_SUCCESS;
}

int WatParser::ParseImport(Queue<WatToken>& tokens)
{
  Import i = {};
  int err;
  if(err = WatString(env, i.module_name, tokens.Pop()))
    return err;
  if(err = WatString(env, i.export_name, tokens.Pop()))
    return err;

  EXPECTED(env, tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);

  WatToken t           = tokens.Pop();
  WatToken name        = GetWatNameToken(tokens);
  kh_indexname_t* hash = 0;
  switch(t.id)
  {
  case WatTokens::FUNC:
    i.kind = WASM_KIND_FUNCTION;
    if(name.id == WatTokens::NAME && (err = ParseName(env, i.func_desc.debug.name, name)))
      return err;
    if(err = ParseTypeUse(tokens, i.func_desc.type_index, &i.func_desc.param_debug, 0, false))
      return err;
    hash = funchash;
    break;
  case WatTokens::GLOBAL:
    i.kind = WASM_KIND_GLOBAL;
    if(err = ParseGlobalDesc(i.global_desc, tokens))
      return err;
    hash = globalhash;
    i.global_desc.debug.name.from(reinterpret_cast<const uint8_t*>(name.pos), name.len, env);
    break;
  case WatTokens::TABLE:
    i.kind = WASM_KIND_TABLE;
    if(err = ParseTableDesc(i.table_desc, tokens))
      return err;
    hash = tablehash;
    i.table_desc.debug.name.from(reinterpret_cast<const uint8_t*>(name.pos), name.len, env);
    break;
  case WatTokens::MEMORY:
    i.kind = WASM_KIND_MEMORY;
    if(err = ParseMemoryDesc(i.mem_desc, tokens))
      return err;
    hash = memoryhash;
    i.mem_desc.debug.name.from(reinterpret_cast<const uint8_t*>(name.pos), name.len, env);
    break;
  default: return ERR_WAT_EXPECTED_KIND;
  }
  EXPECTED(env, tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);

  varuint32 index;
  if(err = AppendImport(i, &index))
    return err;

  return AddName(hash, name, index, 0);
}

int WatParser::ParseExport(Queue<WatToken>& tokens)
{
  Export e = {};
  int err;
  if(err = WatString(env, e.name, tokens.Pop()))
    return err;

  EXPECTED(env, tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
  switch(tokens.Pop().id)
  {
  case WatTokens::FUNC:
    e.kind  = WASM_KIND_FUNCTION;
    e.index = GetFromHash(funchash, tokens.Pop());
    break;
  case WatTokens::GLOBAL:
    e.kind  = WASM_KIND_GLOBAL;
    e.index = GetFromHash(globalhash, tokens.Pop());
    break;
  case WatTokens::TABLE:
    e.kind  = WASM_KIND_TABLE;
    e.index = GetFromHash(tablehash, tokens.Pop());
    break;
  case WatTokens::MEMORY:
    e.kind  = WASM_KIND_MEMORY;
    e.index = GetFromHash(memoryhash, tokens.Pop());
    break;
  default: return ERR_WAT_EXPECTED_KIND;
  }
  EXPECTED(env, tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);

  m.knownsections |= (1 << WASM_SECTION_EXPORT);
  return AppendArray(env, e, m.exportsection.exports, m.exportsection.n_exports);
}

// Rewritten based on the latest definition of ('elem' ...) in the
// bulk-memory-operations spec reference.
int WatParser::ParseElem(TableInit& e, Queue<WatToken>& tokens)
{
  int err;

  // First element: optional id
  WatToken nameTok;
  if(MatchTokens<WatTokens::NAME>(tokens, &nameTok))
  {
    if(err = AddName(elemhash, nameTok, m.element.n_elements, 0))
      return err;
  }

  // Optional table index.
  if(MatchTokens<WatTokens::OPEN, WatTokens::TABLE>(tokens))
  {
    e.flags |= WASM_ELEM_ACTIVE_HAS_INDEX;
    e.index = GetFromHash(tablehash, tokens.Pop());
    if(e.index == ~0u)
      return ERR_WAT_INVALID_VAR;
    EXPECTED(env, tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }

  // The next field that could appear would be the offset, which
  // is the only valid next expression that starts with OPEN
  if(tokens[0].id == WatTokens::OPEN)
  {
    bool offset = MatchTokens<WatTokens::OPEN, WatTokens::OFFSET>(tokens);
    if(err = ParseInitializerInstruction(tokens, e.offset, !offset))
      return err;
    if(offset)
      EXPECTED(env, tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }
  // Otherwise this must be a passive segment
  else
  {
    e.flags |= WASM_ELEM_PASSIVE;
  }

  // Now all that's left is to parse the elemlist
  if(err = ParseElemList(e, tokens))
    return err;

  m.knownsections |= (1 << WASM_SECTION_ELEMENT);
  return AppendArray(env, e, m.element.elements, m.element.n_elements);
}

int innative::WatParser::ParseElemList(TableInit& e, Queue<WatToken>& tokens)
{
  int err;

  bool elemexpr = tokens[0].id == WatTokens::OPEN ||
                  (!MatchTokens<WatTokens::FUNC>(tokens) && MatchTokens<WatTokens::FUNCREF>(tokens));
  if(elemexpr)
  {
    e.flags |= WASM_ELEM_CARRIES_ELEMEXPRS;
    e.elem_type = TE_funcref;
    while(tokens[0].id == WatTokens::OPEN)
    {
      Instruction elem;
      if(err = ParseInitializerInstruction(tokens, elem, true))
        return err;
      if(err = AppendArray(env, elem, e.elemexprs, e.n_elements))
        return err;
    }
  }
  else
  {
    e.extern_kind = 0;
    while(tokens[0].id != WatTokens::CLOSE)
    {
      varuint32 func = GetFromHash(funchash, tokens.Pop());
      if(func == ~0u)
        return ERR_WAT_INVALID_VAR;
      if(err = AppendArray(env, func, e.elements, e.n_elements))
        return err;
    }
  }

  return ERR_SUCCESS;
}

int WatParser::ParseData(Queue<WatToken>& tokens)
{
  DataInit d = { 0 };
  int err;

  if(tokens[0].id == WatTokens::NAME)
  {
    auto t = tokens.Pop();
    if(err = AddName(datahash, t, m.data.n_data, 0))
      return err;
  }

  if(MatchTokens<WatTokens::OPEN, WatTokens::MEMORY>(tokens))
  {
    d.flags |= WASM_DATA_HAS_INDEX;
    d.index = GetFromHash(memoryhash, tokens.Pop());
    if(d.index == ~0u)
      return ERR_WAT_INVALID_VAR;
    EXPECTED(env, tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }

  // The next field that could appear would be the offset, which
  // is the only valid next expression that starts with OPEN
  if(tokens[0].id == WatTokens::OPEN)
  {
    bool offset = MatchTokens<WatTokens::OPEN, WatTokens::OFFSET>(tokens);
    if(err = ParseInitializerInstruction(tokens, d.offset, !offset))
      return err;
    if(offset)
      EXPECTED(env, tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }
  // Otherwise this must be a passive segment
  else
  {
    d.flags |= WASM_DATA_PASSIVE;
  }

  while(tokens[0].id != WatTokens::CLOSE)
  {
    if(tokens[0].id != WatTokens::STRING)
      return ERR_WAT_EXPECTED_STRING;
    if(err = WatString(env, d.data, tokens.Pop()))
      return err;
  }

  m.knownsections |= (1 << WASM_SECTION_DATA);
  return AppendArray(env, d, m.data.data, m.data.n_data);
}

// Skips over an entire section of tokens by counting paranthesis, assuming they are well-formed
void innative::WatSkipSection(Queue<WatToken>& tokens, ptrdiff_t count)
{
  while(tokens.Size())
  {
    if(tokens[0].id == WatTokens::OPEN)
      ++count;
    else if(tokens[0].id == WatTokens::CLOSE)
    {
      if(!--count)
        break; // Deliberately do not pop the CLOSE token because we usually need it afterwards
    }
    tokens.Pop();
  }
}

int WatParser::ParseModule(Environment& env, Module& m, const char* file, Queue<WatToken>& tokens, StringSpan name,
                           WatToken& internalname)
{
  int err;
  m = { WASM_MAGIC_COOKIE, WASM_MAGIC_VERSION, 0 };
  if(name.s && (err = ParseName(env, m.name, WatToken{ WatTokens::NAME, name.s, 0, 0, (int64_t)name.len })))
    return err;

  if((tokens.Peek().id == WatTokens::NAME) && (err = ParseName(env, m.name, internalname = tokens.Pop())))
    return err;

  WatParser state(env, m);

  WatToken t;
  size_t restore = tokens.GetPosition();
  while(tokens.Size() > 0 && tokens.Peek().id != WatTokens::CLOSE)
  {
    EXPECTED(env, tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
    t = tokens.Pop();
    switch(t.id) // This initial pass is for types and function types only
    {
    case WatTokens::TYPE:
      if(err = state.ParseIndexProcess<&WatParser::ParseFunctionType>(tokens))
        return err;
      break;
    default: WatSkipSection(tokens); break;
    }
    EXPECTED(env, tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }

  // This is the main pass for functions/imports/etc. and also identifies illegal tokens
  tokens.SetPosition(restore);
  while(tokens.Size() > 0 && tokens.Peek().id != WatTokens::CLOSE)
  {
    EXPECTED(env, tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
    t = tokens.Pop();
    switch(t.id)
    {
    case WatTokens::FUNC:
    {
      khiter_t iter  = kh_end(state.funchash);
      WatToken fname = GetWatNameToken(tokens);
      if(fname.id == WatTokens::NAME)
      {
        int r;
        iter = kh_put_indexname(state.funchash, StringSpan{ fname.pos, fname.len }, &r);
        if(!r)
          return ERR_WAT_DUPLICATE_NAME;
      }

      StringSpan ref = { nullptr, 0 };
      if(iter != kh_end(state.funchash))
        ref = { kh_key(state.funchash, iter).s, kh_key(state.funchash, iter).len };
      varuint32 index;
      if(err = state.ParseFunction(tokens, &index, ref))
        return err;

      if(fname.id == WatTokens::NAME)
      {
        if(index < m.importsection.functions)
          state.ParseName(env, m.importsection.imports[index].func_desc.debug.name, fname);
        else if(index - m.importsection.functions < m.function.n_funcdecl)
          state.ParseName(env, m.function.funcdecl[index - m.importsection.functions].debug.name, fname);
      }

      if(iter != kh_end(state.funchash))
        kh_val(state.funchash, iter) = index;
      break;
    }
    case WatTokens::IMPORT:
      if(err = state.ParseImport(tokens))
        return err;
      break;
    case WatTokens::TABLE:
      if(err = state.ParseIndexProcess<&WatParser::ParseTable>(tokens))
        return err;
      break;
    case WatTokens::MEMORY:
      if(err = state.ParseIndexProcess<&WatParser::ParseMemory>(tokens))
        return err;
      break;
    case WatTokens::GLOBAL:
      if(err = state.ParseIndexProcess<&WatParser::ParseGlobal>(tokens))
        return err;
      break;
    case WatTokens::EXPORT:
    case WatTokens::TYPE:
    case WatTokens::ELEM:
    case WatTokens::DATA:
    case WatTokens::START: WatSkipSection(tokens); break;
    default: return ERR_WAT_INVALID_TOKEN;
    }
    EXPECTED(env, tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }

  // This pass resolves exports, elem, data, and the start function, to minimize deferred actions
  tokens.SetPosition(restore);
  while(tokens.Size() > 0 && tokens.Peek().id != WatTokens::CLOSE)
  {
    EXPECTED(env, tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
    t = tokens.Pop();
    switch(t.id)
    {
    case WatTokens::EXPORT:
      if(err = state.ParseExport(tokens))
        return err;
      break;
    case WatTokens::ELEM:
    {
      TableInit init = { 0 };
      if(err = state.ParseElem(init, tokens))
        return err;
      break;
    }
    case WatTokens::DATA:
      if(err = state.ParseData(tokens))
        return err;
      break;
    case WatTokens::START:
      if(m.knownsections & (1 << WASM_SECTION_START))
        return ERR_FATAL_MULTIPLE_START_SECTIONS;
      m.knownsections |= (1 << WASM_SECTION_START);
      if(tokens[0].id != WatTokens::NUMBER && tokens[0].id != WatTokens::NAME)
        return ERR_WAT_EXPECTED_VAR;
      m.start_line = tokens[0].line;
      m.start      = state.GetFromHash(state.funchash, tokens.Pop());
      if(m.start == (varuint32)~0)
        return ERR_WAT_INVALID_VAR;
      break;
    default: WatSkipSection(tokens); break;
    }
    EXPECTED(env, tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }

  auto procRef = [](WatParser& s, Module& mod, varuint32 e, int imm) {
    // the range check for e is in the validation step, but we still have to catch a hash lookup failure
    if(e == (varuint32)~0)
      return ERR_INVALID_FUNCTION_INDEX;
    if(s.deferred[0].func < mod.importsection.functions ||
       s.deferred[0].func >= mod.code.n_funcbody + mod.importsection.functions)
      return ERR_INVALID_FUNCTION_INDEX;
    auto& f = mod.code.funcbody[s.deferred[0].func - mod.importsection.functions];
    if(s.deferred[0].index >= f.n_body)
      return ERR_INVALID_FUNCTION_BODY;
    f.body[s.deferred[0].index].immediates[imm]._varuint32 = e;
    return ERR_SUCCESS;
  };

  // Process all deferred actions
  while(state.deferred.Size() > 0)
  {
    switch(state.deferred[0].id)
    {
    case -static_cast<int>(WatTokens::ELEM):
    {
      size_t cache = tokens.GetPosition();
      tokens.SetPosition(static_cast<size_t>(state.deferred[0].func));

      TableInit init = { 0 };
      init.index     = static_cast<decltype(init.index)>(state.deferred[0].index);
      init.offset    = Instruction{ OP_i32_const, 0 };

      EXPECTED(env, tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
      EXPECTED(env, tokens, WatTokens::ELEM, ERR_WAT_EXPECTED_ELEM);
      if(err = state.ParseElemList(init, tokens))
        return err;
      state.m.knownsections |= 1 << WASM_SECTION_ELEMENT;
      if(err = AppendArray(env, init, state.m.element.elements, state.m.element.n_elements))
        return err;
      EXPECTED(env, tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);

      state.m.table.tables[state.deferred[0].index].resizable.minimum = init.n_elements;
      tokens.SetPosition(cache);
    }
    break;
    case OP_global_get:
    case OP_global_set: err = procRef(state, m, state.GetFromHash(state.globalhash, state.deferred[0].t), 0); break;
    case OP_call: err = procRef(state, m, state.GetFromHash(state.funchash, state.deferred[0].t), 0); break;
    case OP_memory_init: err = procRef(state, m, state.GetFromHash(state.datahash, state.deferred[0].t), 1); break;
    case OP_data_drop: err = procRef(state, m, state.GetFromHash(state.datahash, state.deferred[0].t), 0); break;
    case OP_table_init: err = procRef(state, m, state.GetFromHash(state.elemhash, state.deferred[0].t), 1); break;
    case OP_elem_drop: err = procRef(state, m, state.GetFromHash(state.elemhash, state.deferred[0].t), 0); break;
    default: return ERR_WAT_INVALID_TOKEN;
    }
    if(err)
      return err;
    state.deferred.Pop();
  }

  // Manually insert the DataCount section
  m.knownsections |= (1 << WASM_SECTION_DATA_COUNT);
  m.data_count.count = m.data.n_data;

  m.filepath = utility::AllocString(env, file);
  m.exports  = kh_init_exports();
  assert(m.name.get());
  return ParseExportFixup(m, env.errors, env);
}

int innative::ParseWatModule(Environment& env, const char* file, Module& m, const uint8_t* data, size_t sz, StringSpan name)
{
  Queue<WatToken> tokens;
  TokenizeWAT(tokens, reinterpret_cast<const char*>(data), reinterpret_cast<const char*>(data) + sz);
  WatToken nametoken;

  if(!tokens.Size())
    return ERR_FATAL_INVALID_MODULE;

  int err = CheckWatTokens(env, env.errors, tokens, reinterpret_cast<const char*>(data));
  if(err < 0)
    return err;

  // If we don't detect "(module", just assume it's an inline module
  if(tokens[0].id != WatTokens::OPEN || tokens[1].id != WatTokens::MODULE)
    err = WatParser::ParseModule(env, m, file, tokens, name, nametoken);
  else
  {
    if(tokens.Size() == 0 || tokens.Pop().id != WatTokens::OPEN)
      err = ERR_WAT_EXPECTED_OPEN;
    else if(tokens.Size() == 0 || tokens.Pop().id != WatTokens::MODULE)
      err = ERR_WAT_EXPECTED_MODULE;
    else if(!(err = WatParser::ParseModule(env, m, file, tokens, name, nametoken)))
    {
      if(tokens.Size() == 0 || tokens.Pop().id != WatTokens::CLOSE)
        err = ERR_WAT_EXPECTED_CLOSE;
    }
  }

  if(err < 0)
    AppendError(env, env.errors, &m, err, "[%s:%zu]", m.name.str(),
                WatLineNumber(reinterpret_cast<const char*>(data), tokens.Peek().pos));
  return err;
}

size_t innative::WatLineNumber(const char* start, const char* pos)
{
  size_t count = 1;
  while(start < pos)
  {
    if(*(start++) == '\n')
      ++count;
  }
  return count;
}
