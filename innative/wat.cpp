// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "wat.h"
#include "utility.h"
#include "parse.h"
#include "validate.h"
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
}
WatParser::~WatParser()
{
  kh_destroy_indexname(typehash);
  kh_destroy_indexname(funchash);
  kh_destroy_indexname(tablehash);
  kh_destroy_indexname(memoryhash);
  kh_destroy_indexname(globalhash);
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
  static const uint32_t UNI_REPLACEMENT_CHAR = 0x0000FFFD;
  static const uint32_t UNI_MAX_LEGAL_UTF32  = 0x0010FFFF;
  static const uint8_t firstByteMark[7]      = { 0x00, 0x00, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC };
  static const uint32_t byteMask             = 0xBF;
  static const uint32_t byteMark             = 0x80;

  int bytesToWrite;
  if(ch < 0x80)
    bytesToWrite = 1;
  else if(ch < 0x800)
    bytesToWrite = 2;
  else if(ch < 0x10000)
    bytesToWrite = 3;
  else if(ch <= UNI_MAX_LEGAL_UTF32)
    bytesToWrite = 4;
  else
  {
    bytesToWrite = 3;
    ch           = UNI_REPLACEMENT_CHAR;
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

int WatParser::ParseName(const Environment& env, ByteArray& name, const WatToken& t)
{
  if(t.id != WatTokens::NAME || !t.pos || !t.len || t.len > numeric_limits<varuint32>::max())
    return ERR_PARSE_INVALID_NAME;

  name.resize(static_cast<varuint32>(t.len), true, env);
  if(!name.get())
    return ERR_FATAL_OUT_OF_MEMORY;
  tmemcpy(reinterpret_cast<char*>(name.get()), name.size(), t.pos, t.len);

  return ERR_SUCCESS;
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

int WatParser::AddWatValType(const Environment& env, WatTokens id, varsint7*& a, varuint32& n)
{
  varsint7 ty = WatValType(id);
  if(!ty)
    return ERR_WAT_EXPECTED_VALTYPE;
  return AppendArray<varsint7>(env, ty, a, n);
}

int WatParser::ParseFunctionTypeInner(const Environment& env, Queue<WatToken>& tokens, FunctionType& sig, DebugInfo** info,
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
    EXPECTED(tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
    EXPECTED(tokens, WatTokens::PARAM, ERR_WAT_EXPECTED_TOKEN);

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

    EXPECTED(tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }

  while(tokens.Size() > 1 && tokens[0].id == WatTokens::OPEN && tokens[1].id == WatTokens::RESULT)
  {
    EXPECTED(tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
    EXPECTED(tokens, WatTokens::RESULT, ERR_WAT_EXPECTED_TOKEN);

    while(tokens.Peek().id != WatTokens::CLOSE)
      if(err = AddWatValType(env, tokens.Pop().id, sig.returns, sig.n_returns))
        return err;

    EXPECTED(tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }

  // We detect this special case because otherwise it can turn into a "type mismatch" error, which is very confusing
  if(tokens.Size() > 1 && tokens[0].id == WatTokens::OPEN && tokens[1].id == WatTokens::PARAM)
    return ERR_WAT_PARAM_AFTER_RESULT;
  return ERR_SUCCESS;
}

int WatParser::ParseFunctionType(Queue<WatToken>& tokens, varuint32* index)
{
  EXPECTED(tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
  EXPECTED(tokens, WatTokens::FUNC, ERR_WAT_EXPECTED_FUNC);

  FunctionType sig = { 0 };
  int err          = ParseFunctionTypeInner(env, tokens, sig, 0, 0, false);
  if(err != 0)
    return err;
  *index = m.type.n_functypes;
  m.knownsections |= (1 << WASM_SECTION_TYPE);
  if(err = AppendArray<FunctionType>(env, sig, m.type.functypes, m.type.n_functypes))
    return err;

  EXPECTED(tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
  return ERR_SUCCESS;
}

int WatParser::AppendImport(Module& m, const Import& i, varuint32* index)
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

int WatParser::ParseTypeUse(Queue<WatToken>& tokens, varuint32& sig, DebugInfo** info, varuint32* n_info, bool anonymous)
{
  sig = (varuint32)~0;
  if(tokens.Size() > 1 && tokens[0].id == WatTokens::OPEN && tokens[1].id == WatTokens::TYPE)
  {
    EXPECTED(tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
    EXPECTED(tokens, WatTokens::TYPE, ERR_WAT_EXPECTED_TYPE);

    if(tokens.Peek().id != WatTokens::NUMBER && tokens.Peek().id != WatTokens::NAME)
      return ERR_WAT_EXPECTED_VAR;

    sig = GetFromHash(typehash, tokens.Pop());

    if(sig >= m.type.n_functypes)
      AppendError(env, env.errors, &m, ERR_WAT_UNKNOWN_TYPE, "Invalid type signature %u", sig);

    EXPECTED(tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }

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

int WatParser::ParseConstantOperator(Queue<WatToken>& tokens, Instruction& op)
{
  int err = ERR_SUCCESS;
  switch(op.opcode)
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
    return ERR_WAT_EXPECTED_OPERATOR;

  int err;
  if(tokens.Peek().i > 0xFF)
    return ERR_WAT_OUT_OF_RANGE;
  op        = { (uint8_t)tokens.Peek().i };
  op.line   = tokens.Peek().line;
  op.column = tokens.Pop().column;

  switch(op.opcode)
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
  case OP_call: defer = WatParser::DeferWatAction{ op.opcode, tokens.Pop(), 0, 0 }; break;
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

    break;
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

int WatParser::ParseBlockType(Queue<WatToken>& tokens, varsint7& out)
{
  out = TE_void;
  if(tokens.Size() > 1 && tokens[0].id == WatTokens::OPEN && tokens[1].id == WatTokens::RESULT)
  {
    EXPECTED(tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
    EXPECTED(tokens, WatTokens::RESULT, ERR_WAT_EXPECTED_RESULT);

    if(tokens.Peek().id != WatTokens::CLOSE)
    {
      if(!(out = WatValType(tokens.Pop().id)))
        return ERR_WAT_EXPECTED_VALTYPE;

      if(tokens.Peek().id != WatTokens::CLOSE)
        return ERR_MULTIPLE_RETURN_VALUES;
    }

    EXPECTED(tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }

  if(tokens.Size() > 1 && tokens[0].id == WatTokens::OPEN && tokens[1].id == WatTokens::RESULT)
    return ERR_MULTIPLE_RETURN_VALUES;
  return ERR_SUCCESS;
}

int WatParser::ParseExpression(Queue<WatToken>& tokens, FunctionBody& f, FunctionDesc& desc, FunctionType& sig,
                               varuint32 index)
{
  EXPECTED(tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);

  int err;
  varsint7 blocktype;
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

      op.immediates[0]._varsint7 = blocktype;
      op.line                    = t.line;
      op.column                  = t.column;
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

      op.immediates[0]._varsint7 = blocktype;
      op.line                    = t.line;
      op.column                  = t.column;
      if(err = AppendArray<Instruction>(env, op, f.body,
                                        f.n_body)) // We append the if instruction _after_ the optional condition expression
        return err;
    }
  }

    EXPECTED(tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN); // There must always be a Then branch
    EXPECTED(tokens, WatTokens::THEN, ERR_WAT_EXPECTED_THEN);

    while(tokens.Peek().id != WatTokens::CLOSE)
      if(err = ParseInstruction(tokens, f, desc, sig, index))
        return err;

    EXPECTED(tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);

    if(tokens.Peek().id == WatTokens::OPEN) // Must be an else branch if it exists
    {
      EXPECTED(tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);

      WatToken t     = tokens.Peek();
      Instruction op = { OP_else };
      EXPECTED(tokens, WatTokens::ELSE, ERR_WAT_EXPECTED_ELSE);

      op.line   = t.line;
      op.column = t.column;
      if(err = AppendArray<Instruction>(env, op, f.body, f.n_body))
        return err;

      while(tokens.Peek().id != WatTokens::CLOSE)
        if(err = ParseInstruction(tokens, f, desc, sig, index))
          return err;

      EXPECTED(tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
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

  EXPECTED(tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
  return ERR_SUCCESS;
}

int WatParser::ParseInstruction(Queue<WatToken>& tokens, FunctionBody& f, FunctionDesc& desc, FunctionType& sig,
                                varuint32 index)
{
  int err;
  varsint7 blocktype;
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

      op.immediates[0]._varsint7 = blocktype;
      op.line                    = t.line;
      op.column                  = t.column;
      if(err = AppendArray<Instruction>(env, op, f.body, f.n_body))
        return err;
    }

    while(tokens.Peek().id != WatTokens::END)
      if(err = ParseInstruction(tokens, f, desc, sig, index))
        return err;

    EXPECTED(tokens, WatTokens::END, ERR_WAT_EXPECTED_END);

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

      op.immediates[0]._varsint7 = blocktype;
      op.line                    = t.line;
      op.column                  = t.column;
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

      EXPECTED(tokens, WatTokens::END, ERR_WAT_EXPECTED_END);
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

int WatParser::InlineImportExport(const Environment& env, Module& m, Queue<WatToken>& tokens, varuint32* index,
                                  varuint7 kind, Import** out)
{
  int err;
  while(tokens.Size() > 1 && tokens[0].id == WatTokens::OPEN && tokens[1].id == WatTokens::EXPORT)
  {
    EXPECTED(tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
    EXPECTED(tokens, WatTokens::EXPORT, ERR_WAT_EXPECTED_EXPORT);

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
    EXPECTED(tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }
  if(tokens.Size() > 1 && tokens[0].id == WatTokens::OPEN && tokens[1].id == WatTokens::IMPORT)
  {
    EXPECTED(tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
    EXPECTED(tokens, WatTokens::IMPORT, ERR_WAT_EXPECTED_IMPORT);

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
    if(err = AppendImport(m, i, index))
      return err;

    switch(i.kind) // Fix the index
    {
    case WASM_KIND_FUNCTION: *out = m.importsection.imports + *index; break;
    case WASM_KIND_TABLE: *out = m.importsection.imports + m.importsection.functions + *index; break;
    case WASM_KIND_MEMORY: *out = m.importsection.imports + m.importsection.tables + *index; break;
    case WASM_KIND_GLOBAL: *out = m.importsection.imports + m.importsection.memories + *index; break;
    }

    EXPECTED(tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
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

  *index    = m.function.n_funcdecl + m.importsection.functions;
  Import* i = 0;
  if(err = InlineImportExport(env, m, tokens, index, WASM_KIND_FUNCTION, &i))
    return err;

  if(i) // If this is an import, assemble the aux information and abort.
    return ParseTypeUse(tokens, i->func_desc.type_index, &i->func_desc.param_debug, 0, false);

  FunctionDesc desc = { 0 };
  desc.debug.line   = body.line;
  desc.debug.column = body.column;
  if(err = ParseTypeUse(tokens, desc.type_index, &desc.param_debug, 0, false))
    return err;

  FunctionType& functy = m.type.functypes[desc.type_index];
  if(name.len > 0)
    if(err = WatString(env, desc.debug.name, name))
      return err;

  // Read in all the locals
  while(tokens.Size() > 1 && tokens[0].id == WatTokens::OPEN && tokens[1].id == WatTokens::LOCAL)
  {
    WatToken src = tokens[1];
    EXPECTED(tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
    EXPECTED(tokens, WatTokens::LOCAL, ERR_WAT_EXPECTED_LOCAL);

    FunctionLocal local;
    local.debug = { src.line, src.column };
    local.count = 1;

    if(tokens.Peek().id == WatTokens::NAME)
    {
      if(tokens.Peek().len > numeric_limits<varuint32>::max())
        return ERR_WAT_OUT_OF_RANGE;
      ParseName(env, local.debug.name, tokens.Pop());

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

    EXPECTED(tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
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
    limits.flags = 1;
  }

  return ERR_SUCCESS;
}

int WatParser::ParseTableDesc(TableDesc& t, Queue<WatToken>& tokens)
{
  int err;
  if(err = ParseResizableLimits(t.resizable, tokens))
    return err;

  EXPECTED(tokens, WatTokens::FUNCREF, ERR_WAT_EXPECTED_FUNCREF);

  t.element_type = TE_funcref;
  return ERR_SUCCESS;
}

int WatParser::ParseTable(Queue<WatToken>& tokens, varuint32* index)
{
  int err;
  *index    = m.table.n_tables + m.importsection.tables - m.importsection.functions;
  Import* i = 0;
  if(err = InlineImportExport(env, m, tokens, index, WASM_KIND_TABLE, &i))
    return err;

  if(i) // If this is an import, assemble the aux information and abort.
    return ParseTableDesc(i->table_desc, tokens);

  TableDesc table = { 0 };
  switch(tokens.Peek().id)
  {
  case WatTokens::NUMBER:
    if(err = ParseTableDesc(table, tokens))
      return err;
    break;
  default:
    EXPECTED(tokens, WatTokens::FUNCREF, ERR_WAT_EXPECTED_FUNCREF);

    table.element_type    = TE_funcref;
    table.resizable.flags = 0;
    deferred.Push(
      WatParser::DeferWatAction{ -static_cast<int>(WatTokens::ELEM), { WatTokens::NONE }, tokens.GetPosition(), *index });
    WatSkipSection(tokens); // Defer element section to after we know we've loaded everything.
  }

  m.knownsections |= (1 << WASM_SECTION_TABLE);
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
      switch(blank.body[i].opcode)
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
    EXPECTED(tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
    EXPECTED(tokens, WatTokens::MUT, ERR_WAT_EXPECTED_MUT);
    g.mutability = true;
    if(!(g.type = WatValType(tokens.Pop().id)))
      return ERR_WAT_EXPECTED_VALTYPE;

    EXPECTED(tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }
  else
  {
    g.mutability = false;
    if(!(g.type = WatValType(tokens.Pop().id)))
      return ERR_WAT_EXPECTED_VALTYPE;
  }

  return ERR_SUCCESS;
}

int WatParser::ParseGlobal(Queue<WatToken>& tokens, varuint32* index)
{
  int err;
  *index    = m.global.n_globals + m.importsection.globals - m.importsection.memories;
  Import* i = 0;
  if(err = InlineImportExport(env, m, tokens, index, WASM_KIND_GLOBAL, &i))
    return err;

  if(i) // If this is an import, assemble the aux information and abort.
    return ParseGlobalDesc(i->global_desc, tokens);

  GlobalDecl g = { 0 };
  if(err = ParseGlobalDesc(g.desc, tokens))
    return err;

  if(err = ParseInitializer(tokens, g.init))
    return err;

  m.knownsections |= (1 << WASM_SECTION_GLOBAL);
  return AppendArray(env, g, m.global.globals, m.global.n_globals);
}

int WatParser::ParseMemoryDesc(MemoryDesc& m, Queue<WatToken>& tokens) { return ParseResizableLimits(m.limits, tokens); }

int WatParser::ParseMemory(Queue<WatToken>& tokens, varuint32* index)
{
  int err;
  *index    = m.memory.n_memories + m.importsection.memories - m.importsection.tables;
  Import* i = 0;
  if(err = InlineImportExport(env, m, tokens, index, WASM_KIND_MEMORY, &i))
    return err;

  if(i) // If this is an import, assemble the aux information and abort.
    return ParseMemoryDesc(i->mem_desc, tokens);

  MemoryDesc mem = { 0 };

  if(tokens.Size() > 1 && tokens[0].id == WatTokens::OPEN && tokens[1].id == WatTokens::DATA)
  {
    EXPECTED(tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
    EXPECTED(tokens, WatTokens::DATA, ERR_WAT_EXPECTED_TOKEN);
    DataInit init = { 0 };
    init.index    = *index;
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
    mem.limits.minimum = init.data.size();
    EXPECTED(tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }
  else if(err = ParseMemoryDesc(mem, tokens))
    return err;

  m.knownsections |= (1 << WASM_SECTION_MEMORY);
  return AppendArray(env, mem, m.memory.memories, m.memory.n_memories);
}

int WatParser::AddName(kh_indexname_t* h, WatToken t, varuint32 index)
{
  if(t.id == WatTokens::NAME)
  {
    int r;
    khiter_t iter = kh_put_indexname(h, StringSpan{ t.pos, t.len }, &r);
    if(!r)
      return ERR_WAT_DUPLICATE_NAME;
    if(iter != kh_end(h))
      kh_val(h, iter) = index;
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

  EXPECTED(tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);

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
    break;
  case WatTokens::TABLE:
    i.kind = WASM_KIND_TABLE;
    if(err = ParseTableDesc(i.table_desc, tokens))
      return err;
    hash = tablehash;
    break;
  case WatTokens::MEMORY:
    i.kind = WASM_KIND_MEMORY;
    if(err = ParseMemoryDesc(i.mem_desc, tokens))
      return err;
    hash = memoryhash;
    break;
  default: return ERR_WAT_EXPECTED_KIND;
  }
  EXPECTED(tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);

  varuint32 index;
  if(err = AppendImport(m, i, &index))
    return err;

  return AddName(hash, name, index);
}

int WatParser::ParseExport(Queue<WatToken>& tokens)
{
  Export e = {};
  int err;
  if(err = WatString(env, e.name, tokens.Pop()))
    return err;

  EXPECTED(tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
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
  EXPECTED(tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);

  m.knownsections |= (1 << WASM_SECTION_EXPORT);
  return AppendArray(env, e, m.exportsection.exports, m.exportsection.n_exports);
}

int WatParser::ParseElemData(Queue<WatToken>& tokens, varuint32& index, Instruction& op, kh_indexname_t* hash)
{
  if(tokens[0].id == WatTokens::NUMBER || tokens[0].id == WatTokens::NAME)
    index = GetFromHash(hash, tokens.Pop());

  if(index == (varuint32)~0)
    return ERR_WAT_INVALID_VAR;

  if(tokens[0].id == WatTokens::OPEN)
  {
    bool offset = tokens.Size() > 1 && tokens[0].id == WatTokens::OPEN && tokens[1].id == WatTokens::OFFSET;
    if(offset)
    {
      EXPECTED(tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
      EXPECTED(tokens, WatTokens::OFFSET, ERR_WAT_EXPECTED_TOKEN);
    }

    int err = ParseInitializerInstruction(tokens, op, !offset); // Without an offset wrapper, only an expression is allowed
    if(err < 0)
      return err;

    if(offset)
      EXPECTED(tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }

  return ERR_SUCCESS;
}

int WatParser::ParseElem(TableInit& e, Queue<WatToken>& tokens)
{
  while(tokens[0].id != WatTokens::CLOSE)
  {
    int err = AppendArray(env, GetFromHash(funchash, tokens.Pop()), e.elements, e.n_elements);
    if(err)
      return err;
    if(e.elements[e.n_elements - 1] == (varuint32)~0)
      return ERR_WAT_INVALID_VAR;
  }

  m.knownsections |= (1 << WASM_SECTION_ELEMENT);
  return AppendArray(env, e, m.element.elements, m.element.n_elements);
}

int WatParser::ParseData(Queue<WatToken>& tokens)
{
  DataInit d = { 0 };
  int err;
  if(err = ParseElemData(tokens, d.index, d.offset, memoryhash))
    return err;

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
    EXPECTED(tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
    t = tokens.Pop();
    switch(t.id) // This initial pass is for types and function types only
    {
    case WatTokens::TYPE:
      if(err = state.ParseIndexProcess<&WatParser::ParseFunctionType>(tokens, state.typehash))
        return err;
      break;
    default: WatSkipSection(tokens); break;
    }
    EXPECTED(tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }

  // This is the main pass for functions/imports/etc. and also identifies illegal tokens
  tokens.SetPosition(restore);
  while(tokens.Size() > 0 && tokens.Peek().id != WatTokens::CLOSE)
  {
    EXPECTED(tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
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
      if(err = state.ParseIndexProcess<&WatParser::ParseTable>(tokens, state.tablehash))
        return err;
      break;
    case WatTokens::MEMORY:
      if(err = state.ParseIndexProcess<&WatParser::ParseMemory>(tokens, state.memoryhash))
        return err;
      break;
    case WatTokens::GLOBAL:
      if(err = state.ParseIndexProcess<&WatParser::ParseGlobal>(tokens, state.globalhash))
        return err;
      break;
    case WatTokens::EXPORT:
    case WatTokens::TYPE:
    case WatTokens::ELEM:
    case WatTokens::DATA:
    case WatTokens::START: WatSkipSection(tokens); break;
    default: return ERR_WAT_INVALID_TOKEN;
    }
    EXPECTED(tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }

  // This pass resolves exports, elem, data, and the start function, to minimize deferred actions
  tokens.SetPosition(restore);
  while(tokens.Size() > 0 && tokens.Peek().id != WatTokens::CLOSE)
  {
    EXPECTED(tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
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
      if(err = state.ParseElemData(tokens, init.index, init.offset, state.tablehash))
        return err;
      if(err = state.ParseElem(init, tokens))
        return err;
      break;
    }
    case WatTokens::DATA:
      if(err = state.ParseData(tokens))
        return err;
      break;
    case WatTokens::START:
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
    EXPECTED(tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }

  auto procRef = [](WatParser& s, Module& mod, varuint32 e) {
    if(s.deferred[0].func < mod.importsection.functions ||
       s.deferred[0].func >= mod.code.n_funcbody + mod.importsection.functions)
      return ERR_INVALID_FUNCTION_INDEX;
    auto& f = mod.code.funcbody[s.deferred[0].func - mod.importsection.functions];
    if(s.deferred[0].index >= f.n_body)
      return ERR_INVALID_FUNCTION_BODY;
    f.body[s.deferred[0].index].immediates[0]._varuint32 = e;
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

      EXPECTED(tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
      EXPECTED(tokens, WatTokens::ELEM, ERR_WAT_EXPECTED_ELEM);
      err = state.ParseElem(init, tokens);
      EXPECTED(tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);

      state.m.table.tables[state.deferred[0].index].resizable.minimum = init.n_elements;
      tokens.SetPosition(cache);
    }
    break;
    case OP_global_get:
    case OP_global_set: err = procRef(state, m, state.GetFromHash(state.globalhash, state.deferred[0].t)); break;
    case OP_call: err = procRef(state, m, state.GetFromHash(state.funchash, state.deferred[0].t)); break;
    default: return ERR_WAT_INVALID_TOKEN;
    }
    if(err)
      return err;
    state.deferred.Pop();
  }

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