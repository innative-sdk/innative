// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in native-wasm.h

#include "wat.h"
#include "trex.h"
#include "util.h"
#include "stack.h"
#include "parse.h"
#include "validate.h"
#include <vector>

#define LEXER_DIGIT "0-9"
#define LEXER_HEXDIGIT "0-9A-Fa-f"
#define LEXER_LETTER "A-Za-z"
#define LEXER_NUM "([" LEXER_DIGIT "](_?[" LEXER_DIGIT "])*)"
#define LEXER_HEXNUM "[" LEXER_HEXDIGIT "](_?[" LEXER_HEXDIGIT "])*"
#define LEXER_NAT "(" LEXER_NUM "|0x" LEXER_HEXNUM ")"
#define LEXER_INT LEXER_NAT "|+" LEXER_NAT "|-" LEXER_NAT
#define LEXER_FLOAT "(" LEXER_NUM "\\." LEXER_NUM "?(e|E " LEXER_NUM ")? | 0x" LEXER_HEXNUM "\\." LEXER_HEXNUM "?(p|P " LEXER_HEXNUM ")?)"
#define LEXER_NAME "\\$[" LEXER_LETTER LEXER_DIGIT "_.+\\-*/\\\\\\^~=<>!?@#$%&|:'`]+"

KHASH_INIT(tokens, kh_cstr_t, TokenID, 1, kh_str_hash_funcins, kh_str_hash_insequal)

static kh_inline khint_t kh_hash_token(const Token& t)
{
  const char* s = t.pos;
  const char* end = t.pos + t.len;
  khint_t h = (khint_t)*s;

  for(++s; s < end; ++s)
    h = (h << 5) - h + (khint_t)*s;

  return h;
}

static kh_inline bool kh_equal_token(const Token& a, const Token& b)
{
  if(a.len != b.len)
    return false;
  return !memcmp(a.pos, b.pos, a.len);
}

KHASH_INIT(indexname, Token, varuint32, 1, kh_hash_token, kh_equal_token)

struct DeferAction
{
  int id;
  Token t;
  uint64_t func;
  uint64_t index;
};

struct StringRef
{
  const char* s;
  size_t len;

  bool operator ==(const StringRef& r) const
  {
    if(len != r.len)
      return false;
    return !memcmp(s, r.s, len);
  }
};

struct WatState
{
  WatState(Module& mod) : m(mod)
  {
    typehash = kh_init_indexname();
    funchash = kh_init_indexname();
    tablehash = kh_init_indexname();
    memoryhash = kh_init_indexname();
    globalhash = kh_init_indexname();
  }
  ~WatState()
  {
    kh_destroy_indexname(typehash);
    kh_destroy_indexname(funchash);
    kh_destroy_indexname(tablehash);
    kh_destroy_indexname(memoryhash);
    kh_destroy_indexname(globalhash);
  }
  varuint7 GetJump(Token var)
  {
    if(var.id == TOKEN_INTEGER && var.i < std::numeric_limits<varuint7>::max())
      return (varuint7)var.i;
    if(var.id == TOKEN_NAME)
    {
      StringRef r = { var.pos, var.len };
      for(varuint7 i = 0; i < stack.Size(); ++i)
        if(stack[i] == r)
          return i;
    }

    return (varuint7)~0;
  }

  Module& m;
  Stack<DeferAction> defer;
  Stack<StringRef> stack;
  kh_indexname_t* typehash;
  kh_indexname_t* funchash;
  kh_indexname_t* tablehash;
  kh_indexname_t* memoryhash;
  kh_indexname_t* globalhash;
};

kh_tokens_t* GenTokenHash(std::initializer_list<const char*> list)
{
  kh_tokens_t* h = kh_init_tokens();

  TokenID count = 0;
  int r;
  for(const char* s : list)
  {
    auto iter = kh_put_tokens(h, s, &r);
    kh_val(h, iter) = ++count;
  }

  return h;
}

static const char* trex_err = 0;
static kh_tokens_t* tokenhash = GenTokenHash({ "(", ")", "module", "import", "type", "start", "func", "global", "table", "memory", "export",
  "data", "elem", "offset", "align", "local", "result", "param", "i32", "i64", "f32", "f64", "anyfunc", "mut", "block", "loop",
  "if", "then", "else", "end", /* script extensions */ "binary", "quote", "register", "invoke", "get", "assert_return",
  "assert_return_canonical_nan", "assert_return_arithmetic_nan", "assert_trap", "assert_malformed", "assert_invalid",
  "assert_unlinkable", "assert_trap", "script", "input", "output" });
static std::string numbuf;

TRex* regex_NAME = trex_compile("^" LEXER_NAME, &trex_err);
TRex* regex_INT = trex_compile("^" LEXER_INT, &trex_err);
TRex* regex_FLOAT = trex_compile("^" LEXER_FLOAT, &trex_err);

void TokenizeWAT(Stack<Token>& tokens, char* s)
{
  while(*s)
  {
    while(s[0] != 0 && (s[0] == ' ' || s[0] == '\n' || s[0] == '\r' || s[0] == '\t' || s[0] == '\f'))
      ++s;

    switch(s[0])
    {
    case 0:
      break;
    case '(':
      tokens.Push(Token{ TOKEN_OPEN, s });
      ++s;
      break;
    case ')':
      tokens.Push(Token{ TOKEN_CLOSE, s });
      ++s;
      break;
    case '"': // A string
    {
      const char* begin = s;
      while(s[0] != '"' && s[0] != 0)
        s += (s[0] == '\\' && s[1] == '"') ? 2 : 1;

      Token t = { TOKEN_STRING, begin };
      t.len = s - begin;
      tokens.Push(t);

      if(s[0] == '"')
        ++s;
      break;
    }
    case '$': // A name
    {
      const char* begin;
      const char* end;
      if(trex_search(regex_NAME, s, &begin, &end) == TRex_True)
      {
        assert(begin == s);

        Token t = { TOKEN_NAME, begin };
        t.len = end - begin;
        tokens.Push(t);
        s = const_cast<char*>(end);
        break;
      } // If the search fails, fall through and just start trying other regexes. It will eventually be classified as an invalid token.
    }
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9': // Either an integer or a float
    {
      const char* intbegin = 0;
      const char* floatbegin = 0;
      const char* end = 0;
      if(trex_search(regex_INT, s, &intbegin, &end) || trex_search(regex_FLOAT, s, &floatbegin, &end))
      {
        size_t len = end - s;
        numbuf.resize(len);
        int c = 0;
        for(int i = 0; i < len; ++i)
          if(s[i] != '_')
            numbuf[c++] = s[i];

        char* out;
        if(intbegin != nullptr)
          tokens.Push(Token{ TOKEN_INTEGER, s, (int64_t)strtoll(numbuf.data(), &out, 10) });
        else
        {
          Token t = { TOKEN_FLOAT, s };
          t.f = strtod(numbuf.data(), &out);
          tokens.Push(t);
        }

        s = const_cast<char*>(end);
        break;
      }
    }
    case ';': // A comment
    {
      char stop = (s[1] == ';') ? '\n' : ';'; // A ;; means we go until we fine a newline, a ; means we go until the next ;

      while(s[0] != 0 && s[0] != stop)
        ++s;
      if(s[0] != 0)
        ++s;
      break;
    }
    default:
    {
      const char* begin = s;

      while(s[0] != 0 && s[0] != ' ' && s[0] != '\n' && s[0] != '\r' && s[0] != '\t' && s[0] != '\f' && s[0] != '=')
        ++s;

      char old = s[0];
      s[0] = 0;
      khiter_t iter = kh_get_tokens(tokenhash, begin);
      if(kh_exist2(tokenhash, iter))
        tokens.Push(Token{ kh_val(tokenhash, iter), begin });
      else
      {
        byte op = GetInstruction(begin);
        if(op != 0xFF)
          tokens.Push(Token{ TOKEN_OPERATOR, begin, (int64_t)op });
        else
        {
          assert(false);
          tokens.Push(Token{ TOKEN_NONE });
        }
      }

      if(old != 0)
        ++s;
    }
    }
  }
}

#define EXPECTED(t, e, err) if((t).Size() == 0 || (t).Pop().id != (e)) return (err)

int WatString(ByteArray& str, StringRef t)
{
  if(!t.s || !t.len)
    return ERR_PARSE_INVALID_NAME;

  if(str.bytes)
  {
    uint8_t* n = tmalloc<uint8_t>(str.n_bytes + t.len + 1);
    memcpy(n, str.bytes, str.n_bytes);
    str.bytes = n;
  }
  else
    str.bytes = tmalloc<uint8_t>(t.len + 1);

  if(!str.bytes)
    return ERR_FATAL_OUT_OF_MEMORY;

  for(int i = 0; i < t.len; ++i)
  {
    if(t.s[i] == '\\')
    {
      switch(t.s[++i])
      {
      case 'n':
        str.bytes[str.n_bytes++] = '\n';
        break;
      case 't':
        str.bytes[str.n_bytes++] = '\t';
        break;
      case '\\':
        str.bytes[str.n_bytes++] = '\\';
        break;
      case '\'':
        str.bytes[str.n_bytes++] = '\'';
        break;
      case '"':
        str.bytes[str.n_bytes++] = '"';
        break;
      case 'u':
        // TODO: evaluate unicode
      default:
        // TODO: evaluate unicode
        assert(false);
        break;
      }
    }
    else
      str.bytes[str.n_bytes++] = t.s[i];
  }
  str.bytes[str.n_bytes] = 0;

  return ERR_SUCCESS;
}

// A string is similar to a name, but we must resolve escaped characters and hex codes
NW_FORCEINLINE int WatString(ByteArray& str, const Token& t)
{
  if(t.id != TOKEN_STRING)
    return ERR_WAT_EXPECTED_STRING;
  return WatString(str, StringRef{ t.pos, t.len });
}

int WatName(ByteArray& name, const Token& t)
{
  if(t.id != TOKEN_NAME || !t.pos || !t.len)
    return ERR_PARSE_INVALID_NAME;

  name.bytes = tmalloc<uint8_t>(t.len + 1);
  if(!name.bytes || t.len > std::numeric_limits<varuint32>::max())
    return ERR_FATAL_OUT_OF_MEMORY;
  name.n_bytes = (varuint32)t.len;
  memcpy(name.bytes, t.pos, t.len);
  name.bytes[name.n_bytes] = 0;

  return ERR_SUCCESS;
}

khiter_t AddWatName(kh_indexname_t* h, Stack<Token>& tokens, int* r)
{
  if(tokens.Peek().id == TOKEN_NAME)
    return kh_put_indexname(h, tokens.Pop(), r);
  return kh_end(h);
}

template<class T>
int AppendArray(T item, T*& a, varuint32& n)
{
  if(!(a = (T*)realloc(a, ++n)))
    return ERR_FATAL_OUT_OF_MEMORY;
  a[n - 1] = item;
  return ERR_SUCCESS;
}

varsint7 WatValType(TokenID id)
{
  switch(id)
  {
  case TOKEN_i32: return TE_i32;
  case TOKEN_i64: return TE_i64;
  case TOKEN_f32: return TE_f32;
  case TOKEN_f64: return TE_f64;
  }

  return 0;
}

int AddWatValType(TokenID id, varsint7*& a, varuint32& n)
{
  varsint7 ty = WatValType(id);
  if(!ty)
    return ERR_WAT_EXPECTED_VALTYPE;
  return AppendArray<varsint7>(ty, a, n);
}

int WatTypeInner(Stack<Token>& tokens, FunctionSig& sig, const char*** names)
{
  sig.form = TE_func;
  int r;
  while(tokens.Peek().id == TOKEN_OPEN)
  {
    EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);

    switch(tokens.Pop().id)
    {
    case TOKEN_PARAM:
      if(tokens.Peek().id == TOKEN_NAME)
      {
        if(names) // You are legally allowed to put parameter names in typedefs in WAT, but the names are thrown away.
        {
          if(tokens.Peek().len >= std::numeric_limits<varuint32>::max())
            return ERR_WAT_OUT_OF_RANGE;
          varuint32 len = (varuint32)tokens.Peek().len;
          char* s = tmalloc<char>(len + 1);
          memcpy(s, tokens.Peek().pos, len);
          s[len] = 0;

          len = sig.n_params;
          if(r = AppendArray<const char*>(s, *names, len))
            return r;
        }
        tokens.Pop();
        if(r = AddWatValType(tokens.Pop().id, sig.params, sig.n_params))
          return r;
      }
      else
      {
        while(tokens.Peek().id != TOKEN_CLOSE)
          if(r = AddWatValType(tokens.Pop().id, sig.params, sig.n_params))
            return r;
      }
      break;
    case TOKEN_RESULT:
      while(tokens.Peek().id != TOKEN_CLOSE)
        if(r = AddWatValType(tokens.Pop().id, sig.returns, sig.n_returns))
          return r;
      break;
    default:
      return ERR_WAT_EXPECTED_TOKEN;
    }

    EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }

  return ERR_SUCCESS;
}

int WatType(WatState& state, Stack<Token>& tokens, varuint32* index)
{
  EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
  EXPECTED(tokens, TOKEN_FUNC, ERR_WAT_EXPECTED_FUNC);

  FunctionSig sig;
  int r = WatTypeInner(tokens, sig, 0);
  if(r = AppendArray<FunctionSig>(sig, state.m.type.functions, state.m.type.n_functions))
    return r;

  EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
  return ERR_SUCCESS;
}

int WatAppendImport(Module& m, const Import& i, varuint32* index)
{
  if(m.table.n_tables > 0 || m.function.n_funcdecl > 0 || m.global.n_globals > 0 || m.memory.n_memory > 0)
    return ERR_WAT_INVALID_IMPORT_ORDER; // If we're trying to insert an import after declaring a table/func/global/memory, fail.

  if(!(m.importsection.imports = (Import*)realloc(m.importsection.imports, ++m.importsection.n_import)))
    return ERR_FATAL_OUT_OF_MEMORY;

  // Find the correct index to insert into
  for(varuint32 j = 0; j < m.importsection.n_import - 1; ++j)
    if(m.importsection.imports[j].kind > i.kind)
      *index = j;

  if((m.importsection.n_import - *index - 1) > 0) // Move things out of the way if we aren't at the end of the array
    memmove(m.importsection.imports + *index + 1, m.importsection.imports + *index, m.importsection.n_import - *index - 1);

  m.importsection.imports[*index] = i; // Set the value

  // Properly increment counts based on kind
  switch(i.kind)
  {
  case KIND_FUNCTION:
    ++m.importsection.functions;
  case KIND_TABLE:
    ++m.importsection.tables;
  case KIND_MEMORY:
    ++m.importsection.memory;
  case KIND_GLOBAL: // Skip incrementing the globals count, because we already did it when incrementing n_import
    break;
  }

  return ERR_SUCCESS;
}

int WatFuncType(Stack<Token>& tokens, varuint32& sig, const char*** names, WatState& state)
{
  sig = (varuint32)~0;
  if(tokens.Peek().id == TOKEN_OPEN)
  {
    EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
    EXPECTED(tokens, TOKEN_TYPE, ERR_WAT_EXPECTED_TYPE);

    if(tokens.Peek().id == TOKEN_INTEGER && tokens.Peek().i < std::numeric_limits<varuint32>::max())
      sig = (varuint32)tokens.Pop().i;
    else if(tokens.Peek().id == TOKEN_NAME)
    {
      khiter_t iter = kh_get_indexname(state.typehash, tokens.Pop());
      if(!kh_exist2(state.typehash, iter))
        return ERR_WAT_INVALID_NAME;
      sig = kh_val(state.typehash, iter);
    }
    else
      return ERR_WAT_EXPECTED_VAR;

    if(sig > state.m.type.n_functions)
      return ERR_WAT_INVALID_TYPE;

    EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }

  if(tokens.Peek().id == TOKEN_PARAM || tokens.Peek().id == TOKEN_RESULT)
  {
    // Create a type to match this function signature
    FunctionSig func;
    int r = WatTypeInner(tokens, func, names);

    if(sig != (varuint32)~0) // If we already have a type, compare the two types and make sure they are identical
    {
      if(!MatchFunctionSig(state.m.type.functions[sig], func))
        return ERR_WAT_TYPE_MISMATCH;
    }
    else
    {
      sig = state.m.type.n_functions;
      return !r ? AppendArray<FunctionSig>(func, state.m.type.functions, state.m.type.n_functions) : r;
    }
  }

  return ERR_SUCCESS;
}

// Checks if an integer is a power of two
inline bool IsPowerOfTwo(varuint32 x) noexcept
{
  return (x & (x - 1)) == 0;
}

// Given an exact power of two, quickly gets the log2 value
inline uint32_t Power2Log2(uint32_t v) noexcept
{
  assert(IsPowerOfTwo(v));
#ifdef NW_COMPILER_MSC
  unsigned long r;
  _BitScanReverse(&r, v);
#elif defined(NW_COMPILER_GCC)
  uint32_t r = (sizeof(uint32_t) << 3) - 1 - __builtin_clz(v);
#else
  const uint32_t b[] = { 0xAAAAAAAA, 0xCCCCCCCC, 0xF0F0F0F0, 0xFF00FF00, 0xFFFF0000 };
  uint32_t r = (v & b[0]) != 0;
  r |= ((v & b[4]) != 0) << 4;
  r |= ((v & b[3]) != 0) << 3;
  r |= ((v & b[2]) != 0) << 2;
  r |= ((v & b[1]) != 0) << 1;
#endif
  return r;
}

varuint32 WatGetLocal(FunctionBody& f, FunctionSig& sig, const Token& t)
{
  if(t.id == TOKEN_INTEGER && t.i < std::numeric_limits<varuint32>::max())
    return (varuint32)t.i;
  else if(t.id == TOKEN_NAME)
  {
    std::string n(t.pos, t.len);

    for(varuint32 i = 0; i < sig.n_params; ++i)
      if(!strcmp(f.param_names[i], n.c_str()))
        return i;

    for(varuint32 i = 0; i < f.n_locals; ++i)
      if(!strcmp(f.local_names[i], n.c_str()))
        return i + sig.n_params;
  }

  return (varuint32)~0;
}

varuint32 WatGetFromHash(kh_indexname_t* hash, const Token& t)
{
  if(t.id == TOKEN_INTEGER && t.i < std::numeric_limits<varuint32>::max())
    return (varuint32)t.i;
  else if(t.id == TOKEN_NAME)
  {
    khiter_t iter = kh_get_indexname(hash, t);

    if(kh_exist2(hash, iter))
      return kh_val(hash, iter);
  }

  return (varuint32)~0;
}

int WatConstantOperator(Stack<Token>& tokens, Instruction& op, WatState& state)
{
  switch(op.opcode)
  {
  case OP_i32_const:
    if(tokens.Peek().id != TOKEN_INTEGER)
      return ERR_WAT_EXPECTED_INTEGER;
    op.immediates[0]._varsint32 = (varsint32)tokens.Pop().i;
    break;
  case OP_i64_const:
    if(tokens.Peek().id != TOKEN_INTEGER)
      return ERR_WAT_EXPECTED_INTEGER;
    op.immediates[0]._varsint64 = tokens.Pop().i;
    break;
  case OP_f32_const:
    if(tokens.Peek().id == TOKEN_INTEGER)
      op.immediates[0]._float32 = (float32)tokens.Pop().i;
    else if(tokens.Peek().id == TOKEN_FLOAT)
      op.immediates[0]._float32 = (float32)tokens.Pop().f;
    else
      return ERR_WAT_EXPECTED_FLOAT;
    break;
  case OP_f64_const:
    if(tokens.Peek().id == TOKEN_INTEGER)
      op.immediates[0]._float64 = (float64)tokens.Pop().i;
    else if(tokens.Peek().id == TOKEN_FLOAT)
      op.immediates[0]._float64 = tokens.Pop().f;
    else
      return ERR_WAT_EXPECTED_FLOAT;
    break;
  case OP_get_global: // For constant initializers, this has to be an import, and thus must always already exist by the time we reach it.
    op.immediates[0]._varuint32 = WatGetFromHash(state.globalhash, tokens.Pop());
    if(op.immediates[0]._varuint32 == (varuint32)~0)
      return ERR_WAT_INVALID_VAR;
    break;
  }

  return ERR_SUCCESS;
}
int WatOperator(Stack<Token>& tokens, WatState& state, FunctionBody& f, FunctionSig& sig, varuint32 index)
{
  if(tokens.Peek().id != TOKEN_OPERATOR)
    return ERR_WAT_EXPECTED_OPERATOR;

  int r;
  Instruction op;
  op.opcode = GetInstruction(std::string(tokens.Peek().pos, tokens.Peek().len).c_str());
  tokens.Pop();

  switch(op.opcode)
  {
  case 0xFF:
    return ERR_FATAL_UNKNOWN_INSTRUCTION;
  case OP_br:
  case OP_br_if:
    op.immediates[0]._varuint7 = state.GetJump(tokens.Pop());
    if(op.immediates[0]._varuint7 == (varuint7)~0)
      return ERR_WAT_EXPECTED_VAR;
    break;
  case OP_get_local:
  case OP_set_local:
  case OP_tee_local:
    op.immediates[0]._varuint32 = WatGetLocal(f, sig, tokens.Pop());
    if(op.immediates[0]._varuint32 >= f.n_locals + sig.n_params)
      return ERR_WAT_INVALID_LOCAL;
    break;
  case OP_get_global:
  case OP_set_global:
  case OP_call:
    state.defer.Push(DeferAction{ op.opcode, tokens.Pop(), index, f.n_body });
    break;
  case OP_i32_const:
  case OP_i64_const:
  case OP_f32_const:
  case OP_f64_const:
    if(r = WatConstantOperator(tokens, op, state))
      return r;
    break;
  case OP_br_table:
    do
    {
      varuint7 jump = state.GetJump(tokens.Pop());
      if(jump == (varuint7)~0)
        return ERR_WAT_EXPECTED_VAR;

      if(r = AppendArray<varuint32>(jump, op.immediates[0].table, op.immediates[0].n_table))
        return r;
    } while(tokens.Peek().id == TOKEN_NAME || tokens.Peek().id == TOKEN_INTEGER);

    op.immediates[1]._varuint32 = op.immediates[0].table[--op.immediates[0].n_table]; // Remove last jump from table and make it the default
    break;
  case OP_call_indirect:
    WatFuncType(tokens, op.immediates[0]._varuint32, 0, state);
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
    if(tokens.Peek().id == TOKEN_OFFSET)
    {
      tokens.Pop();
      if(tokens.Peek().id != TOKEN_INTEGER)
        return ERR_WAT_EXPECTED_INTEGER;
      op.immediates[1]._varuptr = tokens.Pop().i;
    }
    if(tokens.Peek().id == TOKEN_ALIGN)
    {
      tokens.Pop();
      if(tokens.Peek().id != TOKEN_INTEGER)
        return ERR_WAT_EXPECTED_INTEGER;
      if(tokens.Peek().i >= std::numeric_limits<memflags>::max())
        return ERR_WAT_OUT_OF_RANGE;

      op.immediates[0]._memflags = (memflags)tokens.Pop().i;
      if(op.immediates[0]._memflags == 0 || !IsPowerOfTwo(op.immediates[0]._memflags)) // Ensure this alignment is exactly a power of two
        return ERR_WAT_INVALID_ALIGNMENT;
      op.immediates[0]._memflags = Power2Log2(op.immediates[0]._memflags); // Calculate proper power of two
    }

    break;
  }

  if(r = AppendArray<Instruction>(op, f.body, f.n_body))
    return r;
  return ERR_SUCCESS;
}

void WatLabel(Stack<Token>& tokens, WatState& state)
{
  if(tokens.Peek().id == TOKEN_NAME)
  {
    state.stack.Push(StringRef{ tokens.Peek().pos, tokens.Peek().len });
    tokens.Pop();
  }
  else
    state.stack.Push(StringRef{ 0, 0 });
}

bool CheckLabel(Stack<Token>& tokens, WatState& state)
{
  if(tokens.Peek().id == TOKEN_NAME)
  {
    Token t = tokens.Pop();
    return state.stack.Peek() == StringRef{ t.pos, t.len };
  }

  return true;
}

int WatBlockType(Stack<Token>& tokens, varsint7& out)
{
  out = TE_void;
  if(tokens.Size() > 1 && tokens[0].id == TOKEN_OPEN && tokens[1].id == TOKEN_RESULT)
  {
    EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
    EXPECTED(tokens, TOKEN_RESULT, ERR_WAT_EXPECTED_RESULT);

    if(tokens.Peek().id != TOKEN_CLOSE)
    {
      if(!(out = WatValType(tokens.Pop().id)))
        return ERR_WAT_EXPECTED_VALTYPE;

      if(tokens.Peek().id != TOKEN_CLOSE)
        return ERR_MULTIPLE_RETURN_VALUES;
    }

    EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }

  if(tokens.Size() > 1 && tokens[0].id == TOKEN_OPEN && tokens[1].id == TOKEN_RESULT)
    return ERR_MULTIPLE_RETURN_VALUES;
  return ERR_SUCCESS;
}

int WatInstruction(Stack<Token>& tokens, WatState& state, FunctionBody& f, FunctionSig& sig, varuint32 index);

int WatExpression(Stack<Token>& tokens, WatState& state, FunctionBody& f, FunctionSig& sig, varuint32 index)
{
  if(tokens.Peek().id != TOKEN_OPEN)
    return ERR_WAT_EXPECTED_OPEN;

  int r;
  while(tokens.Peek().id == TOKEN_OPEN)
  {
    EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);

    Token t = tokens.Pop();
    varsint7 blocktype;
    switch(t.id)
    {
    case TOKEN_BLOCK:
    case TOKEN_LOOP:
      WatLabel(tokens, state);
      if(r = WatBlockType(tokens, blocktype))
        return r;

      {
        Instruction op = { t.id == TOKEN_BLOCK ? (byte)OP_block : (byte)OP_loop };
        op.immediates[0]._varsint7 = blocktype;
        if(r = AppendArray<Instruction>(op, f.body, f.n_body))
          return r;
      }

      while(tokens.Peek().id != TOKEN_CLOSE)
        if(r = WatInstruction(tokens, state, f, sig, index))
          return r;

      if(r = AppendArray<Instruction>(Instruction{ OP_end }, f.body, f.n_body))
        return r;
      state.stack.Pop();
      break;
    case TOKEN_IF:
      WatLabel(tokens, state);
      if(r = WatBlockType(tokens, blocktype))
        return r;

      while(tokens.Size() > 1 && tokens[0].id == TOKEN_OPEN && tokens[1].id != TOKEN_THEN)
        WatExpression(tokens, state, f, sig, index);

      {
        Instruction op = { OP_if };
        op.immediates[0]._varsint7 = blocktype;
        if(r = AppendArray<Instruction>(op, f.body, f.n_body)) // We append the if instruction _after_ the optional condition expression
          return r;
      }

      EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN); // There must always be a Then branch
      EXPECTED(tokens, TOKEN_THEN, ERR_WAT_EXPECTED_THEN);

      while(tokens.Peek().id != TOKEN_CLOSE)
        if(r = WatInstruction(tokens, state, f, sig, index))
          return r;

      EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);

      if(tokens.Peek().id == TOKEN_OPEN) // Must be an else branch if it exists
      {
        EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
        EXPECTED(tokens, TOKEN_ELSE, ERR_WAT_EXPECTED_ELSE);

        if(r = AppendArray<Instruction>(Instruction{ OP_else }, f.body, f.n_body))
          return r;

        while(tokens.Peek().id != TOKEN_CLOSE)
          if(r = WatInstruction(tokens, state, f, sig, index))
            return r;

        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      }

      if(r = AppendArray<Instruction>(Instruction{ OP_end }, f.body, f.n_body))
        return r;
      state.stack.Pop();
      break;
    default:
      WatOperator(tokens, state, f, sig, index);
      break;
    }
  }

  EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
  return ERR_SUCCESS;
}

int WatInstruction(Stack<Token>& tokens, WatState& state, FunctionBody& f, FunctionSig& sig, varuint32 index)
{
  int r;
  varsint7 blocktype;
  Token t = tokens.Pop();
  switch(t.id)
  {
  case TOKEN_OPEN: // This must be an expression
    return WatExpression(tokens, state, f, sig, index);
  case TOKEN_BLOCK:
  case TOKEN_LOOP:
    WatLabel(tokens, state);
    if(r = WatBlockType(tokens, blocktype))
      return r;

    {
      Instruction op = { t.id == TOKEN_BLOCK ? (byte)OP_block : (byte)OP_loop };
      op.immediates[0]._varsint7 = blocktype;
      if(r = AppendArray<Instruction>(op, f.body, f.n_body))
        return r;
    }

    while(tokens.Peek().id != TOKEN_END)
      if(r = WatInstruction(tokens, state, f, sig, index))
        return r;

    EXPECTED(tokens, TOKEN_END, ERR_WAT_EXPECTED_END);

    if(r = CheckLabel(tokens, state))
      return r;

    if(r = AppendArray<Instruction>(Instruction{ OP_end }, f.body, f.n_body))
      return r;
    state.stack.Pop();
    break;
  case TOKEN_IF:
    WatLabel(tokens, state);
    if(r = WatBlockType(tokens, blocktype))
      return r;

    {
      Instruction op = { OP_if };
      op.immediates[0]._varsint7 = blocktype;
      if(r = AppendArray<Instruction>(op, f.body, f.n_body)) // We append the if instruction _after_ the optional condition expression
        return r;
    }

    while(tokens.Peek().id != TOKEN_ELSE && tokens.Peek().id != TOKEN_END)
      if(r = WatInstruction(tokens, state, f, sig, index))
        return r;

    if(tokens.Pop().id == TOKEN_ELSE) // Handle else branch
    {
      if(r = CheckLabel(tokens, state))
        return r;

      if(r = AppendArray<Instruction>(Instruction{ OP_else }, f.body, f.n_body))
        return r;

      while(tokens.Peek().id != TOKEN_END)
        if(r = WatInstruction(tokens, state, f, sig, index))
          return r;

      EXPECTED(tokens, TOKEN_END, ERR_WAT_EXPECTED_END);
    }

    if(r = CheckLabel(tokens, state))
      return r;

    if(r = AppendArray<Instruction>(Instruction{ OP_end }, f.body, f.n_body))
      return r;
    state.stack.Pop();
    break;
  default:
    return WatOperator(tokens, state, f, sig, index);
  }

  return ERR_SUCCESS;
}

int WatInlineImportExport(Module& m, Stack<Token>& tokens, varuint32* index, varuint7 kind, Import** out)
{
  EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
  int r;

  switch(tokens.Pop().id)
  {
  case TOKEN_EXPORT:
  {
    Export e;
    e.kind = kind;
    e.index = *index; // This is fine because you can only import OR export on a declaration statement
    if(r = WatString(e.name, tokens.Pop()))
      return r;
    AppendArray<Export>(e, m.exportsection.exports, m.exportsection.n_exports);
  }
  case TOKEN_IMPORT:
  {
    Import i;
    if(r = WatString(i.module_name, tokens.Pop()))
      return r;
    if(r = WatString(i.export_name, tokens.Pop()))
      return r;
    i.kind = kind;
    if(r = WatAppendImport(m, i, index))
      return r;

    *out = m.importsection.imports + *index;
    switch(i.kind) // Fix the index
    {
    case KIND_TABLE: *index -= m.importsection.functions; break;
    case KIND_MEMORY: *index -= m.importsection.tables; break;
    case KIND_GLOBAL: *index -= m.importsection.memory; break;
    }
  }
  default:
    return ERR_WAT_EXPECTED_TOKEN;
  }

  EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
  return ERR_SUCCESS;
}

int WatFunction(WatState& state, Stack<Token>& tokens, varuint32* index, StringRef name)
{
  int r;
  *index = state.m.function.n_funcdecl;
  Import* i = 0;
  if(r = WatInlineImportExport(state.m, tokens, index, KIND_FUNCTION, &i))
    return r;

  if(i) // If this is an import, assemble the aux information and abort.
    return WatFuncType(tokens, i->func_desc.sig_index, &i->func_desc.param_names, state);

  varuint32 sig;
  FunctionBody body;
  if(r = WatFuncType(tokens, sig, &body.param_names, state))
    return r;

  FunctionSig& desc = state.m.type.functions[sig];
  WatString(body.debug_name, name); // Record debug_name if it exists

  // Read in all the locals
  while(tokens.Size() > 1 && tokens.Peek().id == TOKEN_OPEN && tokens[tokens.Size() - 2].id == TOKEN_LOCAL)
  {
    EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
    EXPECTED(tokens, TOKEN_LOCAL, ERR_WAT_EXPECTED_LOCAL);

    if(tokens.Peek().id == TOKEN_NAME)
    {
      if(tokens.Peek().len > std::numeric_limits<varuint32>::max())
        return ERR_WAT_OUT_OF_RANGE;
      varuint32 len = (varuint32)tokens.Peek().len;
      char* s = tmalloc<char>(len + 1);
      memcpy(s, tokens.Peek().pos, len);
      s[len] = 0;

      len = body.n_locals;
      if(r = AppendArray<const char*>(s, body.local_names, len))
        return r;
      tokens.Pop();
    }

    varuint7 local = WatValType(tokens.Pop().id);
    if(!local)
      return ERR_WAT_EXPECTED_VALTYPE;
    if(r = AppendArray<varuint7>(local, body.locals, body.n_locals))
      return r;

    EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }

  // Read in all instructions
  assert(state.stack.Size() == 0);
  while(tokens.Peek().id != TOKEN_END)
  {
    if(r = WatInstruction(tokens, state, body, desc, *index))
      return r;
  }
  assert(state.stack.Size() == 0);

  r = AppendArray(sig, state.m.function.funcdecl, state.m.function.n_funcdecl);

  if(r >= 0)
    r = AppendArray(body, state.m.code.funcbody, state.m.code.n_funcbody);
  
  return r;
}

int WatResizableLimits(ResizableLimits& limits, Stack<Token>& tokens)
{
  if(tokens.Peek().id != TOKEN_INTEGER || tokens.Peek().i >= std::numeric_limits<varuint32>::max())
    return ERR_WAT_EXPECTED_INTEGER;
  limits.minimum = (varuint32)tokens.Pop().i;

  if(tokens.Peek().id == TOKEN_INTEGER)
  {
    if(tokens.Peek().i >= std::numeric_limits<varuint32>::max())
      return ERR_WAT_OUT_OF_RANGE;
    limits.maximum = (varuint32)tokens.Pop().i;
  }

  return ERR_SUCCESS;
}

int WatTableDesc(TableDesc& t, Stack<Token>& tokens)
{
  int r;
  if(r = WatResizableLimits(t.resizable, tokens))
    return r;

  EXPECTED(tokens, TOKEN_ANYFUNC, ERR_WAT_EXPECTED_ANYFUNC);

  t.element_type = TE_anyfunc;
  return ERR_SUCCESS;
}

int WatTable(WatState& state, Stack<Token>& tokens, varuint32* index)
{
  int r;
  *index = state.m.table.n_tables;
  Import* i = 0;
  if(r = WatInlineImportExport(state.m, tokens, index, KIND_TABLE, &i))
    return r;

  if(i) // If this is an import, assemble the aux information and abort.
    return WatTableDesc(i->table_desc, tokens);

  TableDesc table;
  switch(tokens.Peek().id)
  {
  case TOKEN_INTEGER:
    WatTableDesc(table, tokens);
    break;
  default:
    EXPECTED(tokens, TOKEN_ANYFUNC, ERR_WAT_EXPECTED_ANYFUNC);

    table.element_type = TE_anyfunc;

    EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
    EXPECTED(tokens, TOKEN_ELEM, ERR_WAT_EXPECTED_ELEM);

    {
      TableInit init;
      init.index = *index;
      init.offset = Instruction{ OP_i32_const, 0 };

      while(tokens.Peek().id != TOKEN_CLOSE)
      {
        varuint32 f = WatGetFromHash(state.funchash, tokens.Pop());
        if(f == (varuint32)~0)
          return ERR_WAT_INVALID_VAR;
        AppendArray(f, init.elems, init.n_elems);
      }

      table.resizable.minimum = init.n_elems;
      table.resizable.flags = 0;
    }

    EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }

  return AppendArray(table, state.m.table.tables, state.m.table.n_tables);
}

int WatInitializer(WatState& state, Stack<Token>& tokens, Instruction& op)
{
  if(tokens.Peek().id != TOKEN_OPERATOR)
    return ERR_WAT_EXPECTED_OPERATOR;

  int r;
  op.opcode = GetInstruction(std::string(tokens.Peek().pos, tokens.Peek().len).c_str());
  tokens.Pop();

  if(r = WatConstantOperator(tokens, op, state))
    return r;

  if(tokens.Peek().id != TOKEN_CLOSE)
    return ERR_WAT_INVALID_INITIALIZER;

  return ERR_SUCCESS;
}

int WatGlobalDesc(GlobalDesc& g, Stack<Token>& tokens)
{
  if(tokens.Peek().id == TOKEN_OPEN)
  {
    EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
    EXPECTED(tokens, TOKEN_MUT, ERR_WAT_EXPECTED_MUT);
    g.mutability = true;
    if(!(g.type = WatValType(tokens.Pop().id)))
      return ERR_WAT_EXPECTED_VALTYPE;

    EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_OPEN);
  }
  else
  {
    g.mutability = false;
    if(!(g.type = WatValType(tokens.Pop().id)))
      return ERR_WAT_EXPECTED_VALTYPE;
  }

  return ERR_SUCCESS;
}

int WatGlobal(WatState& state, Stack<Token>& tokens, varuint32* index)
{
  int r;
  *index = state.m.global.n_globals;
  Import* i = 0;
  if(r = WatInlineImportExport(state.m, tokens, index, KIND_GLOBAL, &i))
    return r;

  if(i) // If this is an import, assemble the aux information and abort.
    return WatGlobalDesc(i->global_desc, tokens);

  GlobalDecl g;
  if(r = WatGlobalDesc(g.desc, tokens))
    return r;

  if(r = WatInitializer(state, tokens, g.init))
    return r;

  return AppendArray(g, state.m.global.globals, state.m.global.n_globals);
}

int WatMemoryDesc(MemoryDesc& m, Stack<Token>& tokens)
{
  return WatResizableLimits(m.limits, tokens);
}

int WatMemory(WatState& state, Stack<Token>& tokens, varuint32* index)
{
  int r;
  *index = state.m.memory.n_memory;
  Import* i = 0;
  if(r = WatInlineImportExport(state.m, tokens, index, KIND_MEMORY, &i))
    return r;

  if(i) // If this is an import, assemble the aux information and abort.
    return WatMemoryDesc(i->mem_desc, tokens);

  MemoryDesc mem;
  
  if(tokens.Size() > 1 && tokens[0].id == TOKEN_OPEN && tokens[1].id == TOKEN_DATA)
  {
    DataInit init = { 0 };
    init.index = *index;
    init.offset = Instruction{ OP_i32_const, 0 };

    while(tokens[0].id != TOKEN_CLOSE)
    {
      if(tokens[0].id != TOKEN_STRING)
        return ERR_WAT_EXPECTED_STRING;
      WatString(init.data, tokens[0]);
    }

    mem.limits.flags = 0;
    mem.limits.minimum = init.data.n_bytes;
  }
  else if(r = WatMemoryDesc(mem, tokens))
    return r;

  return AppendArray(mem, state.m.memory.memory, state.m.memory.n_memory);
}

template<int(*F)(WatState&, Stack<Token>&, varuint32*)>
int WatIndexProcess(WatState& state, Stack<Token>& tokens, kh_indexname_t* hash)
{
  int r;
  khiter_t iter = AddWatName(hash, tokens, &r);
  if(!r)
    return ERR_WAT_DUPLICATE_NAME;

  varuint32 index;
  if(r = (*F)(state, tokens, &index))
    return r;

  if(iter != kh_end(hash))
    kh_val(hash, iter) = index;
  return ERR_SUCCESS;
}

int WatModule(Environment& env, Module& m, Stack<Token>& tokens)
{
  EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
  EXPECTED(tokens, TOKEN_MODULE, ERR_WAT_EXPECTED_MODULE);

  memset(&m, 0, sizeof(Module));

  if(tokens.Peek().id == TOKEN_NAME)
    WatName(m.name, tokens.Pop());

  WatState state(m);

  Token t;
  int r;
  size_t restore = tokens.Size();
  while(tokens.Size() > 0 && tokens.Peek().id != TOKEN_CLOSE)
  { 
    EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
    t = tokens.Pop();
    switch(t.id) // This initial pass is for types only
    {
    case TOKEN_TYPE:
      if(r = WatIndexProcess<WatType>(state, tokens, state.typehash))
        return r;
      break;
    }
  }

  // This is the main pass for functions/imports/exports/etc. and also identifies illegal tokens
  tokens.SetSize(restore);
  while(tokens.Size() > 0 && tokens.Peek().id != TOKEN_CLOSE)
  {
    EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
    t = tokens.Pop();
    switch(t.id)
    {
    case TOKEN_FUNC:
    {
      khiter_t iter = AddWatName(state.funchash, tokens, &r);
      if(!r)
        return ERR_WAT_DUPLICATE_NAME;

      StringRef ref = { 0,0 };
      if(iter != kh_end(state.funchash))
        ref = { kh_key(state.funchash, iter).pos, kh_key(state.funchash, iter).len };
      varuint32 index;
      if(r = WatFunction(state, tokens, &index, ref))
        return r;

      if(iter != kh_end(state.funchash))
        kh_val(state.funchash, iter) = index;
      break;
    }
    case TOKEN_IMPORT:
      break;
    case TOKEN_EXPORT:
      break;
    case TOKEN_TABLE:
      if(r = WatIndexProcess<WatTable>(state, tokens, state.tablehash))
        return r;
      break;
    case TOKEN_MEMORY:
      if(r = WatIndexProcess<WatMemory>(state, tokens, state.memoryhash))
        return r;
      break;
    case TOKEN_GLOBAL:
      if(r = WatIndexProcess<WatGlobal>(state, tokens, state.globalhash))
        return r;
      break;
    case TOKEN_TYPE:
    case TOKEN_ELEM:
    case TOKEN_DATA:
    case TOKEN_START:
      break;
    default:
      return ERR_WAT_INVALID_TOKEN;
    }
    EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }

  // This pass resolves elem, data, and the start function, to minimize deferred actions
  tokens.SetSize(restore);
  while(tokens.Size() > 0 && tokens.Peek().id != TOKEN_CLOSE)
  {
    EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
    t = tokens.Pop();
    switch(t.id)
    {
    case TOKEN_ELEM:
      break;
    case TOKEN_DATA:
      break;
    case TOKEN_START:
      break;
    }
    EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }

  EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);

  // Process all deferred actions
  while(state.defer.Size() > 0)
  {

  }

  ParseExportFixup(m);
  return ERR_SUCCESS;
}
int WatEnvironment(Environment& env, Stack<Token>& tokens)
{
  return 0;
}

// This parses an entire extended WAT testing script into an environment
int ParseWAT(Environment& env, std::istream& s)
{
  s.seekg(0, std::ios::end);
  size_t len = s.tellg();
  s.seekg(0, std::ios::beg);
  auto buf = std::unique_ptr<char[]>(new char[len + 1]);
  s.read(buf.get(), len);
  buf.get()[len] = 0;

  Stack<Token> tokens;
  TokenizeWAT(tokens, buf.get());
  return WatEnvironment(env, tokens);
}