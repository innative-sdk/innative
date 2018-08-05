// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "wat.h"
#include "util.h"
#include "parse.h"
#include "validate.h"
#include <vector>
#include <regex>

#define LEXER_DIGIT "0-9"
#define LEXER_HEXDIGIT "a-fA-F0-9"
#define LEXER_LETTER "A-Za-z"
#define LEXER_PLUSMINUS "[+-]?"
#define LEXER_NUM "[" LEXER_DIGIT "](_?[" LEXER_DIGIT "])*"
#define LEXER_HEXNUM "[" LEXER_HEXDIGIT "](_?[" LEXER_HEXDIGIT "])*"

namespace innative {
  namespace wat {

    WatState::WatState(Module& mod) : m(mod)
    {
      typehash = kh_init_indexname();
      funchash = kh_init_indexname();
      tablehash = kh_init_indexname();
      memoryhash = kh_init_indexname();
      globalhash = kh_init_indexname();
    }
    WatState::~WatState()
    {
      kh_destroy_indexname(typehash);
      kh_destroy_indexname(funchash);
      kh_destroy_indexname(tablehash);
      kh_destroy_indexname(memoryhash);
      kh_destroy_indexname(globalhash);
    }
    varuint7 WatState::GetJump(Token var)
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

    KHASH_INIT(tokens, StringRef, TokenID, 1, __ac_X31_hash_stringrefins, kh_int_hash_equal)

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

    __KHASH_IMPL(indexname, kh_inline, Token, varuint32, 1, kh_hash_token, kh_equal_token)

      kh_tokens_t* GenTokenHash(std::initializer_list<const char*> list)
    {
      kh_tokens_t* h = kh_init_tokens();

      TokenID count = 0;
      int r;
      for(const char* s : list)
      {
        auto iter = kh_put_tokens(h, StringRef{ s, strlen(s) }, &r);
        kh_val(h, iter) = ++count;
      }

      return h;
    }

    static kh_tokens_t* tokenhash = GenTokenHash({ "(", ")", "module", "import", "type", "start", "func", "global", "table", "memory", "export",
      "data", "elem", "offset", "align", "local", "result", "param", "i32", "i64", "f32", "f64", "anyfunc", "mut", "block", "loop",
      "if", "then", "else", "end", /* script extensions */ "binary", "quote", "register", "invoke", "get", "assert_return",
      "assert_return_canonical_nan", "assert_return_arithmetic_nan", "assert_trap", "assert_malformed", "assert_invalid",
      "assert_unlinkable", "assert_exhaustion", "script", "input", "output" });

    static std::string numbuf;

    void TokenizeWAT(Queue<Token>& tokens, char* s, char* end)
    {
      static const std::regex::flag_type REGEX_CONFIG = std::regex_constants::ECMAScript | std::regex_constants::optimize;
      static const std::regex_constants::match_flag_type REGEX_MATCH = std::regex_constants::match_not_null | std::regex_constants::match_continuous;
      static std::regex regex_INT(LEXER_PLUSMINUS "(0x" LEXER_HEXNUM "|" LEXER_NUM ")", REGEX_CONFIG);
      static std::regex regex_NAME("[$][-" LEXER_LETTER LEXER_DIGIT "_.+*/\\~=<>!?@#$%&|:'`^]+", REGEX_CONFIG);
      static std::regex regex_FLOAT(LEXER_PLUSMINUS LEXER_NUM "[.](" LEXER_NUM ")?([eE]" LEXER_PLUSMINUS LEXER_NUM ")?", REGEX_CONFIG);
      static std::regex regex_HEXFLOAT(LEXER_PLUSMINUS "0x" LEXER_HEXNUM "[.](" LEXER_HEXNUM ")?([pP]" LEXER_PLUSMINUS LEXER_HEXNUM ")?", REGEX_CONFIG);
      static std::regex regex_NANFLOAT(LEXER_PLUSMINUS "[nN][aA][nN](:0x" LEXER_HEXNUM ")?", REGEX_CONFIG);

      while(s < end)
      {
        while(s < end && (s[0] == ' ' || s[0] == '\n' || s[0] == '\r' || s[0] == '\t' || s[0] == '\f'))
          ++s;

        if(s >= end)
          break;

        switch(s[0])
        {
        case 0:
          assert(s < end);
          ++s;
          break;
        case '(':
          if(s + 1 < end && s[1] == ';') // This is a comment
          {
            s += 2;
            size_t depth = 1;
            while(depth > 0 && s < end)
            {
              switch(*s)
              {
              case '(':
                if(s + 1 < end && s[1] == ';')
                  depth += 1;
                ++s;
                break;
              case ';':
                if(s + 1 < end && s[1] == ')')
                  depth -= 1;
                ++s;
                break;
              }
              ++s;
            }
          }
          else
          {
            tokens.Push(Token{ TOKEN_OPEN, s });
            ++s;
          }
          break;
        case ')':
          tokens.Push(Token{ TOKEN_CLOSE, s });
          ++s;
          break;
        case ';': // A comment
        {
          if(s + 1 < end && s[1] == ';')
          {
            do
            {
              ++s;
            } while(s < end && s[0] != '\n');
          }
          else
          {
            tokens.Push(Token{ TOKEN_NONE });
            assert(false);
          }

          if(s < end)
            ++s;

          break;
        }
        case '"': // A string
        {
          const char* begin = ++s;
          while(s[0] != '"' && s + 1 < end)
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
          std::match_results<const char*> results;
          if(std::regex_search<char>(s, results, regex_NAME, REGEX_MATCH))
          {
            assert(results[0].first == s);
            Token t = { TOKEN_NAME, results[0].first + 1 };
            t.len = results[0].length() - 1;
            tokens.Push(t);
            s = const_cast<char*>(results[0].second);
            break;
          } // If the search fails, fall through and just start trying other regexes. It will eventually be classified as an invalid token.
        }
        case '-':
        case '+':
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
          std::match_results<const char*> results;
          if(std::regex_search<const char*>(s, end, results, regex_HEXFLOAT, REGEX_MATCH) || std::regex_search<const char*>(s, end, results, regex_FLOAT, REGEX_MATCH))
          {
            Token t = { TOKEN_FLOAT, s };
            errno = 0;
            t.f = strtod(results[0].str().data(), NULL); // This can handle both normal and hex float format
            if(errno == ERANGE)
              t.id = TOKEN_NONE;

            tokens.Push(t);
            s = const_cast<char*>(results[0].second);
            break;
          }
          if(std::regex_search<const char*>(s, end, results, regex_INT, REGEX_MATCH))
          {
            errno = 0;
            tokens.Push(Token{ TOKEN_INTEGER, s, (int64_t)strtoll(results[0].str().data(), NULL, 10) });
            if(errno == ERANGE)
              tokens.Back().id = TOKEN_NONE;

            s = const_cast<char*>(results[0].second);
            break;
          } // Fall through to attempt something else
        }
        default:
        {
          // Check if this is an NaN first
          std::match_results<const char*> results;
          if(std::regex_search(s, results, regex_NANFLOAT, REGEX_MATCH))
          {
            Token t = { TOKEN_FLOAT, s };
            std::string nan("NAN("); // Construct a valid NAN(0xFFFF) string to be interpreted by strtod

            switch(results[0].first[0]) // Prepend sign if it exists
            {
            case '+':
            case '-':
              nan.insert(0, 1, results[0].first[0]);
              break;
            }

            if(results.length() > 0) // If digits are specified, add them, otherwise generate a quiet NAN
              nan += results[1].str().substr(1);
            nan += ')';

            errno = 0;
            t.f = strtod(nan.data(), NULL);
            if(errno == ERANGE)
              t.id = TOKEN_NONE;
            tokens.Push(t);
            s = const_cast<char*>(results[0].second);
            break;
          }

          const char* begin = s;

          while(s < end && s[0] != ' ' && s[0] != '\n' && s[0] != '\r' && s[0] != '\t' && s[0] != '\f' && s[0] != '=' && s[0] != ')' && s[0] != '(' && s[0] != ';')
            ++s;

          StringRef ref = { begin, static_cast<size_t>(s - begin) };
          khiter_t iter = kh_get_tokens(tokenhash, ref);
          if(kh_exist2(tokenhash, iter))
            tokens.Push(Token{ kh_val(tokenhash, iter), begin });
          else
          {
            uint8_t op = GetInstruction(ref);
            if(op != 0xFF)
              tokens.Push(Token{ TOKEN_OPERATOR, begin, (int64_t)op });
            else
            {
              assert(false);
              tokens.Push(Token{ TOKEN_NONE, begin, (int64_t)ref.len });
            }
          }
          if(*s == '=')
            ++s;
        }
        }
      }
    }

    void WriteUTF32(uint32_t ch, ByteArray& str)
    {
      static const uint32_t UNI_REPLACEMENT_CHAR = 0x0000FFFD;
      static const uint32_t UNI_MAX_LEGAL_UTF32 = 0x0010FFFF;
      static const uint8_t firstByteMark[7] = { 0x00, 0x00, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC };
      static const uint32_t byteMask = 0xBF;
      static const uint32_t byteMark = 0x80;

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
        ch = UNI_REPLACEMENT_CHAR;
      }


      uint8_t* target = str.bytes + str.n_bytes + bytesToWrite;
      switch(bytesToWrite)
      { /* note: everything falls through. */
      case 4: *--target = (uint8_t)((ch | byteMark) & byteMask); ch >>= 6;
      case 3: *--target = (uint8_t)((ch | byteMark) & byteMask); ch >>= 6;
      case 2: *--target = (uint8_t)((ch | byteMark) & byteMask); ch >>= 6;
      case 1: *--target = (uint8_t)(ch | firstByteMark[bytesToWrite]);
      }
      str.n_bytes += bytesToWrite;
    }

    int WatString(ByteArray& str, StringRef t)
    {
      if(!t.s)
        return assert(false), ERR_PARSE_INVALID_NAME;

      if(str.bytes)
      {
        uint8_t* n = tmalloc<uint8_t>(str.n_bytes + t.len + 1);
        tmemcpy(n, str.n_bytes + t.len + 1, str.bytes, str.n_bytes);
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
          {
            char* end;
            long u;
            errno = 0;
            u = strtol(t.s + i + 1, &end, 16);
            if(errno == ERANGE)
              return ERR_WAT_OUT_OF_RANGE;

            WriteUTF32(u, str);
            i += end - (t.s + i + 1);
            break;
          }
          default:
            if((t.s[i] >= '0' && t.s[i] <= '9') || (t.s[i] >= 'A' && t.s[i] <= 'F'))
            {
              if((t.s[i + 1] >= '0' && t.s[i + 1] <= '9') || (t.s[i + 1] >= 'A' && t.s[i + 1] <= 'F'))
              {
                char buf[3] = { t.s[i], t.s[i + 1], 0 };
                str.bytes[str.n_bytes++] = (uint8_t)strtol(buf, 0, 16);
                ++i;
                break;
              }
            }
            return assert(false), ERR_WAT_BAD_ESCAPE;
          }
        }
        else
          str.bytes[str.n_bytes++] = t.s[i];
      }
      str.bytes[str.n_bytes] = 0;

      return ERR_SUCCESS;
    }

    int WatName(ByteArray& name, const Token& t)
    {
      if(t.id != TOKEN_NAME || !t.pos || !t.len)
        return assert(false), ERR_PARSE_INVALID_NAME;

      name.bytes = tmalloc<uint8_t>(t.len + 1);
      if(!name.bytes || t.len > std::numeric_limits<varuint32>::max())
        return assert(false), ERR_FATAL_OUT_OF_MEMORY;
      name.n_bytes = (varuint32)t.len;
      tmemcpy((char*)name.bytes, name.n_bytes, t.pos, t.len);
      name.bytes[name.n_bytes] = 0;

      return ERR_SUCCESS;
    }

    template<class T>
    int AppendArray(T item, T*& a, varuint32& n)
    {
      if(!(a = trealloc<T>(a, ++n)))
        return assert(false), ERR_FATAL_OUT_OF_MEMORY;
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
        return assert(false), ERR_WAT_EXPECTED_VALTYPE;
      return AppendArray<varsint7>(ty, a, n);
    }

    int WatTypeInner(Queue<Token>& tokens, FunctionSig& sig, const char*** names)
    {
      sig.form = TE_func;
      int r;
      while(tokens.Size() > 1 && tokens[0].id == TOKEN_OPEN && (tokens[1].id == TOKEN_PARAM || tokens[1].id == TOKEN_RESULT))
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
                return assert(false), ERR_WAT_OUT_OF_RANGE;
              varuint32 len = (varuint32)tokens.Peek().len;
              char* s = tmalloc<char>(len + 1);
              tmemcpy(s, len + 1, tokens.Peek().pos, len);
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
          return assert(false), ERR_WAT_EXPECTED_TOKEN;
        }

        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      }

      return ERR_SUCCESS;
    }

    int WatType(WatState& state, Queue<Token>& tokens, varuint32* index)
    {
      EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
      EXPECTED(tokens, TOKEN_FUNC, ERR_WAT_EXPECTED_FUNC);

      FunctionSig sig = { 0 };
      int r = WatTypeInner(tokens, sig, 0);
      *index = state.m.type.n_functions;
      if(r = AppendArray<FunctionSig>(sig, state.m.type.functions, state.m.type.n_functions))
        return r;

      EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      return ERR_SUCCESS;
    }

    int WatAppendImport(Module& m, const Import& i, varuint32* index)
    {
      if(m.table.n_tables > 0 || m.function.n_funcdecl > 0 || m.global.n_globals > 0 || m.memory.n_memory > 0)
        return assert(false), ERR_WAT_INVALID_IMPORT_ORDER; // If we're trying to insert an import after declaring a table/func/global/memory, fail.

      *index = m.importsection.n_import;
      if(!(m.importsection.imports = trealloc<Import>(m.importsection.imports, ++m.importsection.n_import)))
        return assert(false), ERR_FATAL_OUT_OF_MEMORY;

      // Find the correct index to insert into
      for(varuint32 j = 0; j < m.importsection.n_import - 1; ++j)
        if(m.importsection.imports[j].kind > i.kind)
          *index = j;

      if((m.importsection.n_import - *index - 1) > 0) // Move things out of the way if we aren't at the end of the array
        memmove(m.importsection.imports + *index + 1, m.importsection.imports + *index, (m.importsection.n_import - *index - 1) * sizeof(Import));

      m.importsection.imports[*index] = i; // Set the value

      // Properly increment counts based on kind
      switch(i.kind)
      {
      case WASM_KIND_FUNCTION:
        ++m.importsection.functions;
      case WASM_KIND_TABLE:
        ++m.importsection.tables;
      case WASM_KIND_MEMORY:
        ++m.importsection.memory;
      case WASM_KIND_GLOBAL: // Skip incrementing the globals count, because we already did it when incrementing n_import
        break;
      }

      switch(i.kind) // Fix the index
      {
      case WASM_KIND_TABLE: *index -= m.importsection.functions; break;
      case WASM_KIND_MEMORY: *index -= m.importsection.tables; break;
      case WASM_KIND_GLOBAL: *index -= m.importsection.memory; break;
      }

      return ERR_SUCCESS;
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

    int WatFuncType(WatState& state, Queue<Token>& tokens, varuint32& sig, const char*** names)
    {
      sig = (varuint32)~0;
      if(tokens.Size() > 1 && tokens[0].id == TOKEN_OPEN && tokens[1].id == TOKEN_TYPE)
      {
        EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
        EXPECTED(tokens, TOKEN_TYPE, ERR_WAT_EXPECTED_TYPE);

        if(tokens.Peek().id != TOKEN_INTEGER && tokens.Peek().id != TOKEN_NAME)
          return assert(false), ERR_WAT_EXPECTED_VAR;

        sig = WatGetFromHash(state.typehash, tokens.Pop());

        if(sig > state.m.type.n_functions)
          return assert(false), ERR_WAT_INVALID_TYPE;

        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      }

      if(tokens.Size() > 1 && tokens[0].id == TOKEN_OPEN && (tokens[1].id == TOKEN_PARAM || tokens[1].id == TOKEN_RESULT))
      {
        // Create a type to match this function signature
        FunctionSig func = { 0 };
        int r = WatTypeInner(tokens, func, names);

        if(sig != (varuint32)~0) // If we already have a type, compare the two types and make sure they are identical
        {
          if(!MatchFunctionSig(state.m.type.functions[sig], func))
            return assert(false), ERR_WAT_TYPE_MISMATCH;
        }
        else
        {
          sig = state.m.type.n_functions;
          return !r ? AppendArray<FunctionSig>(func, state.m.type.functions, state.m.type.n_functions) : r;
        }
      }

      if(sig == (varuint32)~0) // If we still don't have a type, this is an empty type we need to add
      {
        sig = state.m.type.n_functions;
        return AppendArray<FunctionSig>(FunctionSig{ TE_func }, state.m.type.functions, state.m.type.n_functions);
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
#ifdef IR_COMPILER_MSC
      unsigned long r;
      _BitScanReverse(&r, v);
#elif defined(IR_COMPILER_GCC)
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

    int WatConstantOperator(WatState& state, Queue<Token>& tokens, Instruction& op)
    {
      switch(op.opcode)
      {
      case OP_i32_const:
        if(tokens.Peek().id != TOKEN_INTEGER)
          return assert(false), ERR_WAT_EXPECTED_INTEGER;
        op.immediates[0]._varsint32 = (varsint32)tokens.Pop().i;
        break;
      case OP_i64_const:
        if(tokens.Peek().id != TOKEN_INTEGER)
          return assert(false), ERR_WAT_EXPECTED_INTEGER;
        op.immediates[0]._varsint64 = tokens.Pop().i;
        break;
      case OP_f32_const:
        if(tokens.Peek().id == TOKEN_INTEGER)
          op.immediates[0]._float32 = (float32)tokens.Pop().i;
        else if(tokens.Peek().id == TOKEN_FLOAT)
          op.immediates[0]._float32 = (float32)tokens.Pop().f;
        else
          return assert(false), ERR_WAT_EXPECTED_FLOAT;
        break;
      case OP_f64_const:
        if(tokens.Peek().id == TOKEN_INTEGER)
          op.immediates[0]._float64 = (float64)tokens.Pop().i;
        else if(tokens.Peek().id == TOKEN_FLOAT)
          op.immediates[0]._float64 = tokens.Pop().f;
        else
          return assert(false), ERR_WAT_EXPECTED_FLOAT;
        break;
      case OP_get_global: // For constant initializers, this has to be an import, and thus must always already exist by the time we reach it.
        op.immediates[0]._varuint32 = WatGetFromHash(state.globalhash, tokens.Pop());
        if(op.immediates[0]._varuint32 == (varuint32)~0)
          return assert(false), ERR_WAT_INVALID_VAR;
        break;
      default:
        return assert(false), ERR_WAT_INVALID_INITIALIZER;
      }

      return ERR_SUCCESS;
    }
    int WatOperator(WatState& state, Queue<Token>& tokens, Instruction& op, FunctionBody& f, FunctionSig& sig, varuint32 index)
    {
      if(tokens.Peek().id != TOKEN_OPERATOR)
        return assert(false), ERR_WAT_EXPECTED_OPERATOR;

      int r;
      if(tokens.Peek().i > 0xFF)
        return ERR_WAT_OUT_OF_RANGE;
      op = { (uint8_t)tokens.Pop().i };

      switch(op.opcode)
      {
      case 0xFF:
        return assert(false), ERR_FATAL_UNKNOWN_INSTRUCTION;
      case OP_br:
      case OP_br_if:
        op.immediates[0]._varuint7 = state.GetJump(tokens.Pop());
        if(op.immediates[0]._varuint7 == (varuint7)~0)
          return assert(false), ERR_WAT_EXPECTED_VAR;
        break;
      case OP_get_local:
      case OP_set_local:
      case OP_tee_local:
        op.immediates[0]._varuint32 = WatGetLocal(f, sig, tokens.Pop());
        if(op.immediates[0]._varuint32 >= f.n_locals + sig.n_params)
          return assert(false), ERR_WAT_INVALID_LOCAL;
        break;
      case OP_get_global:
      case OP_set_global:
      case OP_call:
        state.defer.Push(DeferWatAction{ op.opcode, tokens.Pop(), index, f.n_body });
        break;
      case OP_i32_const:
      case OP_i64_const:
      case OP_f32_const:
      case OP_f64_const:
        if(r = WatConstantOperator(state, tokens, op))
          return r;
        break;
      case OP_br_table:
        do
        {
          varuint7 jump = state.GetJump(tokens.Pop());
          if(jump == (varuint7)~0)
            return assert(false), ERR_WAT_EXPECTED_VAR;

          if(r = AppendArray<varuint32>(jump, op.immediates[0].table, op.immediates[0].n_table))
            return r;
        } while(tokens.Peek().id == TOKEN_NAME || tokens.Peek().id == TOKEN_INTEGER);

        op.immediates[1]._varuint32 = op.immediates[0].table[--op.immediates[0].n_table]; // Remove last jump from table and make it the default
        break;
      case OP_call_indirect:
        if(r = WatFuncType(state, tokens, op.immediates[0]._varuint32, 0))
          return r;
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
            return assert(false), ERR_WAT_EXPECTED_INTEGER;
          op.immediates[1]._varuptr = tokens.Pop().i;
        }
        if(tokens.Peek().id == TOKEN_ALIGN)
        {
          tokens.Pop();
          if(tokens.Peek().id != TOKEN_INTEGER)
            return assert(false), ERR_WAT_EXPECTED_INTEGER;
          if(tokens.Peek().i >= std::numeric_limits<memflags>::max())
            return assert(false), ERR_WAT_OUT_OF_RANGE;

          op.immediates[0]._memflags = (memflags)tokens.Pop().i;
          if(op.immediates[0]._memflags == 0 || !IsPowerOfTwo(op.immediates[0]._memflags)) // Ensure this alignment is exactly a power of two
            return ERR_WAT_INVALID_ALIGNMENT;
          op.immediates[0]._memflags = Power2Log2(op.immediates[0]._memflags); // Calculate proper power of two
        }

        break;
      }

      return ERR_SUCCESS;
    }

    void WatLabel(WatState& state, Queue<Token>& tokens)
    {
      if(tokens.Peek().id == TOKEN_NAME)
      {
        state.stack.Push(StringRef{ tokens.Peek().pos, tokens.Peek().len });
        tokens.Pop();
      }
      else
        state.stack.Push(StringRef{ 0, 0 });
    }

    bool CheckLabel(WatState& state, Queue<Token>& tokens)
    {
      if(tokens.Peek().id == TOKEN_NAME)
      {
        Token t = tokens.Pop();
        return state.stack.Peek() == StringRef{ t.pos, t.len };
      }

      return true;
    }

    int WatBlockType(Queue<Token>& tokens, varsint7& out)
    {
      out = TE_void;
      if(tokens.Size() > 1 && tokens[0].id == TOKEN_OPEN && tokens[1].id == TOKEN_RESULT)
      {
        EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
        EXPECTED(tokens, TOKEN_RESULT, ERR_WAT_EXPECTED_RESULT);

        if(tokens.Peek().id != TOKEN_CLOSE)
        {
          if(!(out = WatValType(tokens.Pop().id)))
            return assert(false), ERR_WAT_EXPECTED_VALTYPE;

          if(tokens.Peek().id != TOKEN_CLOSE)
            return assert(false), ERR_MULTIPLE_RETURN_VALUES;
        }

        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      }

      if(tokens.Size() > 1 && tokens[0].id == TOKEN_OPEN && tokens[1].id == TOKEN_RESULT)
        return assert(false), ERR_MULTIPLE_RETURN_VALUES;
      return ERR_SUCCESS;
    }

    int WatInstruction(WatState& state, Queue<Token>& tokens, FunctionBody& f, FunctionSig& sig, varuint32 index);

    int WatExpression(WatState& state, Queue<Token>& tokens, FunctionBody& f, FunctionSig& sig, varuint32 index)
    {
      EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);

      int r;
      varsint7 blocktype;
      switch(tokens[0].id)
      {
      case TOKEN_BLOCK:
      case TOKEN_LOOP:
      {
        Token t = tokens.Pop();
        WatLabel(state, tokens);
        if(r = WatBlockType(tokens, blocktype))
          return r;

        {
          Instruction op = { t.id == TOKEN_BLOCK ? (uint8_t)OP_block : (uint8_t)OP_loop };
          op.immediates[0]._varsint7 = blocktype;
          if(r = AppendArray<Instruction>(op, f.body, f.n_body))
            return r;
        }

        while(tokens.Peek().id != TOKEN_CLOSE)
          if(r = WatInstruction(state, tokens, f, sig, index))
            return r;

        if(r = AppendArray<Instruction>(Instruction{ OP_end }, f.body, f.n_body))
          return r;
        state.stack.Pop();
        break;
      }
      case TOKEN_IF:
        tokens.Pop();
        WatLabel(state, tokens);
        if(r = WatBlockType(tokens, blocktype))
          return r;

        while(tokens.Size() > 1 && tokens[0].id == TOKEN_OPEN && tokens[1].id != TOKEN_THEN)
          if(r = WatExpression(state, tokens, f, sig, index))
            return r;

        {
          Instruction op = { OP_if };
          op.immediates[0]._varsint7 = blocktype;
          if(r = AppendArray<Instruction>(op, f.body, f.n_body)) // We append the if instruction _after_ the optional condition expression
            return r;
        }

        EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN); // There must always be a Then branch
        EXPECTED(tokens, TOKEN_THEN, ERR_WAT_EXPECTED_THEN);

        while(tokens.Peek().id != TOKEN_CLOSE)
          if(r = WatInstruction(state, tokens, f, sig, index))
            return r;

        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);

        if(tokens.Peek().id == TOKEN_OPEN) // Must be an else branch if it exists
        {
          EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
          EXPECTED(tokens, TOKEN_ELSE, ERR_WAT_EXPECTED_ELSE);

          if(r = AppendArray<Instruction>(Instruction{ OP_else }, f.body, f.n_body))
            return r;

          while(tokens.Peek().id != TOKEN_CLOSE)
            if(r = WatInstruction(state, tokens, f, sig, index))
              return r;

          EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
        }

        if(r = AppendArray<Instruction>(Instruction{ OP_end }, f.body, f.n_body))
          return r;
        state.stack.Pop();
        break;
      default:
      {
        Instruction op;
        if(r = WatOperator(state, tokens, op, f, sig, index))
          return r;

        // Expressions are folded instructions, so we must unfold them before inserting the operator
        while(tokens[0].id != TOKEN_CLOSE)
          WatExpression(state, tokens, f, sig, index);

        if(r = AppendArray<Instruction>(op, f.body, f.n_body)) // Now we append the operator
          return r;
        break;
      }
      }

      EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      return ERR_SUCCESS;
    }

    int WatInstruction(WatState& state, Queue<Token>& tokens, FunctionBody& f, FunctionSig& sig, varuint32 index)
    {
      int r;
      varsint7 blocktype;
      switch(tokens[0].id)
      {
      case TOKEN_OPEN: // This must be an expression
        return WatExpression(state, tokens, f, sig, index);
      case TOKEN_BLOCK:
      case TOKEN_LOOP:
      {
        Token t = tokens.Pop();
        WatLabel(state, tokens);
        if(r = WatBlockType(tokens, blocktype))
          return r;

        {
          Instruction op = { t.id == TOKEN_BLOCK ? (uint8_t)OP_block : (uint8_t)OP_loop };
          op.immediates[0]._varsint7 = blocktype;
          if(r = AppendArray<Instruction>(op, f.body, f.n_body))
            return r;
        }

        while(tokens.Peek().id != TOKEN_END)
          if(r = WatInstruction(state, tokens, f, sig, index))
            return r;

        EXPECTED(tokens, TOKEN_END, ERR_WAT_EXPECTED_END);

        if(!CheckLabel(state, tokens))
          return ERR_WAT_LABEL_MISMATCH;

        if(r = AppendArray<Instruction>(Instruction{ OP_end }, f.body, f.n_body))
          return r;
        state.stack.Pop();
        break;
      }
      case TOKEN_IF:
        tokens.Pop();
        WatLabel(state, tokens);
        if(r = WatBlockType(tokens, blocktype))
          return r;

        {
          Instruction op = { OP_if };
          op.immediates[0]._varsint7 = blocktype;
          if(r = AppendArray<Instruction>(op, f.body, f.n_body)) // We append the if instruction _after_ the optional condition expression
            return r;
        }

        while(tokens.Peek().id != TOKEN_ELSE && tokens.Peek().id != TOKEN_END)
          if(r = WatInstruction(state, tokens, f, sig, index))
            return r;

        if(tokens.Pop().id == TOKEN_ELSE) // Handle else branch
        {
          if(!CheckLabel(state, tokens))
            return ERR_WAT_LABEL_MISMATCH;

          if(r = AppendArray<Instruction>(Instruction{ OP_else }, f.body, f.n_body))
            return r;

          while(tokens.Peek().id != TOKEN_END)
            if(r = WatInstruction(state, tokens, f, sig, index))
              return r;

          EXPECTED(tokens, TOKEN_END, ERR_WAT_EXPECTED_END);
        }

        if(!CheckLabel(state, tokens))
          return ERR_WAT_LABEL_MISMATCH;

        if(r = AppendArray<Instruction>(Instruction{ OP_end }, f.body, f.n_body))
          return r;
        state.stack.Pop();
        break;
      default:
      {
        Instruction op;
        if(r = WatOperator(state, tokens, op, f, sig, index))
          return r;
        return AppendArray<Instruction>(op, f.body, f.n_body);
      }
      }

      return ERR_SUCCESS;
    }

    int WatInlineImportExport(Module& m, Queue<Token>& tokens, varuint32* index, varuint7 kind, Import** out)
    {
      int r;
      if(tokens.Size() > 1 && tokens[0].id == TOKEN_OPEN && tokens[1].id == TOKEN_EXPORT)
      {
        EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
        EXPECTED(tokens, TOKEN_EXPORT, ERR_WAT_EXPECTED_TOKEN);

        Export e = { 0 };
        e.kind = kind;
        e.index = *index; // This is fine because you can only import OR export on a declaration statement
        if(r = WatString(e.name, tokens.Pop()))
          return r;
        if(r = AppendArray<Export>(e, m.exportsection.exports, m.exportsection.n_exports))
          return r;
        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      }
      else if(tokens.Size() > 1 && tokens[0].id == TOKEN_OPEN && tokens[1].id == TOKEN_IMPORT)
      {
        EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
        EXPECTED(tokens, TOKEN_IMPORT, ERR_WAT_EXPECTED_TOKEN);

        Import i = { 0 };
        if(r = WatString(i.module_name, tokens.Pop()))
          return r;
        if(r = WatString(i.export_name, tokens.Pop()))
          return r;
        i.kind = kind;
        if(r = WatAppendImport(m, i, index))
          return r;

        switch(i.kind) // Fix the index
        {
        case WASM_KIND_FUNCTION: *out = m.importsection.imports + *index; break;
        case WASM_KIND_TABLE: *out = m.importsection.imports + m.importsection.functions + *index; break;
        case WASM_KIND_MEMORY: *out = m.importsection.imports + m.importsection.tables + *index; break;
        case WASM_KIND_GLOBAL: *out = m.importsection.imports + m.importsection.memory + *index; break;
        }

        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      }

      return ERR_SUCCESS;
    }

    int WatLocalAppend(FunctionBody& body, Queue<Token>& tokens)
    {
      varuint7 local = WatValType(tokens.Pop().id);
      if(!local)
        return assert(false), ERR_WAT_EXPECTED_VALTYPE;
      return AppendArray<varuint7>(local, body.locals, body.n_locals);
    }

    int WatFunction(WatState& state, Queue<Token>& tokens, varuint32* index, StringRef name)
    {
      int r;
      *index = state.m.importsection.functions + state.m.function.n_funcdecl;
      Import* i = 0;
      if(r = WatInlineImportExport(state.m, tokens, index, WASM_KIND_FUNCTION, &i))
        return r;

      if(i) // If this is an import, assemble the aux information and abort.
        return WatFuncType(state, tokens, i->func_desc.sig_index, &i->func_desc.param_names);

      varuint32 sig;
      FunctionBody body = { 0 };
      if(r = WatFuncType(state, tokens, sig, &body.param_names))
        return r;

      FunctionSig& desc = state.m.type.functions[sig];
      if(name.len > 0)
        if(r = WatString(body.debug_name, name))
          return r;

      // Read in all the locals
      while(tokens.Size() > 1 && tokens[0].id == TOKEN_OPEN && tokens[1].id == TOKEN_LOCAL)
      {
        EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
        EXPECTED(tokens, TOKEN_LOCAL, ERR_WAT_EXPECTED_LOCAL);

        if(tokens.Peek().id == TOKEN_NAME)
        {
          if(tokens.Peek().len > std::numeric_limits<varuint32>::max())
            return assert(false), ERR_WAT_OUT_OF_RANGE;
          varuint32 len = (varuint32)tokens.Peek().len;
          char* s = tmalloc<char>(len + 1);
          tmemcpy(s, len + 1, tokens.Peek().pos, len);
          s[len] = 0;

          len = body.n_locals;
          if(r = AppendArray<const char*>(s, body.local_names, len))
            return r;
          tokens.Pop();

          if(r = WatLocalAppend(body, tokens)) // Must have exactly one val_type to associate with the name
            return r;
        }
        else // Otherwise can have zero or more val_types
        {
          while(tokens[0].id != TOKEN_CLOSE)
          {
            if(r = WatLocalAppend(body, tokens))
              return r;
          }
        }

        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      }

      // Read in all instructions
      assert(state.stack.Size() == 0);
      while(tokens.Peek().id != TOKEN_CLOSE)
      {
        if(r = WatInstruction(state, tokens, body, desc, *index))
          return r;
      }
      assert(state.stack.Size() == 0);
      if(r = AppendArray(Instruction{ OP_end }, body.body, body.n_body))
        return r;

      if(r = AppendArray(sig, state.m.function.funcdecl, state.m.function.n_funcdecl))
        return r;

      return AppendArray(body, state.m.code.funcbody, state.m.code.n_funcbody);
    }

    int WatResizableLimits(ResizableLimits& limits, Queue<Token>& tokens)
    {
      if(tokens.Peek().id != TOKEN_INTEGER || tokens.Peek().i >= std::numeric_limits<varuint32>::max())
        return assert(false), ERR_WAT_EXPECTED_INTEGER;
      limits.minimum = (varuint32)tokens.Pop().i;

      if(tokens.Peek().id == TOKEN_INTEGER)
      {
        if(tokens.Peek().i >= std::numeric_limits<varuint32>::max())
          return assert(false), ERR_WAT_OUT_OF_RANGE;
        limits.maximum = (varuint32)tokens.Pop().i;
        limits.flags = 1;
      }

      return ERR_SUCCESS;
    }

    int WatTableDesc(TableDesc& t, Queue<Token>& tokens)
    {
      int r;
      if(r = WatResizableLimits(t.resizable, tokens))
        return r;

      EXPECTED(tokens, TOKEN_ANYFUNC, ERR_WAT_EXPECTED_ANYFUNC);

      t.element_type = TE_anyfunc;
      return ERR_SUCCESS;
    }

    int WatTable(WatState& state, Queue<Token>& tokens, varuint32* index)
    {
      int r;
      *index = state.m.table.n_tables;
      Import* i = 0;
      if(r = WatInlineImportExport(state.m, tokens, index, WASM_KIND_TABLE, &i))
        return r;

      if(i) // If this is an import, assemble the aux information and abort.
        return WatTableDesc(i->table_desc, tokens);

      TableDesc table = { 0 };
      switch(tokens.Peek().id)
      {
      case TOKEN_INTEGER:
        if(r = WatTableDesc(table, tokens))
          return r;
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
              return assert(false), ERR_WAT_INVALID_VAR;
            if(r = AppendArray(f, init.elems, init.n_elems))
              return r;
          }

          table.resizable.minimum = init.n_elems;
          table.resizable.flags = 0;
        }

        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      }

      return AppendArray(table, state.m.table.tables, state.m.table.n_tables);
    }

    int WatInitializer(WatState& state, Queue<Token>& tokens, Instruction& op)
    {
      if(tokens.Peek().id != TOKEN_OPERATOR)
        return assert(false), ERR_WAT_EXPECTED_OPERATOR;

      int r;
      if(tokens.Peek().i > 0xFF)
        return ERR_WAT_OUT_OF_RANGE;
      op.opcode = (uint8_t)tokens.Pop().i;

      if(r = WatConstantOperator(state, tokens, op))
        return r;

      if(tokens.Peek().id != TOKEN_CLOSE)
        return assert(false), ERR_WAT_INVALID_INITIALIZER;

      return ERR_SUCCESS;
    }

    int WatGlobalDesc(GlobalDesc& g, Queue<Token>& tokens)
    {
      if(tokens.Peek().id == TOKEN_OPEN)
      {
        EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
        EXPECTED(tokens, TOKEN_MUT, ERR_WAT_EXPECTED_MUT);
        g.mutability = true;
        if(!(g.type = WatValType(tokens.Pop().id)))
          return assert(false), ERR_WAT_EXPECTED_VALTYPE;

        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      }
      else
      {
        g.mutability = false;
        if(!(g.type = WatValType(tokens.Pop().id)))
          return assert(false), ERR_WAT_EXPECTED_VALTYPE;
      }

      return ERR_SUCCESS;
    }

    int WatGlobal(WatState& state, Queue<Token>& tokens, varuint32* index)
    {
      int r;
      *index = state.m.global.n_globals;
      Import* i = 0;
      if(r = WatInlineImportExport(state.m, tokens, index, WASM_KIND_GLOBAL, &i))
        return r;

      if(i) // If this is an import, assemble the aux information and abort.
        return WatGlobalDesc(i->global_desc, tokens);

      GlobalDecl g = { 0 };
      if(r = WatGlobalDesc(g.desc, tokens))
        return r;

      if(r = WatInitializer(state, tokens, g.init))
        return r;

      return AppendArray(g, state.m.global.globals, state.m.global.n_globals);
    }

    int WatMemoryDesc(MemoryDesc& m, Queue<Token>& tokens)
    {
      return WatResizableLimits(m.limits, tokens);
    }

    int WatMemory(WatState& state, Queue<Token>& tokens, varuint32* index)
    {
      int r;
      *index = state.m.memory.n_memory;
      Import* i = 0;
      if(r = WatInlineImportExport(state.m, tokens, index, WASM_KIND_MEMORY, &i))
        return r;

      if(i) // If this is an import, assemble the aux information and abort.
        return WatMemoryDesc(i->mem_desc, tokens);

      MemoryDesc mem = { 0 };

      if(tokens.Size() > 1 && tokens[0].id == TOKEN_OPEN && tokens[1].id == TOKEN_DATA)
      {
        DataInit init = { 0 };
        init.index = *index;
        init.offset = Instruction{ OP_i32_const, 0 };

        while(tokens[0].id != TOKEN_CLOSE)
        {
          if(tokens[0].id != TOKEN_STRING)
            return assert(false), ERR_WAT_EXPECTED_STRING;
          if(r = WatString(init.data, tokens.Pop()))
            return r;
        }

        mem.limits.flags = 0;
        mem.limits.minimum = init.data.n_bytes;
      }
      else if(r = WatMemoryDesc(mem, tokens))
        return r;

      return AppendArray(mem, state.m.memory.memory, state.m.memory.n_memory);
    }

    int AddWatName(kh_indexname_t* h, Token t, varuint32 index)
    {
      if(t.id == TOKEN_NAME)
      {
        int r;
        khiter_t iter = kh_put_indexname(h, t, &r);
        if(!r)
          return assert(false), ERR_WAT_DUPLICATE_NAME;
        if(iter != kh_end(h))
          kh_val(h, iter) = index;
      }

      return ERR_SUCCESS;
    }

    int WatImport(WatState& state, Queue<Token>& tokens)
    {
      Import i = { 0 };
      int r;
      if(r = WatString(i.module_name, tokens.Pop()))
        return r;
      if(r = WatString(i.export_name, tokens.Pop()))
        return r;

      EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);

      Token t = tokens.Pop();
      Token name = GetWatNameToken(tokens);
      kh_indexname_t* hash = 0;
      switch(t.id)
      {
      case TOKEN_FUNC:
        if(r = WatName(i.func_desc.debug_name, name))
          return r;
        if(r = WatFuncType(state, tokens, i.func_desc.sig_index, &i.func_desc.param_names))
          return r;
        hash = state.funchash;
        break;
      case TOKEN_GLOBAL:
        if(r = WatGlobalDesc(i.global_desc, tokens))
          return r;
        hash = state.globalhash;
        break;
      case TOKEN_TABLE:
        if(r = WatTableDesc(i.table_desc, tokens))
          return r;
        hash = state.tablehash;
        break;
      case TOKEN_MEMORY:
        if(r = WatMemoryDesc(i.mem_desc, tokens))
          return r;
        hash = state.memoryhash;
        break;
      default:
        return assert(false), ERR_WAT_EXPECTED_KIND;
      }
      EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);

      varuint32 index;
      if(r = WatAppendImport(state.m, i, &index))
        return r;

      return AddWatName(hash, name, index);
    }

    template<int(*F)(WatState&, Queue<Token>&, varuint32*)>
    int WatIndexProcess(WatState& state, Queue<Token>& tokens, kh_indexname_t* hash)
    {
      Token t = GetWatNameToken(tokens);

      int r;
      varuint32 index = (varuint32)~0;
      if(r = (*F)(state, tokens, &index))
        return r;
      assert(index != (varuint32)~0);

      return AddWatName(hash, t, index);
    }

    int WatExport(WatState& state, Queue<Token>& tokens)
    {
      Export e = { 0 };
      int r;
      if(r = WatString(e.name, tokens.Pop()))
        return r;

      switch(tokens.Pop().id)
      {
      case TOKEN_FUNC:
        e.kind = WASM_KIND_FUNCTION;
        e.index = WatGetFromHash(state.funchash, tokens.Pop());
        break;
      case TOKEN_GLOBAL:
        e.kind = WASM_KIND_GLOBAL;
        e.index = WatGetFromHash(state.globalhash, tokens.Pop());
        break;
      case TOKEN_TABLE:
        e.kind = WASM_KIND_TABLE;
        e.index = WatGetFromHash(state.tablehash, tokens.Pop());
        break;
      case TOKEN_MEMORY:
        e.kind = WASM_KIND_MEMORY;
        e.index = WatGetFromHash(state.memoryhash, tokens.Pop());
        break;
      default:
        return assert(false), ERR_WAT_EXPECTED_KIND;
      }

      return AppendArray(e, state.m.exportsection.exports, state.m.exportsection.n_exports);
    }

    int WatElemData(WatState& state, Queue<Token>& tokens, varuint32& index, Instruction& op, kh_indexname_t* hash)
    {
      if(tokens[0].id == TOKEN_INTEGER || tokens[0].id == TOKEN_NAME)
        index = WatGetFromHash(hash, tokens.Pop());

      if(index == ~0)
        return assert(false), ERR_WAT_INVALID_VAR;

      if(tokens[0].id == TOKEN_OPEN)
      {
        EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
        if(tokens[0].id == TOKEN_OFFSET)
          tokens.Pop();

        int r;
        if(tokens.Peek().i > 0xFF)
          return ERR_WAT_OUT_OF_RANGE;
        op = { (uint8_t)tokens.Pop().i };
        if(r = WatConstantOperator(state, tokens, op))
          return r;

        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      }

      return ERR_SUCCESS;
    }

    int WatElem(WatState& state, Queue<Token>& tokens)
    {
      TableInit e = { 0 };
      int r;
      if(r = WatElemData(state, tokens, e.index, e.offset, state.tablehash))
        return r;

      while(tokens[0].id != TOKEN_CLOSE)
      {
        AppendArray(WatGetFromHash(state.funchash, tokens.Pop()), e.elems, e.n_elems);
        if(e.elems[e.n_elems - 1] == (varuint32)~0)
          return assert(false), ERR_WAT_INVALID_VAR;
      }

      return AppendArray(e, state.m.element.elements, state.m.element.n_elements);
    }

    int WatData(WatState& state, Queue<Token>& tokens)
    {
      DataInit d = { 0 };
      int r;
      if(r = WatElemData(state, tokens, d.index, d.offset, state.memoryhash))
        return r;

      while(tokens[0].id != TOKEN_CLOSE)
      {
        if(tokens[0].id != TOKEN_STRING)
          return assert(false), ERR_WAT_EXPECTED_STRING;
        WatString(d.data, tokens.Pop());
      }

      return AppendArray(d, state.m.data.data, state.m.data.n_data);
    }

    // Skips over an entire section of tokens by counting paranthesis, assuming they are well-formed
    void SkipSection(Queue<Token>& tokens)
    {
      int count = 1; // Assume we are already inside a section
      while(tokens.Size())
      {
        if(tokens[0].id == TOKEN_OPEN)
          ++count;
        else if(tokens[0].id == TOKEN_CLOSE)
        {
          if(!--count)
            break; // Deliberately do not pop the CLOSE token because we usually need it afterwards
        }
        tokens.Pop();
      }
    }

    int WatModule(Environment& env, Module& m, Queue<Token>& tokens, StringRef name)
    {
      int r;
      m = { 0 };
      if(name.s)
        if(r = WatName(m.name, Token{ TOKEN_NAME, (const char*)name.s, (int64_t)name.len }))
          return r;

      if(tokens.Peek().id == TOKEN_NAME)
        if(r = WatName(m.name, tokens.Pop()))
          return r;

      WatState state(m);

      Token t;
      size_t restore = tokens.GetPosition();
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
        default:
          SkipSection(tokens);
          break;
        }
        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      }

      // This is the main pass for functions/imports/etc. and also identifies illegal tokens
      tokens.SetPosition(restore);
      while(tokens.Size() > 0 && tokens.Peek().id != TOKEN_CLOSE)
      {
        EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
        t = tokens.Pop();
        switch(t.id)
        {
        case TOKEN_FUNC:
        {
          khiter_t iter = kh_end(state.funchash);
          if(tokens.Peek().id == TOKEN_NAME)
          {
            iter = kh_put_indexname(state.funchash, tokens.Pop(), &r);
            if(!r)
              return assert(false), ERR_WAT_DUPLICATE_NAME;
          }

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
          if(r = WatImport(state, tokens))
            return r;
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
        case TOKEN_EXPORT:
        case TOKEN_TYPE:
        case TOKEN_ELEM:
        case TOKEN_DATA:
        case TOKEN_START:
          SkipSection(tokens);
          break;
        default:
          return assert(false), ERR_WAT_INVALID_TOKEN;
        }
        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      }

      // This pass resolves exports, elem, data, and the start function, to minimize deferred actions
      tokens.SetPosition(restore);
      while(tokens.Size() > 0 && tokens.Peek().id != TOKEN_CLOSE)
      {
        EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
        t = tokens.Pop();
        switch(t.id)
        {
        case TOKEN_EXPORT:
          if(r = WatExport(state, tokens))
            return r;
          break;
        case TOKEN_ELEM:
          if(r = WatElem(state, tokens))
            return r;
          break;
        case TOKEN_DATA:
          if(r = WatData(state, tokens))
            return r;
          break;
        case TOKEN_START:
          if(tokens[0].id != TOKEN_INTEGER && tokens[0].id != TOKEN_NAME)
            return assert(false), ERR_WAT_EXPECTED_VAR;
          m.start = WatGetFromHash(state.funchash, tokens.Pop());
          m.knownsections |= (1 << WASM_SECTION_START);
          if(m.start == (varuint32)~0)
            return assert(false), ERR_WAT_INVALID_VAR;
          break;
        default:
          SkipSection(tokens);
          break;
        }
        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      }

      // Process all deferred actions
      while(state.defer.Size() > 0)
      {
        if(state.defer[0].func < m.importsection.functions || state.defer[0].func >= m.code.n_funcbody + m.importsection.functions)
          return assert(false), ERR_INVALID_FUNCTION_INDEX;
        varuint32 e;
        switch(state.defer[0].id)
        {
        case OP_get_global:
        case OP_set_global:
          e = WatGetFromHash(state.globalhash, state.defer[0].t);
          break;
        case OP_call:
          e = WatGetFromHash(state.funchash, state.defer[0].t);
          break;
        default:
          return assert(false), ERR_WAT_INVALID_TOKEN;
        }
        auto& f = m.code.funcbody[state.defer[0].func - m.importsection.functions];
        if(state.defer[0].index >= f.n_body)
          return ERR_INVALID_FUNCTION_BODY;
        f.body[state.defer[0].index].immediates[0]._varuint32 = e;
        state.defer.Pop();
      }

      m.exports = kh_init_exports();
      return ParseExportFixup(m);
    }

    int WatEnvironment(Environment& env, Queue<Token>& tokens)
    {
      return 0;
    }

    int ParseWatModule(Environment& env, Module& m, uint8_t* data, size_t sz, StringRef name)
    {
      Queue<Token> tokens;
      TokenizeWAT(tokens, (char*)data, (char*)data + sz);

      // If we don't detect (module, just assume it's an inline module
      if(tokens[0].id != TOKEN_OPEN || tokens[1].id != TOKEN_MODULE)
        return WatModule(env, m, tokens, name);

      EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
      EXPECTED(tokens, TOKEN_MODULE, ERR_WAT_EXPECTED_MODULE);
      int r = WatModule(env, m, tokens, name);
      if(!r)
        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      return r;
    }
  }
}