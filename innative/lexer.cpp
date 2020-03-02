// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "utility.h"
#include "wat.h"
#include "parse.h"
#include "validate.h"
#include <limits>
#include <cmath>

using std::numeric_limits;
using std::string;

using namespace innative;
using namespace utility;
using namespace wat;

namespace innative {
  namespace wat {
    KHASH_INIT(tokens, StringSpan, WatTokens, 1, internal::__ac_X31_hash_stringrefins, kh_int_hash_equal);

    template<int LEN>
    inline kh_tokens_t* GenTokenHash(const char* (&list)[LEN],
                                     std::initializer_list<std::pair<const char*, WatTokens>> legacy)
    {
      kh_tokens_t* h = kh_init_tokens();

      unsigned int count = 0;
      int r;
      for(int i = 0; i < LEN; ++i)
      {
        auto iter       = kh_put_tokens(h, StringSpan{ list[i], strlen(list[i]) }, &r);
        kh_val(h, iter) = WatTokens(count++);
      }

      for(auto& e : legacy)
      {
        auto iter       = kh_put_tokens(h, StringSpan{ e.first, strlen(e.first) }, &r);
        kh_val(h, iter) = e.second;
      }

      return h;
    }

    static const char* tokenlist[] = { "[NONE]",
                                       "(",
                                       ")",
                                       "module",
                                       "import",
                                       "type",
                                       "start",
                                       "func",
                                       "table",
                                       "memory",
                                       "global",
                                       "export",
                                       "data",
                                       "elem",
                                       "offset",
                                       "align",
                                       "local",
                                       "result",
                                       "param",
                                       "i32",
                                       "i64",
                                       "f32",
                                       "f64",
                                       "funcref",
                                       "cref",
                                       "mut",
                                       "block",
                                       "loop",
                                       "if",
                                       "then",
                                       "else",
                                       "end",
                                       "binary", // script expressions
                                       "quote",
                                       "register",
                                       "invoke",
                                       "get",
                                       "assert_return",
                                       "assert_return_canonical_nan",
                                       "assert_return_arithmetic_nan",
                                       "assert_trap",
                                       "assert_malformed",
                                       "assert_invalid",
                                       "assert_unlinkable",
                                       "assert_exhaustion",
                                       "script",
                                       "input",
                                       "output" };

    static const kh_tokens_t* tokenhash = GenTokenHash(tokenlist, { { "anyfunc", WatTokens::FUNCREF } });

    const char* GetTokenString(WatTokens token)
    {
      constexpr int len = sizeof(tokenlist) / sizeof(decltype(tokenlist[0]));
      return ((token != WatTokens::NONE) && (static_cast<decltype(len)>(token) < len)) ?
               tokenlist[static_cast<decltype(len)>(token)] :
               0;
    }

    const char* CheckTokenINF(const char* s, const char* end, std::string* target)
    {
      if(s >= end)
        return nullptr;

      const char* begin = s;
      if(s[0] == '-' || s[0] == '+')
        ++s;
      int i;
      for(i = 0; i < 3 && s < end; ++i)
      {
        if(s[i] != "inf"[i] && s[i] != "INF"[i])
          return nullptr;
      }
      if(i != 3)
        return nullptr;
      s += 3;
      if(target)
        target->assign(begin, s - begin);
      return s;
    }

    const char* CheckTokenNAN(const char* s, const char* end, std::string* target)
    {
      if(s >= end)
        return nullptr;

      if(s[0] == '-' || s[0] == '+')
        ++s;
      const char* begin = s;
      int i;
      for(i = 0; i < 3 && s < end; ++i)
      {
        if(s[i] != "nan"[i] && s[i] != "NAN"[i])
          return nullptr;
      }
      if(i != 3)
        return nullptr;
      s += 3;
      if(s >= end)
        return end;

      for(i = 0; i < 3 && s < end; ++i)
      {
        if(s[i] != ":0x"[i])
          return s;
      }
      s += i;

      if(target)
        target->assign(begin + 3 + i, s - begin - 3 - i);

      while(s < end && (*s == '_' || isxdigit(*s)))
      {
        if(target && *s != '_')
          target->append(1, *s);
        ++s;
      }

      return s;
    }

    template<typename T, typename Arg, typename... Args>
    int ResolveTokenNumber(const WatToken& token, string& numbuf, Arg (*fn)(const char*, char**, Args...), T& out,
                           Args... args)
    {
      numbuf.clear();
      size_t length          = token.len;
      int (*digitcheck)(int) = (token.len > 2 && token.pos[0] == '0' && token.pos[1] == 'x') ? &isxdigit : &isdigit;
      for(size_t i = 0; i < token.len; ++i)
      {
        if(token.pos[i] == '_')
        {
          if(!i || (i + 1) >= token.len || !(*digitcheck)(token.pos[i - 1]) ||
             !(*digitcheck)(token.pos[i + 1])) // If it's a _, it's valid only if it's surrounded by valid digits
            return ERR_WAT_INVALID_NUMBER;
          --length; // Compensate for the character we removed from the amount we expect to consume
        }
        else // otherwise, only add all non-underscore characters
          numbuf += token.pos[i];
      }

      if(digitcheck ==
         &isdigit) // If this is a decimal number, strip all leading 0s because otherwise it'll be considered octal
      {
        size_t iter = numbuf.find_first_not_of('0');
        if(iter != std::string::npos && iter > 0)
        {
          numbuf.erase(0, iter);
          length -= iter;
        }
      }

      errno = 0;
      char* end;
      out = (*fn)(numbuf.c_str(), &end, args...);
#ifdef IN_PLATFORM_POSIX
      if(std::is_floating_point<T>::value)
      {
        if(std::isinf(out)) // libc incorrectly parses certain edge cases as "inf" without setting errno to ERANGE
        {
          const char* p = numbuf.c_str();
          if(*p == '+' || *p == '-')
            ++p;
          if(strncasecmp(p, "inf", 3) != 0)
            return ERR_WAT_OUT_OF_RANGE;
        }
      }
#endif
      if(std::is_same<float, T>::value &&
         fabs(out) <= 1.1754942e-38f) // WebAssembly never considers an underflow to be a range error, it rounds to zero
        errno = 0;
      if(std::is_same<double, T>::value && fabs(out) <= 2.2250738585072012e-308)
        errno = 0;
      if(errno == ERANGE)
        return ERR_WAT_OUT_OF_RANGE;
      // assert(!(errno != 0 || (end - numbuf.c_str()) != length));
      return (errno != 0 || (end - numbuf.c_str()) != length) ? ERR_WAT_INVALID_NUMBER : ERR_SUCCESS;
    }

    int ResolveTokenf32(const WatToken& token, string& numbuf, float32& out)
    {
      char* last;

      numbuf.assign("400000"); // Hex for the first bit in the mantissa
      if(CheckTokenNAN(token.pos, token.pos + token.len, &numbuf))
      {
        auto mantissa = strtoul(numbuf.c_str(), &last, 16);
        if(mantissa < 0x1 || mantissa > 0x7fffff)
          return ERR_WAT_OUT_OF_RANGE;
        union
        {
          uint32_t i;
          float f;
        } u = { 0x7F800000U | (uint32_t)mantissa };
        if(token.pos[0] == '-')
          u.i |= 0x80000000U;

        out = u.f;
        return ERR_SUCCESS;
      }

      if(CheckTokenINF(token.pos, token.pos + token.len, &numbuf) != nullptr)
      {
        out = strtof(numbuf.c_str(), &last);
        return (last - numbuf.c_str()) == numbuf.size() ? ERR_SUCCESS : ERR_WAT_INVALID_NUMBER;
      }
      return ResolveTokenNumber<float32>(token, numbuf, &strtof, out);
    }

    int ResolveTokenf64(const WatToken& token, string& numbuf, float64& out)
    {
      char* last;

      numbuf.assign("8000000000000"); // Hex for the first bit in the mantissa
      if(CheckTokenNAN(token.pos, token.pos + token.len, &numbuf))
      {
        auto mantissa = strtoull(numbuf.c_str(), &last, 16);
        if(mantissa < 0x1 || mantissa > 0xfffffffffffff)
          return ERR_WAT_OUT_OF_RANGE;
        union
        {
          uint64_t i;
          double f;
        } u = { 0x7FF0000000000000ULL | mantissa };
        if(token.pos[0] == '-')
          u.i |= 0x8000000000000000ULL;

        out = u.f;
        return ERR_SUCCESS;
      }

      if(CheckTokenINF(token.pos, token.pos + token.len, &numbuf) != nullptr)
      {
        out = strtod(numbuf.c_str(), &last);
        return (last - numbuf.c_str()) == numbuf.size() ? ERR_SUCCESS : ERR_WAT_INVALID_NUMBER;
      }
      return ResolveTokenNumber<float64>(token, numbuf, &strtod, out);
    }

    int ResolveTokeni64(const WatToken& token, string& numbuf, varsint64& out)
    {
      if(token.len > 0 && token.pos[0] == '-')
        return ResolveTokenNumber<varsint64, long long, int>(token, numbuf, strtoll, out, 0);
      return ResolveTokenNumber<varsint64, unsigned long long, int>(token, numbuf, strtoull, out, 0);
    }

    int ResolveTokenu64(const WatToken& token, string& numbuf, varuint64& out)
    {
      if(token.len > 0 && token.pos[0] == '-')
        return ERR_WAT_OUT_OF_RANGE;
      return ResolveTokeni64(token, numbuf, reinterpret_cast<varsint64&>(out));
    }

    int ResolveTokeni32(const WatToken& token, string& numbuf, varsint32& out)
    {
      varsint64 buf;
      int err = ResolveTokeni64(token, numbuf, buf);
      if(err)
        return err;
      if((buf < std::numeric_limits<varsint32>::min()) || (buf > (varsint64)std::numeric_limits<varuint32>::max()))
        return ERR_WAT_OUT_OF_RANGE;

      out = (varsint32)buf;
      return ERR_SUCCESS;
    }

    int ResolveTokenu32(const WatToken& token, string& numbuf, varuint32& out)
    {
      varsint64 buf;
      int err = ResolveTokeni64(token, numbuf, buf);
      if(err)
        return err;
      if((buf < 0) || (buf > (varsint64)std::numeric_limits<varuint32>::max()))
        return ERR_WAT_OUT_OF_RANGE;

      out = (varsint32)buf;
      return ERR_SUCCESS;
    }

    IN_FORCEINLINE const char* IncToken(const char*& s, const char* end, unsigned int& line, unsigned int& column)
    {
      ++s;
      if(s + 1 < end && ((s[0] == '\r' && s[1] != '\n') || s[0] == '\n'))
      {
        ++line;
        column = 0;
      }
      else
        ++column;
      return s;
    }
  }
}

void innative::TokenizeWAT(Queue<WatToken>& tokens, const char* s, const char* end)
{
  unsigned int line   = 1;
  unsigned int column = 0;
  while(s < end)
  {
    while(s < end && (s[0] == ' ' || s[0] == '\n' || s[0] == '\r' || s[0] == '\t' || s[0] == '\f'))
      IncToken(s, end, line, column);

    if(s >= end)
      break;

    switch(s[0])
    {
    case 0:
      assert(s < end);
      IncToken(s, end, line, column);
      break;
    case '(':
      if(s + 1 < end && s[1] == ';') // This is a comment
      {
        IncToken(s, end, line, column);
        IncToken(s, end, line, column);
        size_t depth = 1;
        while(depth > 0 && s < end)
        {
          switch(*s)
          {
          case '(':
            if(s + 1 < end && s[1] == ';')
              depth += 1;
            IncToken(s, end, line, column);
            break;
          case ';':
            if(s + 1 < end && s[1] == ')')
              depth -= 1;
            IncToken(s, end, line, column);
            break;
          }
          IncToken(s, end, line, column);
        }
      }
      else
      {
        tokens.Push(WatToken{ WatTokens::OPEN, s, line, column });
        IncToken(s, end, line, column);
      }
      break;
    case ')':
      tokens.Push(WatToken{ WatTokens::CLOSE, s, line, column });
      IncToken(s, end, line, column);
      break;
    case ';': // A comment
    {
      if(s + 1 < end && s[1] == ';')
      {
        do
        {
          IncToken(s, end, line, column);
        } while(s < end && s[0] != '\n');
      }
      else
      {
        tokens.Push(WatToken{ WatTokens::NONE });
      }

      if(s < end)
        IncToken(s, end, line, column);

      break;
    }
    case '"': // A string
    {
      const char* begin = IncToken(s, end, line, column);
      while(s[0] != '"' && s + 1 < end)
      {
        if(s[0] == '\\')
          IncToken(s, end, line, column);
        IncToken(s, end, line, column);
      }

      WatToken t = { WatTokens::STRING, begin, line };
      t.len      = s - begin;
      tokens.Push(t);

      if(s[0] == '"')
        IncToken(s, end, line, column);
      break;
    }
    case '$': // A name
    {
      WatToken t = { WatTokens::NAME, s + 1, line };

      // We avoid using a regex here because extremely long names are still technically valid but can overwhelm the standard
      // C++ regex evaluator
      while(s < end)
      {
        IncToken(s, end, line, column);
        switch(*s)
        {
        case '!':
        case '#':
        case '$':
        case '%':
        case '&':
        case '\'':
        case '*':
        case '+':
        case '-':
        case '.':
        case '/':
        case ':':
        case '<':
        case '=':
        case '>':
        case '?':
        case '@':
        case '\\':
        case '^':
        case '_':
        case '`':
        case '|':
        case '~': t.len++; continue;
        default:
          if(isalnum(s[0]))
          {
            t.len++;
            continue;
          }
        }
        break;
      }

      if(!t.len) // Empty names are invalid
        t.id = WatTokens::NONE;

      tokens.Push(t);
      break;
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
      const char* last = s;
      if(!(last = CheckTokenNAN(s, end, 0)) && !(last = CheckTokenINF(s, end, 0))) // Check if this is an NaN or an INF
      {
        last = s; // If it's not an NAN, estimate what the number is
        if(last[0] == '-' || last[0] == '+')
          ++last;
        if(last + 2 < end && last[0] == '0' && last[1] == 'x')
          last += 2;
        if(last >= end || !isxdigit(last[0]))
        {
          tokens.Push(WatToken{ WatTokens::NONE, s, line, column, last - s });
          column += (uint32_t)(last - s);
          s = last;
          break;
        }
        while(last < end && (isalnum(last[0]) || last[0] == '.' || last[0] == '_' || last[0] == '-' || last[0] == '+'))
          ++last;
      }
      tokens.Push(WatToken{ WatTokens::NUMBER, s, line, column, last - s });
      column += (uint32_t)(last - s);
      s = last;
      break;
    }
    default:
    {
      const char* begin = s;
      if((begin = CheckTokenNAN(s, end, 0)) != 0 || (begin = CheckTokenINF(s, end, 0)) != 0) // Check if this is an NaN
      {
        tokens.Push(WatToken{ WatTokens::NUMBER, s, line, column, begin - s });
        column += (uint32_t)(begin - s);
        s = begin;
      }
      else
      {
        begin = s;

        while(s < end && s[0] != ' ' && s[0] != '\n' && s[0] != '\r' && s[0] != '\t' && s[0] != '\f' && s[0] != '=' &&
              s[0] != ')' && s[0] != '(' && s[0] != ';')
          IncToken(s, end, line, column);

        StringSpan ref = { begin, static_cast<size_t>(s - begin) };
        khiter_t iter  = kh_get_tokens(tokenhash, ref);
        if(kh_exist2(tokenhash, iter))
          tokens.Push(WatToken{ kh_val(tokenhash, iter), begin, line, column });
        else
        {
          uint8_t op = GetInstruction(ref);
          if(op != 0xFF)
            tokens.Push(WatToken{ WatTokens::OPERATOR, begin, line, column, (int64_t)op });
          else
          {
            tokens.Push(WatToken{ WatTokens::NONE, begin, line, column, (int64_t)ref.len });
          }
        }
        if(*s == '=')
          IncToken(s, end, line, column);
      }
    }
    }
    if(tokens.Size() > 0)
      assert(tokens.Peek().id < WatTokens::TOTALCOUNT);
  }
}

// Checks for parse errors in the tokenization process
int innative::CheckWatTokens(const Environment& env, ValidationError*& errors, Queue<WatToken>& tokens, const char* start)
{
  int err = ERR_SUCCESS;
  for(size_t i = 0; i < tokens.Size(); ++i)
  {
    switch(tokens[i].id)
    {
    case WatTokens::NONE:
      AppendError(env, errors, nullptr, ERR_WAT_INVALID_TOKEN, "[%zu] Invalid token: %s",
                  WatLineNumber(start, tokens[i].pos), string(tokens[i].pos, tokens[i].len).c_str());
      break;
    case WatTokens::RANGE_ERROR:
      AppendError(env, errors, nullptr, ERR_WAT_OUT_OF_RANGE, "[%zu] Constant out of range: %s",
                  WatLineNumber(start, tokens[i].pos), string(tokens[i].pos, tokens[i].len).c_str());
      break;
    default: continue;
    }
    err = ERR_WAT_INVALID_TOKEN;
  }

  return err;
}