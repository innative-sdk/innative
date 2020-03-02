// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__LEXER_H
#define IN__LEXER_H

#include "innative/schema.h"
#include "utility.h"
#include "queue.h"

namespace innative {
  enum class WatTokens : unsigned short
  {
    NONE = 0, // Error state
    OPEN,     // (
    CLOSE,    // )
    MODULE,
    IMPORT,
    TYPE,
    START,
    FUNC,
    TABLE,
    MEMORY,
    GLOBAL,
    EXPORT,
    DATA,
    ELEM,
    OFFSET,
    ALIGN,
    LOCAL,
    RESULT,
    PARAM,
    i32,
    i64,
    f32,
    f64,
    FUNCREF,
    CREF,
    MUT,
    BLOCK,
    LOOP,
    IF,
    THEN,
    ELSE,
    END,
    BINARY, // Script extension tokens
    QUOTE,
    REGISTER,
    INVOKE,
    GET,
    ASSERT_RETURN,
    ASSERT_RETURN_CANONICAL_NAN,
    ASSERT_RETURN_ARITHMETIC_NAN,
    ASSERT_TRAP,
    ASSERT_MALFORMED,
    ASSERT_INVALID,
    ASSERT_UNLINKABLE,
    ASSERT_EXHAUSTION,
    SCRIPT,
    INPUT,
    OUTPUT,
    NUMBER,
    STRING,
    NAME,     // Same as string but more restricted
    OPERATOR, // Represents all operators other than the control flow ones above
    COMMENT,
    RANGE_ERROR,
    INTEGER, // Explicit integer/float are only used as a shortcut when generating token lists
    FLOAT,
    DEBUG_INFO, // only used in the serializer
    TOTALCOUNT,
  };

  typedef struct __WAT_TOKEN
  {
    WatTokens id;
    const char* pos;
    unsigned int line;
    unsigned int column;
    union
    {
      int64_t i;
      uint64_t u;
      double f;
      size_t len;
    };
  } WatToken;

  namespace wat {
    const char* GetTokenString(WatTokens token);
    int ResolveTokeni32(const WatToken& token, std::string& numbuf, varsint32& out);
    int ResolveTokenu32(const WatToken& token, std::string& numbuf, varuint32& out);
    int ResolveTokenf32(const WatToken& token, std::string& numbuf, float32& out);
    int ResolveTokeni64(const WatToken& token, std::string& numbuf, varsint64& out);
    int ResolveTokenu64(const WatToken& token, std::string& numbuf, varuint64& out);
    int ResolveTokenf64(const WatToken& token, std::string& numbuf, float64& out);
  }

  void TokenizeWAT(Queue<WatToken>& tokens, const char* s, const char* end);
  int CheckWatTokens(const Environment& env, ValidationError*& errors, Queue<WatToken>& tokens, const char* start);
}

#endif