// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef __LEXER_H__IN__
#define __LEXER_H__IN__

#include "innative/schema.h"
#include "util.h"
#include "queue.h"

namespace innative {
  typedef unsigned short WatTokenID;

  typedef struct __WAT_TOKEN
  {
    WatTokenID id;
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
    enum WatTokens : WatTokenID
    {
      TOKEN_NONE = 0, // Error state
      TOKEN_OPEN, // (
      TOKEN_CLOSE, // )
      TOKEN_MODULE,
      TOKEN_IMPORT,
      TOKEN_TYPE,
      TOKEN_START,
      TOKEN_FUNC,
      TOKEN_TABLE,
      TOKEN_MEMORY,
      TOKEN_GLOBAL,
      TOKEN_EXPORT,
      TOKEN_DATA,
      TOKEN_ELEM,
      TOKEN_OFFSET,
      TOKEN_ALIGN,
      TOKEN_LOCAL,
      TOKEN_RESULT,
      TOKEN_PARAM,
      TOKEN_i32,
      TOKEN_i64,
      TOKEN_f32,
      TOKEN_f64,
      TOKEN_FUNCREF,
      TOKEN_CREF,
      TOKEN_MUT,
      TOKEN_BLOCK,
      TOKEN_LOOP,
      TOKEN_IF,
      TOKEN_THEN,
      TOKEN_ELSE,
      TOKEN_END,
      TOKEN_BINARY, // Script extension tokens
      TOKEN_QUOTE,
      TOKEN_REGISTER,
      TOKEN_INVOKE,
      TOKEN_GET,
      TOKEN_ASSERT_RETURN,
      TOKEN_ASSERT_RETURN_CANONICAL_NAN,
      TOKEN_ASSERT_RETURN_ARITHMETIC_NAN,
      TOKEN_ASSERT_TRAP,
      TOKEN_ASSERT_MALFORMED,
      TOKEN_ASSERT_INVALID,
      TOKEN_ASSERT_UNLINKABLE,
      TOKEN_ASSERT_EXHAUSTION,
      TOKEN_SCRIPT,
      TOKEN_INPUT,
      TOKEN_OUTPUT,
      TOKEN_NUMBER,
      TOKEN_STRING,
      TOKEN_NAME, // Same as string but more restricted
      TOKEN_OPERATOR, // Represents all operators other than the control flow ones above
      TOKEN_COMMENT,
      TOKEN_RANGE_ERROR,
      TOKEN_INTEGER, // Explicit integer/float are only used as a shortcut when generating token lists
      TOKEN_FLOAT,
      TOKEN_TOTALCOUNT,
    };

    const char* GetTokenString(WatTokenID token);
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