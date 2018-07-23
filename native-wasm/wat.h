// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in native-wasm.h

#ifndef __WAT_H__NW__
#define __WAT_H__NW__

#include "native-wasm/schema.h"
#include "util.h"
#include <istream>

int ParseWat(Environment& env, uint8_t* data, size_t sz);
int ParseWatModule(Environment& env, Module& m, uint8_t* data, size_t sz, StringRef name);

typedef unsigned short TokenID;

enum Tokens : TokenID
{
  TOKEN_NONE = 0, // Error state
  TOKEN_OPEN, // (
  TOKEN_CLOSE, // )
  TOKEN_MODULE,
  TOKEN_IMPORT,
  TOKEN_TYPE,
  TOKEN_START,
  TOKEN_FUNC,
  TOKEN_GLOBAL,
  TOKEN_TABLE,
  TOKEN_MEMORY,
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
  TOKEN_ANYFUNC,
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
  TOKEN_INTEGER, // General tokens
  TOKEN_FLOAT,
  TOKEN_STRING,
  TOKEN_NAME, // Same as string but more restricted
  TOKEN_OPERATOR, // Represents all operators other than the control flow ones above
  TOKEN_COMMENT,
};

struct Token
{
  TokenID id;
  const char* pos;
  union
  {
    int64_t i;
    double f;
    size_t len;
  };
};

#endif