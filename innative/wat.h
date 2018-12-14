// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef __WAT_H__IR__
#define __WAT_H__IR__

#include "lexer.h"
#include "stack.h"

namespace innative {
  namespace wat {
    struct DeferWatAction
    {
      int id;
      WatToken t;
      uint64_t func;
      uint64_t index;
    };

    KHASH_DECLARE(indexname, utility::StringRef, varuint32);

    struct WatState
    {
      WatState(Environment& e, Module& mod);
      ~WatState();
      varuint32 GetJump(WatState& state, WatToken var);

      Environment& env;
      Module& m;
      Queue<DeferWatAction> defer;
      Stack<utility::StringRef> stack;
      kh_indexname_t* typehash;
      kh_indexname_t* funchash;
      kh_indexname_t* tablehash;
      kh_indexname_t* memoryhash;
      kh_indexname_t* globalhash;
      std::string numbuf;
    };

#define EXPECTED(t, e, err) if((t).Size() == 0 || (t).Pop().id != (e)) return assert(false), (err)

    int ParseWatModule(Environment& env, Module& m, uint8_t* data, size_t sz, utility::StringRef name);
    int WatString(const Environment& env, ByteArray& str, utility::StringRef ref);
    void SkipSection(Queue<WatToken>& tokens, int count = 1);
    int WatInitializer(WatState& state, Queue<WatToken>& tokens, Instruction& op);
    int WatName(const Environment& env, ByteArray& name, const WatToken& t);
    int WatModule(Environment& env, Module& m, Queue<WatToken>& tokens, utility::StringRef name, WatToken& internalname);
    size_t WatLineNumber(const char* start, const char* pos);

    IR_FORCEINLINE int WatString(const Environment& env, ByteArray& str, const WatToken& t)
    {
      if(t.id != TOKEN_STRING)
        return ERR_WAT_EXPECTED_STRING;
      return WatString(env, str, utility::StringRef{ t.pos, t.len });
    }

    IR_FORCEINLINE WatToken GetWatNameToken(Queue<WatToken>& tokens)
    {
      return (tokens.Peek().id == TOKEN_NAME) ? tokens.Pop() : WatToken{ TOKEN_NONE };
    }
  }
}

#endif