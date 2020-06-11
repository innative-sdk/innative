// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__WAT_H
#define IN__WAT_H

#include "lexer.h"
#include "stack.h"
#include "validate.h"

namespace innative {
  namespace wat {
    KHASH_DECLARE(indexname, utility::StringSpan, varuint32);
  }

  struct WatParser
  {
    struct DeferWatAction
    {
      int id;
      WatToken t;
      uint64_t func;
      uint64_t index;
    };

    WatParser(Environment& e, Module& mod);
    ~WatParser();
    varuint32 GetJump(WatToken var);
    int ParseInitializer(Queue<WatToken>& tokens, Instruction& op);
    int ParseFunctionType(Queue<WatToken>& tokens, WatToken token);
    static int ParseFunctionTypeInner(Environment& env, Queue<WatToken>& tokens, FunctionType& sig, DebugInfo** info,
                                      varuint32* n_info, bool anonymous);
    varuint32 GetFromHash(wat::kh_indexname_t* hash, const WatToken& t);
    int MergeFunctionType(const FunctionType& ftype, varuint32& out);
    int ParseType(Queue<WatToken>& tokens, varuint32& sig);
    int ParseTypeUse(Queue<WatToken>& tokens, varuint32& sig, DebugInfo** info, varuint32* n_info, bool anonymous);
    varuint32 GetLocal(FunctionBody& f, FunctionDesc& desc, varuint32 n_param, const WatToken& t);
    varuint32 GetMemory(const WatToken& t);
    int ParseConstantOperator(Queue<WatToken>& tokens, Instruction& op);
    int ParseOperator(Queue<WatToken>& tokens, Instruction& op, FunctionBody& f, FunctionDesc& desc, FunctionType& sig,
                      DeferWatAction& defer);
    int ParseMemarg(Queue<WatToken>& tokens, Instruction& op);
    void ParseLabel(Queue<WatToken>& tokens);
    bool CheckLabel(Queue<WatToken>& tokens);
    int ParseInstruction(Queue<WatToken>& tokens, FunctionBody& f, FunctionDesc& desc, FunctionType& sig, varuint32 index);
    int ParseExpression(Queue<WatToken>& tokens, FunctionBody& f, FunctionDesc& desc, FunctionType& sig, varuint32 index);
    int ParseFunction(Queue<WatToken>& tokens, varuint32* index, utility::StringSpan name);
    int ParseResizableLimits(ResizableLimits& limits, Queue<WatToken>& tokens);
    int ParseTableDesc(TableDesc& t, Queue<WatToken>& tokens);
    int ParseTable(Queue<WatToken>& tokens, WatToken token);
    int ParseInitializerInstruction(Queue<WatToken>& tokens, Instruction& op, bool expr);
    int ParseGlobalDesc(GlobalDesc& g, Queue<WatToken>& tokens);
    int ParseGlobal(Queue<WatToken>& tokens, WatToken token);
    int ParseMemoryDesc(MemoryDesc& m, Queue<WatToken>& tokens);
    int ParseMemory(Queue<WatToken>& tokens, WatToken token);
    int ParseImport(Queue<WatToken>& tokens);
    int ParseExport(Queue<WatToken>& tokens);
    int ParseElemData(Queue<WatToken>& tokens, varuint32& index, Instruction& op, wat::kh_indexname_t* hash);
    int ParseElem(TableInit& e, Queue<WatToken>& tokens);
    int ParseData(Queue<WatToken>& tokens);
    int AppendImport(Module& m, const Import& i, varuint32* index);
    int InlineImportExport(Module& m, Queue<WatToken>& tokens, varuint32* index, varuint7 kind, Import** out);
    int ParseBlockType(Queue<WatToken>& tokens, varsint64& out);

    static int ParseModule(Environment& env, Module& m, const char* file, Queue<WatToken>& tokens, utility::StringSpan name,
                           WatToken& internalname);
    static int ParseName(Environment& env, ByteArray& name, const WatToken& t);
    static int AddWatValType(Environment& env, WatTokens id, varsint7*& a, varuint32& n);
    static int WatString(const Environment& env, ByteArray& str, utility::StringSpan t);
    static void WriteUTF32(uint32_t ch, ByteArray& str, varuint32& index);
    static varsint7 WatValType(WatTokens id);
    static int ParseLocalAppend(const Environment& env, FunctionLocal& local, FunctionBody& body, Queue<WatToken>& tokens);
    static int AddName(wat::kh_indexname_t* h, WatToken t, varuint32 index, DebugInfo* info);

    IN_FORCEINLINE static int WatString(const Environment& env, ByteArray& str, const WatToken& t)
    {
      if(t.id != WatTokens::STRING)
        return ERR_WAT_EXPECTED_STRING;
      return WatString(env, str, utility::StringSpan{ t.pos, t.len });
    }

    IN_FORCEINLINE static WatToken GetWatNameToken(Queue<WatToken>& tokens)
    {
      return (tokens.Peek().id == WatTokens::NAME) ? tokens.Pop() : WatToken{ WatTokens::NONE };
    }

    template<class T> inline static int AppendArray(const Environment& env, T item, T*& a, varuint32& n)
    {
      int err;
      if((err = utility::ReallocArray(env, a, n)) != ERR_SUCCESS)
        return err;
      a[n - 1] = item;
      return ERR_SUCCESS;
    }

    template<int (WatParser::*F)(Queue<WatToken>&, WatToken)> inline int ParseIndexProcess(Queue<WatToken>& tokens)
    {
      return (this->*F)(tokens, GetWatNameToken(tokens));
    }

    template<typename T, int (*FN)(const WatToken&, std::string&, T&)> inline T ResolveInlineToken(const WatToken& token)
    {
      T t;
      int err = (*FN)(token, numbuf, t);
      return !err ? t : (T)~0;
    }

    Environment& env;
    Module& m;
    Queue<DeferWatAction> deferred;
    Stack<utility::StringSpan> stack;
    wat::kh_indexname_t* typehash;
    wat::kh_indexname_t* funchash;
    wat::kh_indexname_t* tablehash;
    wat::kh_indexname_t* memoryhash;
    wat::kh_indexname_t* globalhash;
    std::string numbuf;
  };

  inline int WatAppendError(Environment& env, const WatToken& t, int err)
  {
    AppendError(env, env.errors, 0, err, "Unexpected %hu at [%u:%u]", t.id, t.line, t.column);
    return err;
  }

#define EXPECTED(env, t, e, err)               \
  if((t).Size() == 0)                          \
    return (err);                              \
  else                                         \
  {                                            \
    auto ztoken = (t).Pop();                   \
    if(ztoken.id != (e))                       \
      return WatAppendError(env, ztoken, err); \
  }

  int ParseWatModule(Environment& env, const char* file, Module& m, const uint8_t* data, size_t sz,
                     utility::StringSpan name);
  void WatSkipSection(Queue<WatToken>& tokens, ptrdiff_t count = 1);
  size_t WatLineNumber(const char* start, const char* pos);
}

#endif