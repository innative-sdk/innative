// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__WAT_H
#define IN__WAT_H

#include "lexer.h"
#include "stack.h"

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
    int ParseFunctionType(Queue<WatToken>& tokens, varuint32* index);
    static int ParseFunctionTypeInner(const Environment& env, Queue<WatToken>& tokens, FunctionType& sig, DebugInfo** info,
                                      varuint32* n_info, bool anonymous);
    varuint32 GetFromHash(wat::kh_indexname_t* hash, const WatToken& t);
    int MergeFunctionType(const FunctionType& ftype, varuint32& out);
    int ParseTypeUse(Queue<WatToken>& tokens, varuint32& sig, DebugInfo** info, varuint32* n_info, bool anonymous);
    varuint32 GetLocal(FunctionBody& f, FunctionDesc& desc, varuint32 n_param, const WatToken& t);
    int ParseConstantOperator(Queue<WatToken>& tokens, Instruction& op);
    int ParseOperator(Queue<WatToken>& tokens, Instruction& op, FunctionBody& f, FunctionDesc& desc, FunctionType& sig,
                      DeferWatAction& defer);
    void ParseLabel(Queue<WatToken>& tokens);
    bool CheckLabel(Queue<WatToken>& tokens);
    int ParseInstruction(Queue<WatToken>& tokens, FunctionBody& f, FunctionDesc& desc, FunctionType& sig, varuint32 index);
    int ParseExpression(Queue<WatToken>& tokens, FunctionBody& f, FunctionDesc& desc, FunctionType& sig, varuint32 index);
    int ParseFunction(Queue<WatToken>& tokens, varuint32* index, utility::StringSpan name);
    int ParseResizableLimits(ResizableLimits& limits, Queue<WatToken>& tokens);
    int ParseTableDesc(TableDesc& t, Queue<WatToken>& tokens);
    int ParseTable(Queue<WatToken>& tokens, varuint32* index);
    int ParseInitializerInstruction(Queue<WatToken>& tokens, Instruction& op, bool expr);
    static int ParseGlobalDesc(GlobalDesc& g, Queue<WatToken>& tokens);
    int ParseGlobal(Queue<WatToken>& tokens, varuint32* index);
    int ParseMemoryDesc(MemoryDesc& m, Queue<WatToken>& tokens);
    int ParseMemory(Queue<WatToken>& tokens, varuint32* index);
    int ParseImport(Queue<WatToken>& tokens);
    int ParseExport(Queue<WatToken>& tokens);
    int ParseElemData(Queue<WatToken>& tokens, varuint32& index, Instruction& op, wat::kh_indexname_t* hash);
    int ParseElem(TableInit& e, Queue<WatToken>& tokens);
    int ParseData(Queue<WatToken>& tokens);
    int AppendImport(Module& m, const Import& i, varuint32* index);
    int InlineImportExport(const Environment& env, Module& m, Queue<WatToken>& tokens, varuint32* index, varuint7 kind,
                           Import** out);

    static int ParseBlockType(Queue<WatToken>& tokens, varsint7& out);
    static int ParseModule(Environment& env, Module& m, const char* file, Queue<WatToken>& tokens, utility::StringSpan name,
                           WatToken& internalname);
    static int ParseName(const Environment& env, ByteArray& name, const WatToken& t);
    static int AddWatValType(const Environment& env, WatTokens id, varsint7*& a, varuint32& n);
    static int WatString(const Environment& env, ByteArray& str, utility::StringSpan t);
    static void WriteUTF32(uint32_t ch, ByteArray& str, varuint32& index);
    static varsint7 WatValType(WatTokens id);
    static int ParseLocalAppend(const Environment& env, FunctionLocal& local, FunctionBody& body, Queue<WatToken>& tokens);
    static int AddName(wat::kh_indexname_t* h, WatToken t, varuint32 index);

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

    template<int (WatParser::*F)(Queue<WatToken>&, varuint32*)>
    inline int ParseIndexProcess(Queue<WatToken>& tokens, wat::kh_indexname_t* hash)
    {
      WatToken t = GetWatNameToken(tokens);

      int err;
      varuint32 index = (varuint32)~0;
      if(err = (this->*F)(tokens, &index))
        return err;
      assert(index != (varuint32)~0);

      return AddName(hash, t, index);
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

#define EXPECTED(t, e, err)                  \
  if((t).Size() == 0 || (t).Pop().id != (e)) \
  return (err)

  int ParseWatModule(Environment& env, const char* file, Module& m, const uint8_t* data, size_t sz,
                     utility::StringSpan name);
  void WatSkipSection(Queue<WatToken>& tokens, ptrdiff_t count = 1);
  size_t WatLineNumber(const char* start, const char* pos);
}

#endif