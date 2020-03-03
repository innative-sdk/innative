// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__SERIALIZE_H
#define IN__SERIALIZE_H

#include "wat.h"

namespace innative {
  class Serializer
  {
  public:
    Serializer(const Environment& env, Module& m, std::ostream* out);
    static WatTokens TypeEncodingToken(varsint7 type_encoding);
    void PushNewNameToken(const char* format, ...);
    void PushParamName(varuint32 index, const DebugInfo* names, varuint32 num, char prefix);
    void PushLocalName(varuint32 index, const FunctionBody* body);
    void PushFunctionName(varuint32 index);
    void PushIdentifierToken(const ByteArray& id, WatTokens token = WatTokens::STRING);
    void TokenizeInstruction(Instruction& ins, const FunctionBody* body, const FunctionDesc* desc, size_t& block,
                             bool emitdebug);
    void PushExportToken(varuint7 kind, varuint32 index, bool outside);
    void TokenizeModule(bool emitdebug);
    void WriteTokens(std::ostream& out);
    void PushBlockToken(int index);
    void PushGlobalName(varuint32 index);
    void DumpTokens(unsigned int& line, unsigned int& column);

    Queue<WatToken> tokens;
    Stack<WatToken> blocktokens;
    const Environment& env;
    Module& m;
    std::ostream* _dump;
    size_t _line;
    std::streampos _lastp;
    size_t _depth;
    size_t _stack;
    bool _localbreak;
  };
}

#endif