// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__SERIALIZE_H
#define IN__SERIALIZE_H

#include "wat.h"

namespace innative {
  class Serializer
  {
  public:
    Serializer(const Environment& env, const Module& m);
    static WatTokens TypeEncodingToken(varsint7 type_encoding);
    void PushNewNameToken(const char* format, ...);
    void PushParamName(varuint32 index, const DebugInfo* names, varuint32 num, char prefix);
    void PushLocalName(varuint32 index, const FunctionBody* body);
    void PushFunctionName(varuint32 index);
    void PushIdentifierToken(const ByteArray& id, WatTokens token = WatTokens::STRING);
    void TokenizeInstruction(const Instruction& ins, const FunctionBody* body, const FunctionDesc* desc, size_t& block,
                             bool emitdebug);
    void PushExportToken(varuint7 kind, varuint32 index, bool outside);
    void TokenizeModule(bool emitdebug);
    void WriteTokens(std::ostream& out);
    void PushBlockToken(int index);
    void PushGlobalName(varuint32 index);

    Queue<WatToken> tokens;
    Stack<WatToken> blocktokens;
    const Environment& env;
    const Module& m;
  };
}

#endif