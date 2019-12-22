// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__SERIALIZE_H
#define IN__SERIALIZE_H

#include "wat.h"

namespace innative {
  namespace wat {
    WatTokens TypeEncodingToken(varsint7 type_encoding);
    void PushNewNameToken(const Environment& env, Queue<WatToken>& tokens, const char* format, ...);
    void PushParamName(const Environment& env, Queue<WatToken>& tokens, varuint32 index, const DebugInfo* names,
                       varuint32 num, char prefix);
    void PushLocalName(const Environment& env, Queue<WatToken>& tokens, varuint32 index, const FunctionBody* body);
    void PushFunctionName(const Environment& env, Queue<WatToken>& tokens, const Module& m, varuint32 index);
    void PushIdentifierToken(Queue<WatToken>& tokens, const ByteArray& id, WatTokens token = WatTokens::STRING);
    void TokenizeInstruction(const Environment& env, Queue<WatToken>& tokens, const Module& m, const Instruction& ins,
                             const FunctionBody* body, const FunctionDesc* desc);
    void PushExportToken(Queue<WatToken>& tokens, const Module& m, varuint7 kind, varuint32 index, bool outside);
    void TokenizeModule(const Environment& env, Queue<WatToken>& tokens, const Module& m);
    void WriteTokens(Queue<WatToken> tokens, std::ostream& out);
  }
}

#endif