// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef __SERIALIZE_H__IR__
#define __SERIALIZE_H__IR__

#include "wat.h"

namespace innative {
  namespace wat {
    Tokens TypeEncodingToken(varsint7 type_encoding);
    void PushNewNameToken(Queue<Token>& tokens, const char* format, ...);
    void PushLocalName(Queue<Token>& tokens, varuint32 index, const char** names, varuint32 num, char prefix);
    void PushFunctionName(Queue<Token>& tokens, const Module& m, varuint32 index);
    void PushIdentifierToken(Queue<Token>& tokens, const ByteArray& id, Tokens token = TOKEN_STRING);
    void TokenizeInstruction(Queue<Token>& tokens, const Module& m, const Instruction& ins, const FunctionBody* body, const FunctionType* ftype);
    void PushExportToken(Queue<Token>& tokens, const Module& m, WASM_KIND kind, varuint32 index);
    void TokenizeModule(Queue<Token>& tokens, const Module& m);
    void WriteTokens(Queue<Token> tokens, std::ostream& out);
  }
}

#endif