// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef __VALIDATE_H__IR__
#define __VALIDATE_H__IR__

#include "innative/schema.h"

namespace innative {
  namespace validate {
    bool ValidateIdentifier(ByteArray& bytes);
    bool ValidateValueType(varsint7 type);
    void ValidateEnvironment(Environment& env);
    void ValidateModule(Environment& env, Module& m);
    bool ValidateSectionOrder(uint32& sections, varuint7 opcode);
    bool MatchFunctionSig(FunctionSig& a, FunctionSig& b);
    varsint32 EvalInitializerI32(Instruction& ins, Environment& env, Module* m);
    void AppendError(ValidationError*& errors, Module* m, int code, const char* fmt, ...);
  }
}

#endif