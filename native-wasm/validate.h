// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in native-wasm.h

#ifndef __VALIDATE_H__NW__
#define __VALIDATE_H__NW__

#include "native-wasm/schema.h"

bool ValidateIdentifier(ByteArray& bytes);
bool ValidateValueType(varsint7 type);
void ValidateEnvironment(Environment& env);
bool ValidateSectionOrder(uint32& sections, varuint7 opcode);
bool MatchFunctionSig(FunctionSig& a, FunctionSig& b);
varsint32 EvalInitializerI32(Instruction& ins, Environment& env, Module* m);

#endif