// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__VALIDATE_H
#define IN__VALIDATE_H

#include "innative/schema.h"

namespace innative {
  bool ValidateIdentifier(const ByteArray& bytes);
  void AppendError(const Environment& env, ValidationError*& errors, Module* m, int code, const char* fmt, ...);
  void ValidateFunctionSig(const FunctionType& sig, Environment& env, Module* m);
  bool MatchFunctionType(const FunctionType& a, const FunctionType& b);
  void ValidateImport(const Import& imp, Environment& env, Module* m);
  void ValidateFunction(const FunctionDesc& decl, Environment& env, Module* m);
  void ValidateLimits(const ResizableLimits& limits, Environment& env, Module* m);
  void ValidateTable(const TableDesc& table, Environment& env, Module* m);
  void ValidateMemory(const MemoryDesc& mem, Environment& env, Module* m);
  void ValidateBlockSignature(const Instruction& ins, varsint7 sig, Environment& env, Module* m);
  varsint7 ValidateInitializer(const Instruction& ins, Environment& env, Module* m);
  void ValidateGlobal(const GlobalDecl& decl, Environment& env, Module* m);
  void ValidateExport(const Export& e, Environment& env, Module* m);
  varsint32 EvalInitializerI32(const Instruction& ins, Environment& env, Module* m);
  void ValidateTableOffset(const TableInit& init, Environment& env, Module* m);
  void ValidateFunctionBody(const FunctionType& sig, const FunctionBody& body, Environment& env, Module* m);
  void ValidateDataOffset(const DataInit& init, Environment& env, Module* m);
  void ValidateImportOrder(Module& m);
  void ValidateModule(Environment& env, Module& m);
  void ValidateEnvironment(Environment& env);
  bool ValidateSectionOrder(const uint32& sections, varuint7 opcode);
}

#endif