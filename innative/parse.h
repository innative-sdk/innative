// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef __PARSE_H__IR__
#define __PARSE_H__IR__

#include "stream.h"

namespace innative {
  IR_ERROR ParseByteArray(utility::Stream& s, ByteArray& section, bool terminator, const Environment& env);
  IR_ERROR ParseIdentifier(utility::Stream& s, ByteArray& section, const Environment& env);
  IR_ERROR ParseInitializer(utility::Stream& s, Instruction& ins, const Environment& env);
  IR_ERROR ParseFunctionType(utility::Stream& s, FunctionType& ftype, const Environment& env);
  IR_ERROR ParseResizableLimits(utility::Stream& s, ResizableLimits& limits);
  IR_ERROR ParseMemoryDesc(utility::Stream& s, MemoryDesc& mem);
  IR_ERROR ParseTableDesc(utility::Stream& s, TableDesc& t);
  IR_ERROR ParseGlobalDesc(utility::Stream& s, GlobalDesc& g);
  IR_ERROR ParseGlobalDecl(utility::Stream& s, GlobalDecl& g, const Environment& env);
  IR_ERROR ParseImport(utility::Stream& s, Import& i, const Environment& env);
  IR_ERROR ParseExport(utility::Stream& s, Export& e, const Environment& env);
  IR_ERROR ParseInstruction(utility::Stream& s, Instruction& ins, const Environment& env);
  IR_ERROR ParseTableInit(utility::Stream& s, TableInit& init, Module& m, const Environment& env);
  IR_ERROR ParseFunctionBody(utility::Stream& s, FunctionBody& f, const Environment& env);
  IR_ERROR ParseDataInit(utility::Stream& s, DataInit& data, const Environment& env);
  IR_ERROR ParseNameSectionLocal(utility::Stream& s, size_t num, DebugInfo*& target, const Environment& env);
  IR_ERROR ParseNameSection(utility::Stream& s, size_t end, Module& m, const Environment& env);
  IR_ERROR ParseModule(utility::Stream& s, const Environment& env, Module& module, ByteArray name, ValidationError*& errors);
  IR_ERROR ParseExportFixup(Module& module, ValidationError*& errors, const Environment& env);
}


#endif