// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__PARSE_H
#define IN__PARSE_H

#include "stream.h"

namespace innative {
  IN_ERROR ParseByteArray(utility::Stream& s, ByteArray& section, bool terminator, const Environment& env);
  IN_ERROR ParseIdentifier(utility::Stream& s, ByteArray& section, const Environment& env);
  IN_ERROR ParseInitializer(utility::Stream& s, Instruction& ins, const Environment& env);
  IN_ERROR ParseFunctionType(utility::Stream& s, FunctionType& ftype, const Environment& env);
  IN_ERROR ParseFunctionDesc(utility::Stream& s, FunctionDesc& desc);
  IN_ERROR ParseResizableLimits(utility::Stream& s, ResizableLimits& limits);
  IN_ERROR ParseMemoryDesc(utility::Stream& s, MemoryDesc& mem);
  IN_ERROR ParseTableDesc(utility::Stream& s, TableDesc& t);
  IN_ERROR ParseGlobalDesc(utility::Stream& s, GlobalDesc& g);
  IN_ERROR ParseGlobalDecl(utility::Stream& s, GlobalDecl& g, const Environment& env);
  IN_ERROR ParseImport(utility::Stream& s, Import& i, const Environment& env);
  IN_ERROR ParseExport(utility::Stream& s, Export& e, const Environment& env);
  IN_ERROR ParseInstruction(utility::Stream& s, Instruction& ins, const Environment& env);
  IN_ERROR ParseTableInit(utility::Stream& s, TableInit& init, Module& m, const Environment& env);
  IN_ERROR ParseFunctionBody(utility::Stream& s, FunctionBody& f, Module& m, const Environment& env);
  IN_ERROR ParseDataInit(utility::Stream& s, DataInit& data, const Environment& env);
  IN_ERROR ParseNameSectionParam(utility::Stream& s, size_t num, Module& m, FunctionDesc& desc,
                                 DebugInfo* (*fn)(utility::Stream&, Module&, FunctionDesc&, varuint32, const Environment&),
                                 const Environment& env);
  IN_ERROR ParseNameSection(utility::Stream& s, size_t end, Module& m, const Environment& env);
  IN_ERROR ParseModule(utility::Stream& s, const char* file, const Environment& env, Module& module, ByteArray name,
                       ValidationError*& errors);
  IN_ERROR ParseExportFixup(Module& module, ValidationError*& errors, const Environment& env);
}

#endif