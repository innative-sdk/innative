// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef __PARSE_H__IR__
#define __PARSE_H__IR__

#include "util.h"

namespace innative {
  IR_ERROR ParseByteArray(utility::Stream& s, ByteArray& section, bool terminator);
  IR_ERROR ParseIdentifier(utility::Stream& s, ByteArray& section);
  IR_ERROR ParseInitializer(utility::Stream& s, Instruction& ins);
  IR_ERROR ParseFunctionType(utility::Stream& s, FunctionType& ftype);
  IR_ERROR ParseResizableLimits(utility::Stream& s, ResizableLimits& limits);
  IR_ERROR ParseMemoryDesc(utility::Stream& s, MemoryDesc& mem);
  IR_ERROR ParseTableDesc(utility::Stream& s, TableDesc& t);
  IR_ERROR ParseGlobalDesc(utility::Stream& s, GlobalDesc& g);
  IR_ERROR ParseGlobalDecl(utility::Stream& s, GlobalDecl& g);
  IR_ERROR ParseImport(utility::Stream& s, Import& i);
  IR_ERROR ParseExport(utility::Stream& s, Export& e);
  IR_ERROR ParseInstruction(utility::Stream& s, Instruction& ins);
  IR_ERROR ParseTableInit(utility::Stream& s, TableInit& init, Module& m);
  IR_ERROR ParseFunctionBody(utility::Stream& s, FunctionBody& f);
  IR_ERROR ParseDataInit(utility::Stream& s, DataInit& data);
  IR_ERROR ParseNameSectionLocal(utility::Stream& s, size_t num, const char**& target);
  IR_ERROR ParseNameSection(utility::Stream& s, size_t end, Module& m);
  IR_ERROR ParseModule(utility::Stream& s, Module& module, ByteArray name, ValidationError*& errors);
  IR_ERROR ParseExportFixup(Module& module, ValidationError*& errors);
}


#endif