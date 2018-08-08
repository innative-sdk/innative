// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef __PARSE_H__IR__
#define __PARSE_H__IR__

#include "util.h"

namespace innative {
  namespace parse {
    IR_ERROR ParseFuncSig(Stream& s, FunctionSig& sig);
    IR_ERROR ParseResizableLimits(Stream& s, ResizableLimits& limits);
    IR_ERROR ParseMemoryDesc(Stream& s, MemoryDesc& mem);
    IR_ERROR ParseTableDesc(Stream& s, TableDesc& table);
    IR_ERROR ParseGlobalDesc(Stream& s, GlobalDesc& global);
    IR_ERROR ParseImport(Stream& s, Import& imp);
    IR_ERROR ParseExport(Stream& s, Export& exp);
    IR_ERROR ParseInstruction(Stream& s, Instruction& instruction);
    IR_ERROR ParseFunctionBody(Stream& s, FunctionBody& body);
    IR_ERROR ParseDataInit(Stream& s, DataInit& data);
    IR_ERROR ParseModule(Stream& s, Module& module, ByteArray name);
    IR_ERROR ParseExportFixup(Module& module);
  }
}

#endif