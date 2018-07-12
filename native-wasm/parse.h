// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in native-wasm.h

#ifndef __PARSE_H__NW__
#define __PARSE_H__NW__

#include "util.h"

ERROR_CODE ParseFuncSig(Stream& s, FunctionSig& sig);
ERROR_CODE ParseResizableLimits(Stream& s, ResizableLimits& limits);
ERROR_CODE ParseMemoryDesc(Stream& s, MemoryDesc& mem);
ERROR_CODE ParseTableDesc(Stream& s, TableDesc& table);
ERROR_CODE ParseGlobalDesc(Stream& s, GlobalDesc& global);
ERROR_CODE ParseImport(Stream& s, Import& imp);
ERROR_CODE ParseExport(Stream& s, Export& exp);
ERROR_CODE ParseInstruction(Stream& s, Instruction& instruction);
ERROR_CODE ParseFunctionBody(Stream& s, FunctionBody& body);
ERROR_CODE ParseDataInit(Stream& s, DataInit& data);
ERROR_CODE ParseModule(Stream& s, Module& module, ByteArray name);
ERROR_CODE ParseExportFixup(Module& module);

#endif