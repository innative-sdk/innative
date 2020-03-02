// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__DEBUG_H
#define IN__DEBUG_H

#include "innative/schema.h"
#include "llvm.h"

namespace innative {
  struct Compiler;

  class Debugger
  {
  public:
    Debugger();
    virtual ~Debugger();
    virtual void FuncDecl(llvm::Function* fn, unsigned int offset, unsigned int line, bool optimized);
    virtual void FuncBody(llvm::Function* fn, size_t indice, FunctionDesc& desc, FunctionBody& body);
    virtual void PostFuncBody(llvm::Function* fn, FunctionBody& body);
    virtual void FuncParam(llvm::Function* fn, size_t indice, FunctionDesc& desc);
    virtual void FuncLocal(llvm::Function* fn, size_t indice, FunctionDesc& desc);
    virtual void DebugIns(llvm::Function* fn, Instruction& i);
    virtual void DebugGlobal(llvm::GlobalVariable* v, llvm::StringRef name, size_t line);
    virtual void DebugSetGlobal(int index);
    virtual void PushBlock(llvm::DILocalScope* scope, const llvm::DebugLoc& loc);
    virtual void PopBlock();
    virtual void Finalize();

    llvm::DIType* CreateDebugType(llvm::Type* t);
    llvm::DISubroutineType* CreateFunctionDebugType(llvm::FunctionType* fn, llvm::CallingConv::ID callconv);
    void FunctionDebugInfo(llvm::Function* fn, llvm::StringRef name, bool optimized, bool definition, bool artificial,
                           llvm::DIFile* file, unsigned int line, unsigned int col, llvm::DISubroutineType* subtype = 0);
    void SetSPLocation(llvm::IRBuilder<>& builder, llvm::DISubprogram* sp);

    static Debugger* Create(Compiler& context);
    static llvm::DIFile::ChecksumKind ComputeChecksum(llvm::StringRef data, llvm::SmallString<32>& Checksum);
    static llvm::DILocation* GetSPLocation(llvm::LLVMContext& context, llvm::DISubprogram* sp);
    static std::string GenFlagString(const Environment& env);
    template<class T> static T Align(T x, T a)
    {
      auto y = x + a - 1;
      return !a ? x : y - y % a;
    }

    llvm::DILocalScope* _curscope;

  protected:
    Debugger(Compiler* compiler, llvm::Module& m, const char* name, const char* filepath, char target);

    Compiler* _compiler;
    llvm::DIBuilder* _dbuilder;
    llvm::DIType* diF32;
    llvm::DIType* diF64;
    llvm::DIType* diI1;
    llvm::DIType* diI8;
    llvm::DIType* diI32;
    llvm::DIType* diI64;
    llvm::DIType* diVoid;
    llvm::DICompileUnit* dcu;
    llvm::DIFile* dunit; // Source WASM or WAT file
    FunctionBody* _curbody;
  };
}

#endif