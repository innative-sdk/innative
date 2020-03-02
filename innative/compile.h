// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__COMPILE_H
#define IN__COMPILE_H

#include "innative/schema.h"
#include "constants.h"
#include "llvm.h"
#include "filesys.h"
#include "stack.h"
#include "debug.h"
#include <vector>
#include <string>

namespace innative {
  KHASH_DECLARE(importhash, const char*, llvm::GlobalObject*);

  struct Compiler
  {
    struct BlockResult
    {
      llvm::Value* v;
      llvm::BasicBlock* b;
      BlockResult* next;
    };

    struct Block
    {
      llvm::BasicBlock* block;  // Label
      llvm::BasicBlock* ifelse; // Label for else statement
      size_t limit;             // Limit of value stack
      varsint7 sig;             // Block signature
      uint8_t op;               // instruction that pushed this label
      BlockResult* results;     // Holds alternative branch results targeting this block
    };

    struct Intrinsic
    {
      static const int MAX_PARAMS = 1;

      const char* name;
      IN_ERROR (Compiler::*fn)(llvm::Value**, llvm::Value*&);
      WASM_TYPE_ENCODING params[MAX_PARAMS];
      int num;
    };

    struct FunctionSet
    {
      llvm::Function* internal;
      llvm::Function* exported;
      llvm::Function* imported;
      const Intrinsic* intrinsic;
      llvm::AllocaInst* memlocal;
    };

    Environment& env;
    Module& m;
    llvm::LLVMContext& ctx;
    llvm::Module* mod;
    llvm::IRBuilder<>& builder;
    llvm::TargetMachine* machine;
    kh_importhash_t* importhash;
    path objfile; // If this module has been compiled to a .obj file, stores the path so we can reliably delete it.
    llvm::IntegerType* intptrty;
    llvm::StructType* mempairty;
    std::unique_ptr<Debugger> debugger;
    Stack<llvm::Value*> values; // Tracks the current value stack
    Stack<Block> control;       // Control flow stack
    std::vector<llvm::AllocaInst*> locals;
    llvm::AllocaInst* memlocal;
    std::vector<llvm::GlobalVariable*> memories;
    std::vector<llvm::GlobalVariable*> tables;
    std::vector<llvm::GlobalVariable*> globals;
    llvm::GlobalVariable* exported_functions;
    std::vector<FunctionSet> functions;
    llvm::Function* init;
    llvm::Function* exit;
    llvm::Function* start;
    llvm::Function* memgrow;
    std::string natvis;

    using Func    = llvm::Function;
    using FuncTy  = llvm::FunctionType;
    using llvmTy  = llvm::Type;
    using llvmVal = llvm::Value;
    using CInt    = llvm::ConstantInt;
    using BB      = llvm::BasicBlock;

    llvm::StructType* GetTableType(varsint7 element_type);
    llvm::StructType* GetPairType(llvmTy* ty);
    llvm::Value* GetPairPtr(llvm::GlobalVariable* v, int index);
    llvm::Constant* GetPairNull(llvm::StructType* ty);
    IN_ERROR InsertConditionalTrap(llvmVal* cond);
    llvmTy* GetLLVMType(varsint7 type);
    FuncTy* GetFunctionType(FunctionType& signature);
    Func* HomogenizeFunction(Func* fn, llvm::StringRef name, const llvm::Twine& canonical,
                             llvm::GlobalValue::LinkageTypes linkage, llvm::CallingConv::ID callconv);
    Func* PassFunction(Func* fn, llvm::StringRef name, const llvm::Twine& canonical,
                       llvm::GlobalValue::LinkageTypes linkage, llvm::CallingConv::ID callconv);
    Func* WrapFunction(Func* fn, llvm::StringRef name, const llvm::Twine& canonical,
                       llvm::GlobalValue::LinkageTypes linkage, llvm::CallingConv::ID callconv);
    IN_ERROR PopType(varsint7 ty, llvmVal*& v, bool peek = false);
    llvmVal* MaskShiftBits(llvmVal* value);
    BB* PushLabel(const char* name, varsint7 sig, uint8_t opcode, Func* fnptr, llvm::DILocalScope* scope);
    BB* BindLabel(BB* block);
    IN_ERROR PushResult(BlockResult** root, llvmVal* result, BB* block, const Environment& env);
    IN_ERROR AddBranch(Block& target);
    IN_ERROR PopLabel(BB* block);
    void PolymorphicStack();
    llvmVal* GetMemSize(llvm::GlobalVariable* target);
    varuint32 GetFirstType(varuint32 type);
    llvmVal* GetMemPointer(llvmVal* base, llvm::PointerType* pointer_type, varuint7 memory, varuint32 offset);
    IN_ERROR InsertTruncTrap(double max, double min, llvm::Type* ty);
    llvm::Value* GetLocal(varuint32 index);
    llvm::GlobalVariable* CreateGlobal(llvmTy* ty, bool isconst, bool external, llvm::StringRef name,
                                       const llvm::Twine& canonical, size_t line, llvm::Constant* init);
    uint64_t GetTotalSize(llvmTy* t);
    const Intrinsic* GetIntrinsic(Import& imp);
    void ExportFunction(FunctionSet& fn,
                        Func* (Compiler::*wrapper)(Func* fn, llvm::StringRef name, const llvm::Twine& canonical,
                                                   llvm::GlobalValue::LinkageTypes linkage, llvm::CallingConv::ID callconv),
                        llvm::StringRef name, const llvm::Twine& canonical);
    void AddMemLocalCaching();

    Func* CompileFunction(FunctionType& signature, const llvm::Twine& name);
    IN_ERROR CompileSelectOp(const llvm::Twine& name, llvm::Instruction* from);
    IN_ERROR CompileIfBlock(varsint7 sig);
    IN_ERROR CompileElseBlock();
    IN_ERROR CompileReturn(varsint7 sig);
    IN_ERROR CompileEndBlock();
    void CompileTrap();
    IN_ERROR CompileBranch(varuint32 depth);
    IN_ERROR CompileIfBranch(varuint32 depth);
    IN_ERROR CompileBranchTable(varuint32 n_table, varuint32* table, varuint32 def);
    IN_ERROR CompileCall(varuint32 index);
    IN_ERROR CompileConstant(Instruction& instruction, llvm::Constant*& constant);
    IN_ERROR CompileIndirectCall(varuint32 index);
    llvmVal* CompileMemSize(llvm::GlobalVariable* target);
    IN_ERROR CompileMemGrow(const char* name);
    void DumpCompilerState();
    IN_ERROR CompileInstruction(Instruction& ins);
    IN_ERROR CompileFunctionBody(Func* fn, size_t indice, llvm::AllocaInst*& memlocal, FunctionDesc& desc,
                                 FunctionBody& body);
    IN_ERROR CompileInitGlobal(Module& m, varuint32 index, llvm::Constant*& out);
    IN_ERROR CompileInitConstant(Instruction& instruction, Module& m, llvm::Constant*& out);
    IN_ERROR CompileModule(varuint32 m_idx);

    IN_ERROR IN_Intrinsic_ToC(llvm::Value** params, llvm::Value*& out);
    IN_ERROR IN_Intrinsic_FromC(llvm::Value** params, llvm::Value*& out);
    IN_ERROR IN_Intrinsic_Trap(llvm::Value** params, llvm::Value*& out);
    IN_ERROR IN_Intrinsic_FuncPtr(llvm::Value** params, llvm::Value*& out);

    static WASM_TYPE_ENCODING GetTypeEncoding(llvm::Type* t);
    static bool CheckSig(varsint7 sig, const Stack<llvmVal*>& values);
    static bool CheckType(varsint7 ty, llvmVal* v);
    static uint64_t GetLocalOffset(llvm::AllocaInst* p);
    static Func* TopLevelFunction(llvm::LLVMContext& context, llvm::IRBuilder<>& builder, const char* name,
                                  llvm::Module* m);
    static void PostOrderTraversal(llvm::Function* f);
    static void ResolveModuleExports(const Environment* env, Module* root, llvm::LLVMContext& context);

    // In order to directly call external functions we default to C
    // static const llvm::CallingConv::ID InternalConvention = llvm::CallingConv::Fast;
    static const llvm::CallingConv::ID InternalConvention = llvm::CallingConv::C;
    static const struct Intrinsic intrinsics[4];

    inline static std::string CppString(const char* str, size_t len)
    {
      std::string s(str, len);
      for(size_t i = 0; i < s.size(); ++i)
        if(!isalnum(s[i]))
          s[i] = '_';
      return s;
    }
    inline static std::string CppString(const char* str) { return CppString(str, strlen(str)); }

    inline static ptrdiff_t PrintReplacement(bool xml, char* out, size_t n, const char* str, size_t len)
    {
      if(!str)
        return 0;
      if(!xml)
        return !len ? snprintf(out, n, "%s", str) : snprintf(out, n, "%.*s", static_cast<unsigned int>(len), str);
      if(!len)
        len = strlen(str);

      char* start = out;
      for(size_t i = 0; i < len; ++i)
      {
        int diff = 1;
        switch(str[i])
        {
        case '<': diff = snprintf(out, n, "&lt;"); break;
        case '>': diff = snprintf(out, n, "&gt;"); break;
        case '\'': diff = snprintf(out, n, "&apos;"); break;
        case '"': diff = snprintf(out, n, "&quot;"); break;
        case '&': diff = snprintf(out, n, "&amp;"); break;
        default:
          if(n > 0)
            *out = str[i];
          break;
        }
        n = (static_cast<ptrdiff_t>(n) > diff) ? n - diff : 0;
        out += diff;
      }
      return out - start;
    }

    template<int N> IN_FORCEINLINE static ptrdiff_t FormatArg(bool xml, char* out, ptrdiff_t n, int i)
    {
      return snprintf(out, n, "{INVALID PARAMETER %i}", i);
    }

    template<int N, typename Arg, typename... Args>
    IN_FORCEINLINE static ptrdiff_t FormatArg(bool xml, char* out, size_t n, int i, Arg&& arg, Args&&... args)
    {
      if(sizeof...(Args) == (N - i))
      {
        if constexpr(std::is_base_of<llvm::StringRef, std::decay_t<Arg>>::value ||
                     std::is_base_of<std::string, std::decay_t<Arg>>::value)
          return PrintReplacement(xml, out, n, arg.data(), arg.size());
        else if constexpr(std::is_integral<std::decay_t<Arg>>::value)
        {
          char format[5] = { '%' };
          switch(sizeof(Arg))
          {
          case 1: format[2] = 'h';
          case 2: format[1] = 'h'; break;
          case 8:
            format[1] = 'l';
            format[2] = 'l';
            break;
          }
          STRCATx0(format, std::is_unsigned<std::decay_t<Arg>>::value ? "u" : "i");
          return snprintf(out, n, format, std::forward<Arg>(arg));
        }
        else if constexpr(std::is_floating_point<Arg>::value)
          return snprintf(out, n, "%f", std::forward<Arg>(arg));
        else if constexpr(std::is_same<std::decay_t<Arg>, const char*>::value)
          return PrintReplacement(xml, out, n, std::forward<Arg>(arg), 0);
        else
          return snprintf(out, n, "{INVALID TYPE %i}", i);
      }
      return FormatArg<N, Args...>(xml, out, n, i, std::forward<Args>(args)...);
    }

    template<typename... Args>
    inline static size_t Format(bool xml, char* out, size_t n, const char* format, Args&&... args)
    {
      size_t count = 0;
      while(format[0])
      {
        if(format[0] == '{' && format[1] >= '0' && format[1] <= '9' &&
           (format[2] == '}' || (format[2] >= '0' && format[2] <= '9' && format[3] == '}')))
        {
          int i = 0;
          if(format[2] == '}')
          {
            i = format[1] - '0';
            format += 3;
          }
          else
          {
            i = format[2] - '0';
            i += (format[1] - '0') * 10;
            format += 4;
          }
          count += FormatArg<sizeof...(Args) - 1, Args...>(xml, out + count, (n <= count ? 0 : n - count), i,
                                                           std::forward<Args>(args)...);
        }
        else
        {
          if(count < n)
            out[count] = format[0];
          ++count;
          ++format;
        }
      }
      return count;
    }

    inline IN_ERROR PushReturn() { return ERR_SUCCESS; }

    // Given a set of returns in the order given in the function/instruction signature, pushes them on to the stack in
    // reverse order
    template<typename Arg, typename... Args> inline IN_ERROR PushReturn(Arg arg, Args... args)
    {
      IN_ERROR e = PushReturn(args...);
      auto ty    = static_cast<llvmVal*>(arg)->getType();
      if(ty->isIntegerTy() && ty->getIntegerBitWidth() != 32 && ty->getIntegerBitWidth() != 64 &&
         ty->getIntegerBitWidth() != 1)
        assert(false);
      values.Push((ty->isIntegerTy() && ty->getIntegerBitWidth() == 1) ?
                    builder.CreateIntCast(arg, builder.getInt32Ty(), false) :
                    arg);
      return e;
    }

  private:
    template<WASM_TYPE_ENCODING Ty1, WASM_TYPE_ENCODING Ty2, WASM_TYPE_ENCODING TyR, typename... Args>
    IN_ERROR CompileBinaryOp(llvmVal* (llvm::IRBuilder<>::*op)(llvmVal*, llvmVal*, Args...), Args... args);
    template<WASM_TYPE_ENCODING Ty1, WASM_TYPE_ENCODING Ty2, WASM_TYPE_ENCODING TyR>
    IN_ERROR CompileBinaryIntrinsic(llvm::Intrinsic::ID id, const llvm::Twine& name);
    template<WASM_TYPE_ENCODING Ty1, WASM_TYPE_ENCODING Ty2, WASM_TYPE_ENCODING TyR, typename... Args>
    IN_ERROR CompileBinaryShiftOp(llvmVal* (llvm::IRBuilder<>::*op)(llvmVal*, llvmVal*, Args...), Args... args);
    template<WASM_TYPE_ENCODING Ty1, WASM_TYPE_ENCODING TyR, typename... Args>
    IN_ERROR CompileUnaryOp(llvmVal* (llvm::IRBuilder<>::*op)(llvmVal*, Args...), Args... args);
    template<WASM_TYPE_ENCODING Ty1, WASM_TYPE_ENCODING TyR, typename... Args>
    IN_ERROR CompileUnaryIntrinsic(llvm::Intrinsic::ID id, const llvm::Twine& name, Args... args);
    template<WASM_TYPE_ENCODING TYPE, bool LEFT> IN_ERROR CompileRotationOp(const char* name);
    template<bool SIGNED>
    IN_ERROR CompileLoad(varuint7 memory, varuint32 offset, varuint32 memflags, const char* name, llvmTy* ext, llvmTy* ty);
    template<WASM_TYPE_ENCODING TY>
    IN_ERROR CompileStore(varuint7 memory, varuint32 offset, varuint32 memflags, const char* name, llvm::IntegerType* ext);
    template<WASM_TYPE_ENCODING Ty1, WASM_TYPE_ENCODING Ty2, WASM_TYPE_ENCODING TyR>
    IN_ERROR CompileSRem(const llvm::Twine& name);
    template<WASM_TYPE_ENCODING Ty1, WASM_TYPE_ENCODING Ty2, WASM_TYPE_ENCODING TyR, typename... Args>
    IN_ERROR CompileDiv(bool overflow, llvmVal* (llvm::IRBuilder<>::*op)(llvmVal*, llvmVal*, Args...), Args... args);
    template<WASM_TYPE_ENCODING Ty1, WASM_TYPE_ENCODING Ty2, WASM_TYPE_ENCODING TyR>
    IN_ERROR CompileFloatCmp(llvm::Intrinsic::ID id, const llvm::Twine& name);
  };

  template<typename... Args> IN_FORCEINLINE std::string FormatString(bool xml, const char* format, Args&&... args)
  {
    std::string out;
    out.resize(Compiler::Format<Args...>(xml, 0, 0, format, std::forward<Args>(args)...) + 1);
    out.resize(Compiler::Format<Args...>(xml, out.data(), out.size(), format, std::forward<Args>(args)...));
    return out;
  }
}

#endif
