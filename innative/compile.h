// Copyright (c)2021 Fundament Software
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__COMPILE_H
#define IN__COMPILE_H

#include "innative/schema.h"
#include "utility.h"
#include "llvm.h"
#include "filesys.h"
#include "stack.h"
#include "debug.h"
#include <vector>
#include <string>

namespace innative {
  KHASH_DECLARE(importhash, const char*, llvm::GlobalObject*);

  namespace utility {
    template<typename... Args>
    inline IN_ERROR LogErrorType(const Environment& env, const char* format, IN_ERROR err, const llvm::Type* ty,
                                 Args... args)
    {
      if(env.loglevel < LOG_FATAL)
        return err;

      char buf[32];
      (*env.loghook)(&env, format, utility::EnumToString(utility::ERR_ENUM_MAP, (int)err, buf, sizeof(buf)),
                             args...);

      std::string errMsg;
      llvm::raw_string_ostream ostream{ errMsg };
      ty->print(ostream, true);
      ostream.flush();
      (*env.loghook)(&env, "%s\n", errMsg.c_str());
      return err;
    }
  }

  struct Compiler
  {
    struct BlockResult
    {
      llvm::SmallVector<llvm::Value*, 2> v;
      llvm::BasicBlock* b;
      BlockResult* next;
    };

    struct Block
    {
      llvm::BasicBlock* block;  // Label
      llvm::BasicBlock* ifelse; // Label before if statement
      size_t limit;             // Limit of value stack
      varsint64 sig;            // Block signature
      uint8_t op;               // instruction that pushed this label
      BlockResult* results;     // Holds alternative branch results targeting this block
      llvm::PHINode** phi;      // Holds phi nodes for the this block, if it already exists (for loops)
      size_t n_phi;
      llvm::Value* ifcmp;    // if comparison
      llvm::SmallVector<llvm::Value*, 2> ifstack;
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

    struct DataSegment
    {
      llvm::Constant* data;
      llvm::GlobalVariable* global;
      llvm::GlobalVariable* runtime_len; // because of dumb data.drop :unamused:
    };

    struct ElemSegment
    {
      llvm::Constant* elem;
      llvm::GlobalVariable* global;
      llvm::GlobalVariable* runtime_len; // because of dumb table.drop :unamused:
    };

    enum class FPToIntOp : int
    {
      F32ToI32 = 0,
      F32ToU32 = 1,
      F64ToI32 = 2,
      F64ToU32 = 3,
      F32ToI64 = 4,
      F32ToU64 = 5,
      F64ToI64 = 6,
      F64ToU64 = 7,
    };
    Compiler(Environment& env, Module& m, llvm::LLVMContext& ctx, llvm::Module* mod, llvm::IRBuilder<>& builder,
             llvm::TargetMachine* machine, kh_importhash_t* importhash, const path& objfile);
    Environment& env;
    Module& m;
    llvm::LLVMContext& ctx;
    llvm::Module* mod;
    llvm::IRBuilder<>& builder;
    llvm::TargetMachine* machine;
    kh_importhash_t* importhash;
    path objfile; // If this module has been compiled to a .obj file, stores the path so we can reliably delete it.
    std::unique_ptr<Debugger> debugger;
    Stack<llvm::Value*> values; // Tracks the current value stack
    Stack<Block> control;       // Control flow stack
    std::vector<llvm::AllocaInst*> locals;
    std::vector<llvm::GlobalVariable*> memories;
    std::vector<llvm::GlobalVariable*> tables;
    std::vector<llvm::GlobalVariable*> globals;
    std::vector<DataSegment> data_globals;
    std::vector<ElemSegment> elem_globals;
    std::vector<FunctionSet> functions;
    std::string natvis;
    llvm::GlobalVariable* exported_functions;
    llvm::AllocaInst* memlocal;
    llvm::IntegerType* intptrty;
    llvm::StructType* mempairty;
    llvm::Function* init;
    llvm::Function* exit;
    llvm::Function* start;
    llvm::Function* fn_memgrow;
    llvm::Function* fn_memcpy;
    llvm::Function* fn_memmove;
    llvm::Function* fn_memset;
    llvm::Function* fn_memcmp;
    llvm::Function* atomic_notify;
    llvm::Function* atomic_wait32;
    llvm::Function* atomic_wait64;
    llvm::Function* current;
    llvm::GlobalVariable* instruction_counter;
    char logbuf[32]; // This makes some errors easier to log

    using Func    = llvm::Function;
    using FuncTy  = llvm::FunctionType;
    using llvmTy  = llvm::Type;
    using llvmVal = llvm::Value;
    using CInt    = llvm::ConstantInt;
    using CFloat  = llvm::ConstantFP;
    using BB      = llvm::BasicBlock;

    llvm::StructType* GetTableType(varsint7 element_type);
    llvm::StructType* GetPairType(llvmTy* ty);
    llvm::Value* GetPairPtr(llvm::GlobalVariable* v, int index);
    llvm::LoadInst* LoadPairPtr(llvm::GlobalVariable* v, int index);
    llvm::Constant* GetPairNull(llvm::StructType* ty);
    IN_ERROR InsertConditionalTrap(llvmVal* cond);
    IN_ERROR InsertBoundsCheck(llvm::Type* inputTy, llvmVal* end, std::initializer_list<llvmVal*> offsets,
                               llvmVal* accessSize, llvmVal*& loc);
    llvmTy* GetLLVMType(varsint7 type);
    llvmTy* GetLLVMTypes(varsint7* types, varuint32 count);
    llvmTy* GetLLVMTypeSig(varsint64 sig);
    FuncTy* GetFunctionType(FunctionType& signature);
    Func* PassFunction(Func* fn, llvm::StringRef name, llvm::StringRef canonical,
                       llvm::GlobalValue::LinkageTypes linkage, llvm::CallingConv::ID callconv);
    Func* WrapFunction(Func* fn, llvm::StringRef name, llvm::StringRef canonical,
                       llvm::GlobalValue::LinkageTypes linkage, llvm::CallingConv::ID callconv);
    Func* GenericFunction(Func* fn, llvm::StringRef name, llvm::StringRef canonical,
                          llvm::GlobalValue::LinkageTypes linkage, llvm::CallingConv::ID callconv);
    IN_ERROR PopType(varsint7 ty, llvmVal*& v, bool peek = false);
    IN_ERROR PopSig(varsint64 sig, llvm::SmallVector<llvmVal*, 2>& v, bool peek, bool loop);
    llvmVal* MaskShiftBits(llvmVal* value);
    BB* PushLabel(const char* name, varsint64 sig, uint8_t opcode, Func* fnptr, llvm::DILocalScope* scope, bool discard);
    BB* BindLabel(BB* block);
    IN_ERROR PushResult(BlockResult** root, llvm::SmallVector<llvmVal*, 2>& result, BB* block, const Environment& env);
    IN_ERROR AddBranch(Block& target, bool loop);
    IN_ERROR PopLabel(BB* block, llvm::SmallVector<llvmVal*, 2>& push);
    void PolymorphicStack();
    llvmVal* GetMemSize(llvm::GlobalVariable* target);
    varuint32 GetFirstType(varuint32 type);
    llvmVal* GetMemPointer(llvmVal* base, llvm::PointerType* pointer_type, varuint32 memory, varuint32 offset);
    llvmVal* GetMemPointerRegion(llvmVal* base, llvm::PointerType* pointer_type, llvmVal* byteLength, varuint32 memory,
                                 varuint32 offset);
    llvmVal* LoadInBoundsGEP(llvmVal* Ptr, llvm::ArrayRef<llvmVal*> IdxList, const llvm::Twine& Name = "");

    IN_ERROR InsertTruncTrap(double max, double min, llvm::Type* ty);
    llvm::Value* GetLocal(varuint32 index);
    llvm::GlobalVariable* CreateGlobal(llvmTy* ty, bool isconst, bool external, llvm::StringRef name,
                                       llvm::StringRef canonical, size_t line, llvm::Constant* init);
    uint64_t GetTotalSize(llvmTy* t);
    const Intrinsic* GetIntrinsic(Import& imp);
    void ExportFunction(FunctionSet& fn,
                        Func* (Compiler::*wrapper)(Func* fn, llvm::StringRef name, llvm::StringRef canonical,
                                                   llvm::GlobalValue::LinkageTypes linkage, llvm::CallingConv::ID callconv),
                        llvm::StringRef name, llvm::StringRef canonical);
    void AddMemLocalCaching();

    Func* CompileFunction(FunctionType& signature, const llvm::Twine& name);
    IN_ERROR CompileSelectOp(const llvm::Twine& name, llvm::Instruction* from);
    IN_ERROR CompileIfBlock(varsint64 sig);
    IN_ERROR CompileElseBlock();
    IN_ERROR CompileReturn(varsint64 sig);
    IN_ERROR CompileEndBlock();
    void CompileTrap();
    IN_ERROR CompileBranch(varuint32 depth);
    IN_ERROR CompileIfBranch(varuint32 depth);
    IN_ERROR CompileBranchTable(varuint32 n_table, varuint32* table, varuint32 def);
    IN_ERROR CompileCall(varuint32 index);
    IN_ERROR CompileConstant(Instruction& instruction, llvm::Constant*& constant);
    IN_ERROR CompileIndirectCall(varuint32 index);
    llvmVal* CompileMemSize(llvm::GlobalVariable* target);
    IN_ERROR CompileMemGrow(varuint32 memory, const char* name);
    IN_ERROR CompileMemInit(varuint32 dst_mem, varuint32 src_seg);
    IN_ERROR CompileMemCopy(varuint32 dst_mem, varuint32 src_mem);
    IN_ERROR CompileMemFill(varuint32 mem);
    IN_ERROR CompileTableInit(varuint32 dst_tbl, varuint32 src_elem);
    IN_ERROR CompileTableCopy(varuint32 dst_tbl, varuint32 src_tbl);
    IN_ERROR CompileDataDrop(varuint32 segment);
    IN_ERROR CompileElemDrop(varuint32 segment);
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
    static bool CheckType(varsint7 ty, llvmTy* t);
    static bool CheckSig(varsint64 sig, llvmTy* t, Module& m);
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
      assert(!ty->isPointerTy() || !ty->getPointerElementType()->isStructTy());
      assert(!ty->isStructTy());
      if(ty->isIntegerTy() && ty->getIntegerBitWidth() != 32 && ty->getIntegerBitWidth() != 64 &&
         ty->getIntegerBitWidth() != 1)
        assert(false);
      values.Push((ty->isIntegerTy() && ty->getIntegerBitWidth() == 1) ?
                    builder.CreateIntCast(arg, builder.getInt32Ty(), false) :
                    arg);
      return e;
    }

    // Pushes an array of arguments in reverse order
    inline IN_ERROR PushReturns(llvm::ArrayRef<llvmVal*> args)
    {
      IN_ERROR err = ERR_SUCCESS;
      for(size_t i = args.size(); i-- > 0;)
      {
        if(err = PushReturn(args[i]))
          return err;
      }
      return err;
    }

    // Pushes a single argument, splitting up the argument if it's a struct and the sig indicates multiple arguments
    IN_ERROR PushStructReturn(llvmVal* arg, varsint64 sig);

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
    IN_ERROR CompileLoad(varuint32 memory, varuint32 offset, varuint32 memflags, const char* name, llvmTy* ext, llvmTy* ty);
    template<WASM_TYPE_ENCODING TY>
    IN_ERROR CompileStore(varuint32 memory, varuint32 offset, varuint32 memflags, const char* name, llvm::IntegerType* ext);
    template<WASM_TYPE_ENCODING Ty1, WASM_TYPE_ENCODING Ty2, WASM_TYPE_ENCODING TyR>
    IN_ERROR CompileSRem(const llvm::Twine& name);
    template<WASM_TYPE_ENCODING Ty1, WASM_TYPE_ENCODING Ty2, WASM_TYPE_ENCODING TyR, typename... Args>
    IN_ERROR CompileDiv(bool overflow, llvmVal* (llvm::IRBuilder<>::*op)(llvmVal*, llvmVal*, Args...), Args... args);
    template<WASM_TYPE_ENCODING Ty1, WASM_TYPE_ENCODING Ty2, WASM_TYPE_ENCODING TyR>
    IN_ERROR CompileFloatCmp(llvm::Intrinsic::ID id, const llvm::Twine& name);

    IN_ERROR CompileSignExtendOp(WASM_TYPE_ENCODING argTy, unsigned valueBits, const char* name);
    IN_ERROR CompileFPToInt(FPToIntOp op, bool saturating, const char* name);

    IN_ERROR CompileAtomicInstruction(Instruction& ins);

    IN_ERROR InsertAlignmentTrap(llvmVal* ptr, varuint32 memory, varuint32 memflags);
    IN_ERROR InsertAtomicMemGet(Instruction& ins, llvmVal*& ptr, unsigned& align);

    // F = (unsigned align, llvmVal* ptr) -> IN_ERROR
    template<typename F> IN_ERROR CompileAtomicMemInstruction(Instruction& ins, F&& inner);
    // F = (unsigned align, llvmVal* ptr, llvmVal* arg2) -> IN_ERROR
    template<typename F> IN_ERROR CompileAtomicMemInstruction(Instruction& ins, WASM_TYPE_ENCODING arg2Ty, F&& inner);
    // F = (unsigned align, llvmVal* ptr, llvmVal* arg2, llvmVal* arg3) -> IN_ERROR
    template<typename F>
    IN_ERROR CompileAtomicMemInstruction(Instruction& ins, WASM_TYPE_ENCODING arg2Ty, WASM_TYPE_ENCODING arg3Ty, F&& inner);

    IN_ERROR CompileAtomicNotify(Instruction& ins, const char* name);
    IN_ERROR CompileAtomicWait(Instruction& ins, WASM_TYPE_ENCODING varTy, llvm::Function* wait_func, const char* name);

    IN_ERROR CompileAtomicFence(const char* name);

    IN_ERROR CompileAtomicLoad(Instruction& ins, WASM_TYPE_ENCODING varTy, const char* name);
    IN_ERROR CompileAtomicStore(Instruction& ins, WASM_TYPE_ENCODING varTy, const char* name);

    IN_ERROR CompileAtomicRMW(Instruction& ins, WASM_TYPE_ENCODING varTy, llvm::AtomicRMWInst::BinOp Op, const char* name);
    IN_ERROR CompileAtomicCmpXchg(Instruction& ins, WASM_TYPE_ENCODING varTy, const char* name);

    void _logprefix();
    template<typename... Args>
    inline IN_ERROR _log(const char* format, IN_ERROR err, Args... args)
    {
      _logprefix();
      return utility::LogErrorString(env, format, err, args...);
    }

    template<typename... Args>
    inline IN_ERROR _logtype(const char* format, IN_ERROR err, const llvm::Type* ty, Args... args)
    {
      _logprefix();
      return utility::LogErrorType(env, format, err, ty, args...);
    }
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
