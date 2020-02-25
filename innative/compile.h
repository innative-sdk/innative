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
  namespace code {
    struct Intrinsic;

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

    struct FunctionSet
    {
      llvm::Function* internal;
      llvm::Function* exported;
      llvm::Function* imported;
      Intrinsic* intrinsic;
      llvm::AllocaInst* memlocal;
    };

    KHASH_DECLARE(importhash, const char*, llvm::GlobalObject*);

    struct Context
    {
      Environment& env;
      Module& m;
      llvm::LLVMContext& context;
      llvm::Module* llvm;
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

      llvm::StructType* GetTableType(varsint7 element_type);
      llvm::StructType* GetPairType(llvm::Type* ty);
      llvm::Value* GetPairPtr(llvm::GlobalVariable* v, int index);
      llvm::Constant* GetPairNull(llvm::StructType* ty);

      inline static std::string CppString(const char* str, size_t len)
      {
        std::string s(str, len);
        for(size_t i = 0; i < s.size(); ++i)
          if(!isalnum(s[i]))
            s[i] = '_';
        return s;
      }
      inline static std::string CppString(const char* str) { return CppString(str, strlen(str)); }
    };

    IN_ERROR InsertConditionalTrap(llvm::Value* cond, Context& context);

    inline ptrdiff_t PrintReplacement(bool xml, char* out, size_t n, const char* str, size_t len)
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
        n = (n > diff) ? n - diff : 0;
        out += diff;
      }
      return out - start;
    }

    template<int N> IN_FORCEINLINE static ptrdiff_t FormatArg(bool xml, char* out, ptrdiff_t n, int i)
    {
      return snprintf(out, n, "{INVALID PARAMETER %i}", i);
    }

    template<int N, typename Arg, typename... Args>
    IN_FORCEINLINE ptrdiff_t FormatArg(bool xml, char* out, size_t n, int i, Arg&& arg, Args&&... args)
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

    template<typename... Args> inline size_t Format(bool xml, char* out, size_t n, const char* format, Args&&... args)
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

    template<typename... Args> inline std::string FormatString(bool xml, const char* format, Args&&... args)
    {
      std::string out;
      out.resize(Format<Args...>(xml, 0, 0, format, std::forward<Args>(args)...) + 1);
      out.resize(Format<Args...>(xml, out.data(), out.size(), format, std::forward<Args>(args)...));
      return out;
    }
  }
}

#endif
