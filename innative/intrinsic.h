// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef __INTRINSIC_H__IN__
#define __INTRINSIC_H__IN__

#include "llvm.h"

namespace innative {
  namespace code {
    IN_ERROR IN_Intrinsic_ToC(struct Context& context, llvm::Value** params, llvm::Value*& out);
    IN_ERROR IN_Intrinsic_FromC(struct Context& context, llvm::Value** params, llvm::Value*& out);
    IN_ERROR IN_Intrinsic_Trap(struct Context& context, llvm::Value** params, llvm::Value*& out);
    IN_ERROR IN_Intrinsic_FuncPtr(struct Context& context, llvm::Value** params, llvm::Value*& out);

    struct Intrinsic
    {
      static const int MAX_PARAMS = 1;

      const char* name;
      IN_ERROR (*fn)(struct Context&, llvm::Value**, llvm::Value*&);
      WASM_TYPE_ENCODING params[MAX_PARAMS];
      int num;
    };

    static Intrinsic intrinsics[] = {
      { "_innative_to_c", &IN_Intrinsic_ToC, {TE_i64}, 1 },
      { "_innative_from_c", &IN_Intrinsic_FromC, {TE_i64}, 1},
      { "_innative_trap", &IN_Intrinsic_Trap, {}, 0 },
      { "_innative_funcptr", &IN_Intrinsic_FuncPtr, {TE_i32}, 1},
    };
  }
}

#endif