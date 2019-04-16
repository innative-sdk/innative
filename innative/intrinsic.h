// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef __INTRINSIC_H__IN__
#define __INTRINSIC_H__IN__

#include "llvm.h"

namespace innative {
  namespace code {
    llvm::Function* IN_Intrinsic_ToC(llvm::Function* f, struct Context& context);
    llvm::Function* IN_Intrinsic_FromC(llvm::Function* f, struct Context& context);
    llvm::Function* IN_Intrinsic_Trap(llvm::Function* f, struct code::Context& context);

    struct Intrinsic
    {
      const char* name;
      llvm::Function* (*gen)(llvm::Function* f, struct Context&);
      llvm::Function* fn;
    };

    static Intrinsic intrinsics[] = {
      Intrinsic{ "_innative_to_c", &IN_Intrinsic_ToC, nullptr },
      Intrinsic{ "_innative_from_c", &IN_Intrinsic_FromC, nullptr },
      Intrinsic{ "_innative_trap", &IN_Intrinsic_Trap, nullptr }
    };
  }
}

#endif