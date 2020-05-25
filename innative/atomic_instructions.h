// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__ATOMIC_INSTRUCTIONS_H
#define IN__ATOMIC_INSTRUCTIONS_H

#include "innative/schema.h"

namespace innative::atomic_details {
  constexpr auto OP_START      = OP_i32_atomic_load;
  constexpr auto OP_END        = OP_i64_atomic_rmw32_cmpxchg_u + 1;
  constexpr auto OP_GROUP_SIZE = 7;
  enum class OpGroup
  {
    Load,
    Store,
    Add,
    Sub,
    And,
    Or,
    Xor,
    Xchg,
    CmpXchg,

    INVALID_CAT,
  };

  // Abusing the fact that all of the atomic instructions are grouped in regular intervals of 7 values,
  // we can extract some metadata about which instruction we're looking at with some modulo and division
  constexpr OpGroup GetOpGroup(uint8_t op) { return OpGroup((op - OP_START) / OP_GROUP_SIZE); }
  constexpr int GetOpType(uint8_t op) { return (op - OP_START) % OP_GROUP_SIZE; }

  // This integer constant is actually an 8 byte lookup table, with the values read by index from right to left
  // expressing whether the instruction in the given place in the period is an i64 instruction.
  constexpr uint8_t IsI64(int opType) { return uint8_t(0x01'01'01'00'00'01'00 >> (opType * 8)); }

  // Gets the WASM_TYPE_ENCODING for the given atomic opcode[1] using our sneaky math from above
  constexpr WASM_TYPE_ENCODING GetOpTy(uint8_t op) { return WASM_TYPE_ENCODING(TE_i32 - IsI64(GetOpType(op))); }

  // Returns the one and only valid `align` field value for the instruction's memarg
  constexpr uint8_t GetValidAlignment(uint8_t op)
  {
    switch(op)
    {
    case OP_atomic_notify: return 2;
    case OP_atomic_wait32: return 2;
    case OP_atomic_wait64: return 3;
    case OP_atomic_fence: return 0;
    default: return uint8_t(0x02'01'00'01'00'03'02 >> (GetOpType(op) * 8));
    }
  }
  
  // Whether the atomic instruction is part of the range of periodic instruction values
  constexpr bool IsLSRMWOp(uint8_t op) { return op >= OP_START && op < OP_END; }

  // Gets the number of stack arguments the LSRMW instruction is supposed to take
  constexpr int GetArgCount(uint8_t op)
  {
    switch(GetOpGroup(op))
    {
    case OpGroup::Load: return 1;
    case OpGroup::CmpXchg: return 3;
    default: return 2;
    }
  }

  // Make sure these atomic helpers have the right assumptions :)
  static_assert(GetOpGroup(OP_i32_atomic_load) == OpGroup::Load);
  static_assert(GetOpGroup(OP_i64_atomic_load32_u) == OpGroup::Load);
  static_assert(GetOpGroup(OP_i32_atomic_store) == OpGroup::Store);
  static_assert(GetOpGroup(OP_i32_atomic_rmw_add) == OpGroup::Add);
  static_assert(GetOpGroup(OP_i32_atomic_rmw_sub) == OpGroup::Sub);
  static_assert(GetOpGroup(OP_i32_atomic_rmw_and) == OpGroup::And);
  static_assert(GetOpGroup(OP_i32_atomic_rmw_or) == OpGroup::Or);
  static_assert(GetOpGroup(OP_i32_atomic_rmw_xor) == OpGroup::Xor);
  static_assert(GetOpGroup(OP_i32_atomic_rmw_xchg) == OpGroup::Xchg);
  static_assert(GetOpGroup(OP_i32_atomic_rmw_cmpxchg) == OpGroup::CmpXchg);
  static_assert(GetOpGroup(OP_END) == OpGroup::INVALID_CAT);

  static_assert(GetOpType(OP_END) == 0);
  static_assert(TE_i32 - 1 == TE_i64);

  static_assert(GetOpTy(OP_i32_atomic_load) == TE_i32);
  static_assert(GetOpTy(OP_i64_atomic_load) == TE_i64);
  static_assert(GetOpTy(OP_i32_atomic_load8_u) == TE_i32);
  static_assert(GetOpTy(OP_i32_atomic_load16_u) == TE_i32);
  static_assert(GetOpTy(OP_i64_atomic_load8_u) == TE_i64);
  static_assert(GetOpTy(OP_i64_atomic_load16_u) == TE_i64);
  static_assert(GetOpTy(OP_i64_atomic_load32_u) == TE_i64);

  static_assert(GetValidAlignment(OP_i32_atomic_load) == 2);
  static_assert(GetValidAlignment(OP_i64_atomic_load) == 3);
  static_assert(GetValidAlignment(OP_i32_atomic_load8_u) == 0);
  static_assert(GetValidAlignment(OP_i32_atomic_load16_u) == 1);
  static_assert(GetValidAlignment(OP_i64_atomic_load8_u) == 0);
  static_assert(GetValidAlignment(OP_i64_atomic_load16_u) == 1);
  static_assert(GetValidAlignment(OP_i64_atomic_load32_u) == 2);
}

#endif
