// Copyright (c)2021 Fundament Software
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__CONSTANTS_H
#define IN__CONSTANTS_H

#include "innative/innative.h"
#include "innative/khash.h"
#include "innative/opcodes.h"
#include "innative/schema.h"
#include <ostream>
#include <array>

KHASH_DECLARE(mapenum, int, const char*);

namespace innative {
  enum class LLD_FORMAT : uint8_t
  {
    COFF,
    ELF,
    WASM,
  };

  namespace utility {
    constexpr char IN_GETCPUINFO[]           = "__innative_getcpuinfo";
    constexpr char IN_EXTENSION[]            = ".ir-cache";
    constexpr char IN_ENV_EXTENSION[]        = ".ir-env-cache";
    constexpr char IN_GLUE_STRING[]          = "_WASM_";
    constexpr char IN_MEMORY_MAX_METADATA[]  = "__IN_MEMORY_MAX_METADATA";
    constexpr char IN_LOCAL_INDEX_METADATA[] = "__IN_LOCAL_INDEX";
    constexpr char IN_MEMORY_GROW_METADATA[] = "__IN_MEMORY_GROW_METADATA";
    constexpr char IN_FUNCTION_TRAVERSED[]   = "__IN_FUNCTION_TRAVERSED";
    constexpr char IN_TEMP_PREFIX[]          = "wast_m";
    constexpr char IN_METADATA_PREFIX[]      = "$_innative_module#";
    constexpr char IN_BASE64[]               = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    constexpr char IN_INIT_POSTFIX[]         = "innative_internal_init";
    constexpr char IN_EXIT_POSTFIX[]         = "innative_internal_exit";
    constexpr char IN_GENERIC_POSTFIX[]      = "~|GENERIC|";
    constexpr uint8_t BASE64['z' + 1] = { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
                                          255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
                                          255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 62,  255, 255, 255, 63,
                                          52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  255, 255, 255, 32,  255, 255,
                                          255, 0,   1,   2,   3,   4,   5,   6,   7,   8,   9,   10,  11,  12,  13,  14,
                                          15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  255, 255, 255, 255, 255,
                                          255, 26,  27,  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,
                                          41,  42,  43,  44,  45,  46,  47,  48,  49,  50,  51 };

    static const unsigned int WASM_MAGIC_COOKIE      = INNATIVE_WASM_MAGIC_COOKIE;
    static const unsigned int WASM_MAGIC_VERSION     = INNATIVE_WASM_MAGIC_VERSION;
    static const unsigned int WASM_SECTION_C_LINKAGE = (1 << 31); // magic internal section flag for forcing c linkage
    static const size_t VLQ_CONTINUATION_BIT         = 0b100000;

    struct OP
    {
      OP();
      ~OP();
      const char* Get(int code) const;
      static inline constexpr int ToInt(const uint8_t (&x)[MAX_OPCODE_BYTES])
      {
        static_assert(MAX_OPCODE_BYTES <= 2, "more opcodes than we can handle!");
        return x[0] | static_cast<int>(x[1] << CHAR_BIT);
      }
      static inline constexpr int ToInt(const std::array<uint8_t, MAX_OPCODE_BYTES>& x)
      {
        static_assert(MAX_OPCODE_BYTES <= 2, "more opcodes than we can handle!");
        return x[0] | static_cast<int>(x[1] << CHAR_BIT);
      }
      inline const char* operator[](const uint8_t (&x)[MAX_OPCODE_BYTES]) const { return Get(ToInt(x)); }

      kh_mapenum_s* MAP;
      static constexpr std::array<std::pair<std::array<uint8_t, 2>, const char*>, 261> LIST = {
        // Control flow operators
        std::pair<std::array<uint8_t, 2>, const char*>{ { 0x00, 0x00 }, "unreachable" },
        { { 0x01, 0x00 }, "nop" },
        { { 0x02, 0x00 }, "block" },
        { { 0x03, 0x00 }, "loop" },
        { { 0x04, 0x00 }, "if" },
        { { 0x05, 0x00 }, "else" },
        { { 0x0b, 0x00 }, "end" },
        { { 0x0c, 0x00 }, "br" },
        { { 0x0d, 0x00 }, "br_if" },
        { { 0x0e, 0x00 }, "br_table" },
        { { 0x0f, 0x00 }, "return" },

        // Call operators
        { { 0x10, 0x00 }, "call" },
        { { 0x11, 0x00 }, "call_indirect" },

        // Parametric operators
        { { 0x1a, 0x00 }, "drop" },
        { { 0x1b, 0x00 }, "select" },

        // Variable access
        { { 0x20, 0x00 }, "local.get" },
        { { 0x21, 0x00 }, "local.set" },
        { { 0x22, 0x00 }, "local.tee" },
        { { 0x23, 0x00 }, "global.get" },
        { { 0x24, 0x00 }, "global.set" },

        // Memory-related operator
        { { 0x28, 0x00 }, "i32.load" },
        { { 0x29, 0x00 }, "i64.load" },
        { { 0x2a, 0x00 }, "f32.load" },
        { { 0x2b, 0x00 }, "f64.load" },
        { { 0x2c, 0x00 }, "i32.load8_s" },
        { { 0x2d, 0x00 }, "i32.load8_u" },
        { { 0x2e, 0x00 }, "i32.load16_s" },
        { { 0x2f, 0x00 }, "i32.load16_u" },
        { { 0x30, 0x00 }, "i64.load8_s" },
        { { 0x31, 0x00 }, "i64.load8_u" },
        { { 0x32, 0x00 }, "i64.load16_s" },
        { { 0x33, 0x00 }, "i64.load16_u" },
        { { 0x34, 0x00 }, "i64.load32_s" },
        { { 0x35, 0x00 }, "i64.load32_u" },
        { { 0x36, 0x00 }, "i32.store" },
        { { 0x37, 0x00 }, "i64.store" },
        { { 0x38, 0x00 }, "f32.store" },
        { { 0x39, 0x00 }, "f64.store" },
        { { 0x3a, 0x00 }, "i32.store8" },
        { { 0x3b, 0x00 }, "i32.store16" },
        { { 0x3c, 0x00 }, "i64.store8" },
        { { 0x3d, 0x00 }, "i64.store16" },
        { { 0x3e, 0x00 }, "i64.store32" },
        { { 0x3f, 0x00 }, "memory.size" },
        { { 0x40, 0x00 }, "memory.grow" },

        // Constants
        { { 0x41, 0x00 }, "i32.const" },
        { { 0x42, 0x00 }, "i64.const" },
        { { 0x43, 0x00 }, "f32.const" },
        { { 0x44, 0x00 }, "f64.const" },

        // Comparison operators
        { { 0x45, 0x00 }, "i32.eqz" },
        { { 0x46, 0x00 }, "i32.eq" },
        { { 0x47, 0x00 }, "i32.ne" },
        { { 0x48, 0x00 }, "i32.lt_s" },
        { { 0x49, 0x00 }, "i32.lt_u" },
        { { 0x4a, 0x00 }, "i32.gt_s" },
        { { 0x4b, 0x00 }, "i32.gt_u" },
        { { 0x4c, 0x00 }, "i32.le_s" },
        { { 0x4d, 0x00 }, "i32.le_u" },
        { { 0x4e, 0x00 }, "i32.ge_s" },
        { { 0x4f, 0x00 }, "i32.ge_u" },

        { { 0x50, 0x00 }, "i64.eqz" },
        { { 0x51, 0x00 }, "i64.eq" },
        { { 0x52, 0x00 }, "i64.ne" },
        { { 0x53, 0x00 }, "i64.lt_s" },
        { { 0x54, 0x00 }, "i64.lt_u" },
        { { 0x55, 0x00 }, "i64.gt_s" },
        { { 0x56, 0x00 }, "i64.gt_u" },
        { { 0x57, 0x00 }, "i64.le_s" },
        { { 0x58, 0x00 }, "i64.le_u" },
        { { 0x59, 0x00 }, "i64.ge_s" },
        { { 0x5a, 0x00 }, "i64.ge_u" },

        { { 0x5b, 0x00 }, "f32.eq" },
        { { 0x5c, 0x00 }, "f32.ne" },
        { { 0x5d, 0x00 }, "f32.lt" },
        { { 0x5e, 0x00 }, "f32.gt" },
        { { 0x5f, 0x00 }, "f32.le" },
        { { 0x60, 0x00 }, "f32.ge" },

        { { 0x61, 0x00 }, "f64.eq" },
        { { 0x62, 0x00 }, "f64.ne" },
        { { 0x63, 0x00 }, "f64.lt" },
        { { 0x64, 0x00 }, "f64.gt" },
        { { 0x65, 0x00 }, "f64.le" },
        { { 0x66, 0x00 }, "f64.ge" },

        // Numeric operators
        { { 0x67, 0x00 }, "i32.clz" },
        { { 0x68, 0x00 }, "i32.ctz" },
        { { 0x69, 0x00 }, "i32.popcnt" },
        { { 0x6a, 0x00 }, "i32.add" },
        { { 0x6b, 0x00 }, "i32.sub" },
        { { 0x6c, 0x00 }, "i32.mul" },
        { { 0x6d, 0x00 }, "i32.div_s" },
        { { 0x6e, 0x00 }, "i32.div_u" },
        { { 0x6f, 0x00 }, "i32.rem_s" },
        { { 0x70, 0x00 }, "i32.rem_u" },
        { { 0x71, 0x00 }, "i32.and" },
        { { 0x72, 0x00 }, "i32.or" },
        { { 0x73, 0x00 }, "i32.xor" },
        { { 0x74, 0x00 }, "i32.shl" },
        { { 0x75, 0x00 }, "i32.shr_s" },
        { { 0x76, 0x00 }, "i32.shr_u" },
        { { 0x77, 0x00 }, "i32.rotl" },
        { { 0x78, 0x00 }, "i32.rotr" },

        { { 0x79, 0x00 }, "i64.clz" },
        { { 0x7a, 0x00 }, "i64.ctz" },
        { { 0x7b, 0x00 }, "i64.popcnt" },
        { { 0x7c, 0x00 }, "i64.add" },
        { { 0x7d, 0x00 }, "i64.sub" },
        { { 0x7e, 0x00 }, "i64.mul" },
        { { 0x7f, 0x00 }, "i64.div_s" },
        { { 0x80, 0x00 }, "i64.div_u" },
        { { 0x81, 0x00 }, "i64.rem_s" },
        { { 0x82, 0x00 }, "i64.rem_u" },
        { { 0x83, 0x00 }, "i64.and" },
        { { 0x84, 0x00 }, "i64.or" },
        { { 0x85, 0x00 }, "i64.xor" },
        { { 0x86, 0x00 }, "i64.shl" },
        { { 0x87, 0x00 }, "i64.shr_s" },
        { { 0x88, 0x00 }, "i64.shr_u" },
        { { 0x89, 0x00 }, "i64.rotl" },
        { { 0x8a, 0x00 }, "i64.rotr" },

        { { 0x8b, 0x00 }, "f32.abs" },
        { { 0x8c, 0x00 }, "f32.neg" },
        { { 0x8d, 0x00 }, "f32.ceil" },
        { { 0x8e, 0x00 }, "f32.floor" },
        { { 0x8f, 0x00 }, "f32.trunc" },
        { { 0x90, 0x00 }, "f32.nearest" },
        { { 0x91, 0x00 }, "f32.sqrt" },
        { { 0x92, 0x00 }, "f32.add" },
        { { 0x93, 0x00 }, "f32.sub" },
        { { 0x94, 0x00 }, "f32.mul" },
        { { 0x95, 0x00 }, "f32.div" },
        { { 0x96, 0x00 }, "f32.min" },
        { { 0x97, 0x00 }, "f32.max" },
        { { 0x98, 0x00 }, "f32.copysign" },

        { { 0x99, 0x00 }, "f64.abs" },
        { { 0x9a, 0x00 }, "f64.neg" },
        { { 0x9b, 0x00 }, "f64.ceil" },
        { { 0x9c, 0x00 }, "f64.floor" },
        { { 0x9d, 0x00 }, "f64.trunc" },
        { { 0x9e, 0x00 }, "f64.nearest" },
        { { 0x9f, 0x00 }, "f64.sqrt" },
        { { 0xa0, 0x00 }, "f64.add" },
        { { 0xa1, 0x00 }, "f64.sub" },
        { { 0xa2, 0x00 }, "f64.mul" },
        { { 0xa3, 0x00 }, "f64.div" },
        { { 0xa4, 0x00 }, "f64.min" },
        { { 0xa5, 0x00 }, "f64.max" },
        { { 0xa6, 0x00 }, "f64.copysign" },

        // Conversions
        { { 0xa7, 0x00 }, "i32.wrap_i64" },
        { { 0xa8, 0x00 }, "i32.trunc_f32_s" },
        { { 0xa9, 0x00 }, "i32.trunc_f32_u" },
        { { 0xaa, 0x00 }, "i32.trunc_f64_s" },
        { { 0xab, 0x00 }, "i32.trunc_f64_u" },

        { { 0xac, 0x00 }, "i64.extend_i32_s" },
        { { 0xad, 0x00 }, "i64.extend_i32_u" },
        { { 0xae, 0x00 }, "i64.trunc_f32_s" },
        { { 0xaf, 0x00 }, "i64.trunc_f32_u" },
        { { 0xb0, 0x00 }, "i64.trunc_f64_s" },
        { { 0xb1, 0x00 }, "i64.trunc_f64_u" },

        { { 0xb2, 0x00 }, "f32.convert_i32_s" },
        { { 0xb3, 0x00 }, "f32.convert_i32_u" },
        { { 0xb4, 0x00 }, "f32.convert_i64_s" },
        { { 0xb5, 0x00 }, "f32.convert_i64_u" },
        { { 0xb6, 0x00 }, "f32.demote_f64" },

        { { 0xb7, 0x00 }, "f64.convert_i32_s" },
        { { 0xb8, 0x00 }, "f64.convert_i32_u" },
        { { 0xb9, 0x00 }, "f64.convert_i64_s" },
        { { 0xba, 0x00 }, "f64.convert_i64_u" },
        { { 0xbb, 0x00 }, "f64.promote_f32" },

        // Reinterpretations
        { { 0xbc, 0x00 }, "i32.reinterpret_f32" },
        { { 0xbd, 0x00 }, "i64.reinterpret_f64" },
        { { 0xbe, 0x00 }, "f32.reinterpret_i32" },
        { { 0xbf, 0x00 }, "f64.reinterpret_i64" },

        // Sign extension ops
        { { 0xc0, 0x00 }, "i32.extend8_s" },
        { { 0xc1, 0x00 }, "i32.extend16_s" },
        { { 0xc2, 0x00 }, "i64.extend8_s" },
        { { 0xc3, 0x00 }, "i64.extend16_s" },
        { { 0xc4, 0x00 }, "i64.extend32_s" },

        // Nontrapping Float to Int conversions
        { { 0xfc, 0x00 }, "i32.trunc_sat_f32_s" },
        { { 0xfc, 0x01 }, "i32.trunc_sat_f32_u" },
        { { 0xfc, 0x02 }, "i32.trunc_sat_f64_s" },
        { { 0xfc, 0x03 }, "i32.trunc_sat_f64_u" },
        { { 0xfc, 0x04 }, "i64.trunc_sat_f32_s" },
        { { 0xfc, 0x05 }, "i64.trunc_sat_f32_u" },
        { { 0xfc, 0x06 }, "i64.trunc_sat_f64_s" },
        { { 0xfc, 0x07 }, "i64.trunc_sat_f64_u" },

        // Bulk memory operations
        { { 0xfc, 0x08 }, "memory.init" },
        { { 0xfc, 0x09 }, "data.drop" },
        { { 0xfc, 0x0a }, "memory.copy" },
        { { 0xfc, 0x0b }, "memory.fill" },
        { { 0xfc, 0x0c }, "table.init" },
        { { 0xfc, 0x0d }, "elem.drop" },
        { { 0xfc, 0x0e }, "table.copy" },

        // Atomics
        { { 0xfe, 0x00 }, "memory.atomic.notify" },
        { { 0xfe, 0x01 }, "memory.atomic.wait32" },
        { { 0xfe, 0x02 }, "memory.atomic.wait64" },

        { { 0xfe, 0x03 }, "atomic.fence" },

        { { 0xfe, 0x10 }, "i32.atomic.load" },
        { { 0xfe, 0x11 }, "i64.atomic.load" },
        { { 0xfe, 0x12 }, "i32.atomic.load8_u" },
        { { 0xfe, 0x13 }, "i32.atomic.load16_u" },
        { { 0xfe, 0x14 }, "i64.atomic.load8_u" },
        { { 0xfe, 0x15 }, "i64.atomic.load16_u" },
        { { 0xfe, 0x16 }, "i64.atomic.load32_u" },
        { { 0xfe, 0x17 }, "i32.atomic.store" },
        { { 0xfe, 0x18 }, "i64.atomic.store" },
        { { 0xfe, 0x19 }, "i32.atomic.store8" },
        { { 0xfe, 0x1A }, "i32.atomic.store16" },
        { { 0xfe, 0x1B }, "i64.atomic.store8" },
        { { 0xfe, 0x1C }, "i64.atomic.store16" },
        { { 0xfe, 0x1D }, "i64.atomic.store32" },

        { { 0xfe, 0x1E }, "i32.atomic.rmw.add" },
        { { 0xfe, 0x1F }, "i64.atomic.rmw.add" },
        { { 0xfe, 0x20 }, "i32.atomic.rmw8.add_u" },
        { { 0xfe, 0x21 }, "i32.atomic.rmw16.add_u" },
        { { 0xfe, 0x22 }, "i64.atomic.rmw8.add_u" },
        { { 0xfe, 0x23 }, "i64.atomic.rmw16.add_u" },
        { { 0xfe, 0x24 }, "i64.atomic.rmw32.add_u" },

        { { 0xfe, 0x25 }, "i32.atomic.rmw.sub" },
        { { 0xfe, 0x26 }, "i64.atomic.rmw.sub" },
        { { 0xfe, 0x27 }, "i32.atomic.rmw8.sub_u" },
        { { 0xfe, 0x28 }, "i32.atomic.rmw16.sub_u" },
        { { 0xfe, 0x29 }, "i64.atomic.rmw8.sub_u" },
        { { 0xfe, 0x2A }, "i64.atomic.rmw16.sub_u" },
        { { 0xfe, 0x2B }, "i64.atomic.rmw32.sub_u" },

        { { 0xfe, 0x2C }, "i32.atomic.rmw.and" },
        { { 0xfe, 0x2D }, "i64.atomic.rmw.and" },
        { { 0xfe, 0x2E }, "i32.atomic.rmw8.and_u" },
        { { 0xfe, 0x2F }, "i32.atomic.rmw16.and_u" },
        { { 0xfe, 0x30 }, "i64.atomic.rmw8.and_u" },
        { { 0xfe, 0x31 }, "i64.atomic.rmw16.and_u" },
        { { 0xfe, 0x32 }, "i64.atomic.rmw32.and_u" },

        { { 0xfe, 0x33 }, "i32.atomic.rmw.or" },
        { { 0xfe, 0x34 }, "i64.atomic.rmw.or" },
        { { 0xfe, 0x35 }, "i32.atomic.rmw8.or_u" },
        { { 0xfe, 0x36 }, "i32.atomic.rmw16.or_u" },
        { { 0xfe, 0x37 }, "i64.atomic.rmw8.or_u" },
        { { 0xfe, 0x38 }, "i64.atomic.rmw16.or_u" },
        { { 0xfe, 0x39 }, "i64.atomic.rmw32.or_u" },

        { { 0xfe, 0x3A }, "i32.atomic.rmw.xor" },
        { { 0xfe, 0x3B }, "i64.atomic.rmw.xor" },
        { { 0xfe, 0x3C }, "i32.atomic.rmw8.xor_u" },
        { { 0xfe, 0x3D }, "i32.atomic.rmw16.xor_u" },
        { { 0xfe, 0x3E }, "i64.atomic.rmw8.xor_u" },
        { { 0xfe, 0x3F }, "i64.atomic.rmw16.xor_u" },
        { { 0xfe, 0x40 }, "i64.atomic.rmw32.xor_u" },

        { { 0xfe, 0x41 }, "i32.atomic.rmw.xchg" },
        { { 0xfe, 0x42 }, "i64.atomic.rmw.xchg" },
        { { 0xfe, 0x43 }, "i32.atomic.rmw8.xchg_u" },
        { { 0xfe, 0x44 }, "i32.atomic.rmw16.xchg_u" },
        { { 0xfe, 0x45 }, "i64.atomic.rmw8.xchg_u" },
        { { 0xfe, 0x46 }, "i64.atomic.rmw16.xchg_u" },
        { { 0xfe, 0x47 }, "i64.atomic.rmw32.xchg_u" },

        { { 0xfe, 0x48 }, "i32.atomic.rmw.cmpxchg" },
        { { 0xfe, 0x49 }, "i64.atomic.rmw.cmpxchg" },
        { { 0xfe, 0x4A }, "i32.atomic.rmw8.cmpxchg_u" },
        { { 0xfe, 0x4B }, "i32.atomic.rmw16.cmpxchg_u" },
        { { 0xfe, 0x4C }, "i64.atomic.rmw8.cmpxchg_u" },
        { { 0xfe, 0x4D }, "i64.atomic.rmw16.cmpxchg_u" },
        { { 0xfe, 0x4E }, "i64.atomic.rmw32.cmpxchg_u" },

        // reference-types proposal stubs
        { { 0xd0, 0x00 }, "ref.null" },
        { { 0xd2, 0x00 }, "ref.func" },
      };
      static const OP NAMES;
    };

    extern const kh_mapenum_s* ERR_ENUM_MAP;
    extern const kh_mapenum_s* TYPE_ENCODING_MAP;
    extern const kh_mapenum_s* WAST_ASSERTION_MAP;
    extern const kh_mapenum_s* OPNAME_MAP;
    extern const kh_mapenum_s* ARCH_MAP;
    extern const kh_mapenum_s* ABI_MAP;

    const char* EnumToString(const kh_mapenum_s* h, int i, char* buf, size_t n);
    kh_mapenum_s* GenMapEnum(std::initializer_list<std::pair<int, const char*>> list);
  }
}

#endif
