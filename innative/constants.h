// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__CONSTANTS_H
#define IN__CONSTANTS_H

#include "innative/innative.h"
#include "innative/khash.h"
#include "innative/opcodes.h"
#include <ostream>
#include <array>

#define MAKESTRING2(x) #x
#define MAKESTRING(x)  MAKESTRING2(x)

#define IN_VERSION_STRING \
  MAKESTRING(INNATIVE_VERSION_MAJOR) "." MAKESTRING(INNATIVE_VERSION_MINOR) "." MAKESTRING(INNATIVE_VERSION_REVISION)

KHASH_DECLARE(mapenum, int, const char*);

namespace innative {
  enum class LLD_FORMAT : uint8_t
  {
    COFF,
    ELF,
    WASM,
  };

  enum class ABI : uint8_t
  {
    NONE    = 0,
    Win32   = 62, // Not tracked by LLD
    Win64   = 63, // Not tracked by LLD
    POSIX   = 9,  // Sys V
    Linux   = 3,
    FreeBSD = 9,
    Solaris = 6,
    ARM     = 97,
  };

#ifdef IN_PLATFORM_WIN32
  #ifdef IN_CPU_x86
  static constexpr ABI CURRENT_ABI = ABI::Win32;
  #else
  static constexpr ABI CURRENT_ABI = ABI::Win64;
  #endif
#elif defined(IN_PLATFORM_BSD)
  static constexpr ABI CURRENT_ABI            = ABI::FreeBSD;
#elif defined(IN_PLATFORM_SOLARIS)
  static constexpr ABI CURRENT_ABI   = ABI::Solaris;
#elif defined(IN_PLATFORM_LINUX)
  static constexpr ABI CURRENT_ABI   = ABI::Linux;
#elif defined(IN_PLATFORM_POSIX)
  static constexpr ABI CURRENT_ABI   = ABI::POSIX;
#else
  static constexpr ABI CURRENT_ABI   = ABI::NONE;
#endif

  enum class ARCH : uint16_t
  {
    UNKNOWN = 0,
    x86     = 3,
    amd64   = 62,
    IA64    = 255, // Not tracked by LLD
    ARM64   = 183, // AARCH64
    ARM     = 40,
    MIPS    = 8,
    PPC64   = 21,
    PPC     = 20,
    RISCV   = 243,
  };

#ifdef IN_CPU_x86_64
  static constexpr ARCH CURRENT_ARCH = ARCH::amd64;
#elif defined(IN_CPU_x86)
  static constexpr ARCH CURRENT_ARCH          = ARCH::x86;
#elif defined(IN_CPU_IA_64)
  static constexpr ARCH CURRENT_ARCH = ARCH::IA64;
#elif defined(IN_CPU_ARM64)
  static constexpr ARCH CURRENT_ARCH = ARCH::ARM64;
#elif defined(IN_CPU_ARM)
  static constexpr ARCH CURRENT_ARCH = ARCH::ARM;
#elif defined(IN_CPU_MIPS)
  static constexpr ARCH CURRENT_ARCH = ARCH::MIPS;
#elif defined(IN_CPU_POWERPC64)
  static constexpr ARCH CURRENT_ARCH = ARCH::PPC64;
#elif defined(IN_CPU_POWERPC)
  static constexpr ARCH CURRENT_ARCH = ARCH::PPC;
#else
  static constexpr ARCH CURRENT_ARCH = ARCH::UNKNOWN;
#endif

#ifdef IN_ENDIAN_LITTLE
  static constexpr bool CURRENT_LITTLE_ENDIAN = true;
#else
  static constexpr bool CURRENT_LITTLE_ENDIAN = false;
#endif

#ifdef IN_64BIT
  static constexpr int CURRENT_ARCH_BITS = 64;
#elif defined(IN_32BIT)
  static constexpr int CURRENT_ARCH_BITS      = 32;
#else

#endif
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
    constexpr uint8_t BASE64['z' + 1] = { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
                                          255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
                                          255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 62,  255, 255, 255, 63,
                                          52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  255, 255, 255, 32,  255, 255,
                                          255, 0,   1,   2,   3,   4,   5,   6,   7,   8,   9,   10,  11,  12,  13,  14,
                                          15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  255, 255, 255, 255, 255,
                                          255, 26,  27,  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,
                                          41,  42,  43,  44,  45,  46,  47,  48,  49,  50,  51 };

    static const unsigned int WASM_MAGIC_COOKIE  = INNATIVE_WASM_MAGIC_COOKIE;
    static const unsigned int WASM_MAGIC_VERSION = INNATIVE_WASM_MAGIC_VERSION;
    static const size_t VLQ_CONTINUATION_BIT     = 0b100000;

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
      static constexpr std::array<std::pair<std::array<uint8_t, 2>, const char*>, 179> LIST = {
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

        // Bulk memory operations
        { { 0xfc, 0x08 }, "memory.init" },
        { { 0xfc, 0x09 }, "data.drop" },
        { { 0xfc, 0x0a }, "memory.copy" },
        { { 0xfc, 0x0b }, "memory.fill" },
        { { 0xfc, 0x0c }, "table.init" },
        { { 0xfc, 0x0d }, "elem.drop" },
        { { 0xfc, 0x0e }, "table.copy" },
      };
      static const OP NAMES;
    };

    extern const kh_mapenum_s* ERR_ENUM_MAP;
    extern const kh_mapenum_s* TYPE_ENCODING_MAP;
    extern const kh_mapenum_s* WAST_ASSERTION_MAP;
    extern const kh_mapenum_s* OPNAME_MAP;

    const char* EnumToString(const kh_mapenum_s* h, int i, char* buf, size_t n);
    kh_mapenum_s* GenMapEnum(std::initializer_list<std::pair<int, const char*>> list);
  }
}

#endif