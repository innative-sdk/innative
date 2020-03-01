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

    extern const std::array<const char*, OP_CODE_COUNT> OPNAMES;

    static const unsigned int WASM_MAGIC_COOKIE  = INNATIVE_WASM_MAGIC_COOKIE;
    static const unsigned int WASM_MAGIC_VERSION = INNATIVE_WASM_MAGIC_VERSION;
    static const size_t VLQ_CONTINUATION_BIT     = 0b100000;

    extern const kh_mapenum_s* ERR_ENUM_MAP;
    extern const kh_mapenum_s* TYPE_ENCODING_MAP;
    extern const kh_mapenum_s* WAST_ASSERTION_MAP;

    const char* EnumToString(const kh_mapenum_s* h, int i, char* buf, size_t n);
    kh_mapenum_s* GenMapEnum(std::initializer_list<std::pair<int, const char*>> list);
  }
}

#endif