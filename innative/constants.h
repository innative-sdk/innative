// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__CONSTANTS_H
#define IN__CONSTANTS_H

#include "innative/innative.h"
#include "innative/khash.h"
#include "innative/opcodes.h"
#include <ostream>
#include <array>

#define MAKESTRING2(x) #x
#define MAKESTRING(x) MAKESTRING2(x)

#define IN_VERSION_STRING \
  MAKESTRING(INNATIVE_VERSION_MAJOR) "." MAKESTRING(INNATIVE_VERSION_MINOR) "." MAKESTRING(INNATIVE_VERSION_REVISION)

KHASH_DECLARE(mapenum, int, const char*);

namespace innative {
  namespace utility {
    constexpr char IN_GETCPUINFO[]           = "__innative_getcpuinfo";
    constexpr char IN_EXTENSION[]            = ".ir-cache";
    constexpr char IN_ENV_EXTENSION[]        = ".ir-env-cache";
    constexpr char IN_GLUE_STRING[]          = "_WASM_";
    constexpr char IN_MEMORY_MAX_METADATA[]  = "__IN_MEMORY_MAX_METADATA";
    constexpr char IN_MEMORY_GROW_METADATA[] = "__IN_MEMORY_GROW_METADATA";
    constexpr char IN_FUNCTION_TRAVERSED[]   = "__IN_FUNCTION_TRAVERSED";
    constexpr char IN_TEMP_PREFIX[]          = "wast_m";

    extern const std::array<const char*, OP_CODE_COUNT> OPNAMES;

    static const unsigned int WASM_MAGIC_COOKIE  = 0x6d736100;
    static const unsigned int WASM_MAGIC_VERSION = 0x01;

    extern const kh_mapenum_s* ERR_ENUM_MAP;
    extern const kh_mapenum_s* TYPE_ENCODING_MAP;
    extern const kh_mapenum_s* WAST_ASSERTION_MAP;

    const char* EnumToString(const kh_mapenum_s* h, int i, char* buf, size_t n);
    kh_mapenum_s* GenMapEnum(std::initializer_list<std::pair<int, const char*>> list);
  }
}

#endif