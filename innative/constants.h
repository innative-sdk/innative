// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef __CONSTANTS_H__IN__
#define __CONSTANTS_H__IN__

#include "innative/innative.h"
#include "innative/khash.h"
#include "innative/opcodes.h"
#include <ostream>
#include <array>

#define MAKESTRING2(x) #x
#define MAKESTRING(x) MAKESTRING2(x)

#define IN_VERSION_STRING MAKESTRING(INNATIVE_VERSION_MAJOR) "." MAKESTRING(INNATIVE_VERSION_MINOR) "." MAKESTRING(INNATIVE_VERSION_REVISION)

KHASH_DECLARE(mapenum, int, const char*);

namespace innative {
  namespace utility {
    extern const char* IN_GETCPUINFO;
    extern const char* IN_EXTENSION;
    extern const char* IN_ENV_EXTENSION;
    extern const char* IN_GLUE_STRING;
    extern const char* IN_MEMORY_MAX_METADATA;
    extern const char* IN_TEMP_PREFIX;

    extern const std::array<const char*, OP_CODE_COUNT> OPNAMES;

    static const unsigned int WASM_MAGIC_COOKIE = 0x6d736100;
    static const unsigned int WASM_MAGIC_VERSION = 0x01;

    extern const struct kh_mapenum_s* ERR_ENUM_MAP;
    extern const struct kh_mapenum_s* TYPE_ENCODING_MAP;
    extern const struct kh_mapenum_s* WAST_ASSERTION_MAP;

    const char* EnumToString(const struct kh_mapenum_s* h, int i, char* buf, size_t n);
    struct kh_mapenum_s* GenMapEnum(std::initializer_list<std::pair<int, const char*>> list);
  }
}

#endif