// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef __CONSTANTS_H__IR__
#define __CONSTANTS_H__IR__

#define IR_INIT_FUNCTION "_innative_internal_start"

namespace innative {
  namespace utility {
    extern const char* IR_ENTRYPOINT;
    extern const char* IR_GETCPUINFO;
    extern const char* IR_EXTENSION;
    extern const char* IR_ENV_EXTENSION;
    extern const char* IR_GLUE_STRING;

    extern int OPNAMECOUNT;
    extern const char OPNAMES[][20];

    static const unsigned int WASM_MAGIC_COOKIE = 0x6d736100;
    static const unsigned int WASM_MAGIC_VERSION = 0x01;
  }
}

#endif