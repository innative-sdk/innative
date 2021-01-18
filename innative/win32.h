// Copyright (c)2021 Fundament Software
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef __WIN32_H__IN__
#define __WIN32_H__IN__

#pragma pack(push)
#pragma pack(8)
#define WINVER        0x0600 // Vista
#define _WIN32_WINNT  0x0600
#define NTDDI_VERSION 0x06000000 // Vista
#define WIN32_LEAN_AND_MEAN
#ifndef NOMINMAX // Some compilers enable this by default
  #define NOMINMAX
#endif
#define NODRAWTEXT
#define NOBITMAP
#define NOMCX
#define NOSERVICE
#define NOHELP
#define NOGDI
#include <windows.h>
#pragma pack(pop)

#define WIN32_RESOURCE_EMBEDDING      "WASM_EMBEDDING"
#define WIN32_RESOURCE_MODULE         "WASM_MODULE"
#define WIN32_RESOURCE_WHITELIST      "WASM_WHITELIST"
#define WIN32_RESOURCE_FLAGS          "WASM_FLAGS"
#define WIN32_RESOURCE_FLAGS_FLAGS    "flags"
#define WIN32_RESOURCE_FLAGS_OPTIMIZE "optimize"
#define WIN32_RESOURCE_FLAGS_FEATURES "features"
#define WIDESTRING(x)                 L#x

#endif
