// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef __WIN32_H__IR__
#define __WIN32_H__IR__

#pragma pack(push)
#pragma pack(8)
#define WINVER 0x0501 //_WIN32_WINNT_WINXP   
#define _WIN32_WINNT 0x0501
#define NTDDI_VERSION 0x05010300 //NTDDI_WINXPSP3 
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

#define WIN32_RESOURCE_EMBEDDING L"WASM_EMBEDDING"
#define WIN32_RESOURCE_MODULE L"WASM_MODULE"
#define WIN32_RESOURCE_WHITELIST L"WASM_WHITELIST"
#define WIN32_RESOURCE_FLAGS L"WASM_FLAGS"

#endif
