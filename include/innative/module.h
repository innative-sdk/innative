// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__MODULE_H
#define IN__MODULE_H

#ifdef __cplusplus
extern "C" {
#endif

enum WASM_MODULE_SECTIONS
{
  WASM_MODULE_TYPE = 1,
  WASM_MODULE_IMPORT_FUNCTION,
  WASM_MODULE_IMPORT_TABLE,
  WASM_MODULE_IMPORT_MEMORY,
  WASM_MODULE_IMPORT_GLOBAL,
  WASM_MODULE_FUNCTION,
  WASM_MODULE_TABLE,
  WASM_MODULE_MEMORY,
  WASM_MODULE_GLOBAL,
  WASM_MODULE_EXPORT,
  WASM_MODULE_ELEMENT,
  WASM_MODULE_CODE,
  WASM_MODULE_DATA,
  WASM_MODULE_CUSTOM,
};

#ifdef __cplusplus
}
#endif

#endif