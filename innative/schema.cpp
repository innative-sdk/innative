// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "innative/schema.h"
#include "util.h"

bool __WASM_BYTE_ARRAY::operator==(const __WASM_BYTE_ARRAY& r) const
{
  if(r.n_bytes != n_bytes)
    return false;
  return !memcmp(bytes, r.bytes, n_bytes);
}

void __WASM_BYTE_ARRAY::resize(varuint32 sz, bool terminator, const Environment& env)
{
  n_bytes = sz;
  if(sz > 0)
  {
    bytes = innative::utility::tmalloc<uint8_t>(env, n_bytes + terminator);
    if(bytes && terminator)
      bytes[n_bytes] = 0;
  }
  else
    bytes = terminator ? (uint8_t*)"" : nullptr;
}

void __WASM_BYTE_ARRAY::discard(varuint32 sz, bool terminator)
{
  n_bytes = sz;
  if(n_bytes > 0 && terminator)
    bytes[n_bytes] = 0;
}
