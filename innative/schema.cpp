// Copyright (c)2021 Fundament Software
// For conditions of distribution and use, see copyright notice in innative.h

#include "innative/schema.h"
#include "utility.h"

#define str_pair_hash_equal(a, b) ((strcmp(a, b) == 0) && (strcmp(strchr(a, 0) + 1, strchr(b, 0) + 1) == 0))

__KHASH_IMPL(exports, , Identifier, varuint32, 1, innative::internal::__ac_X31_hash_bytearray, kh_int_hash_equal);
__KHASH_IMPL(cimport, , Identifier, char, 1, innative::internal::__ac_X31_hash_bytearray, kh_int_hash_equal);
__KHASH_IMPL(modules, , Identifier, size_t, 1, innative::internal::__ac_X31_hash_bytearray, kh_int_hash_equal);
__KHASH_IMPL(modulepair, , kh_cstr_t, FunctionType, 1, innative::internal::__ac_X31_hash_string_pair, str_pair_hash_equal);
__KHASH_IMPL(fmapping, , kh_cstr_t, kh_cstr_t, 1, __ac_X31_hash_string, kh_str_hash_equal);
__KHASH_IMPL(prependmap, , kh_cstr_t, int, 1, __ac_X31_hash_string, kh_str_hash_equal);

void IN_WASM_BYTE_ARRAY::from(const char* s, const Environment& env)
{
  from(reinterpret_cast<const uint8_t*>(s), strlen(s), env);
}
void IN_WASM_BYTE_ARRAY::from(const uint8_t* b, varuint32 n, const struct IN_WASM_ENVIRONMENT& env)
{
  resize(n, true, env);
  if(bytes != nullptr)
    innative::utility::tmemcpy<uint8_t>(bytes, n, b, n);
}

bool IN_WASM_BYTE_ARRAY::operator==(const IN_WASM_BYTE_ARRAY& r) const
{
  if(r.n_bytes != n_bytes)
    return false;
  return !memcmp(bytes, r.bytes, n_bytes);
}

void IN_WASM_BYTE_ARRAY::resize(varuint32 sz, bool terminator, const Environment& env)
{
  n_bytes = sz;
  if(sz > 0)
  {
    bytes = innative::utility::tmalloc<uint8_t>(env, n_bytes + terminator);
    if(bytes && terminator)
      bytes[n_bytes] = 0;
  }
  else
    bytes = terminator ? reinterpret_cast<uint8_t*>(const_cast<char*>("")) : nullptr;
}

void IN_WASM_BYTE_ARRAY::discard(varuint32 sz, bool terminator)
{
  n_bytes = sz;
  if(n_bytes > 0 && terminator)
    bytes[n_bytes] = 0;
}
