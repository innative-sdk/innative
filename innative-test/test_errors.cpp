// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "test.h"

void TestHarness::test_errors()
{
  for(int i = ERR_PARSE_UNEXPECTED_EOF; i <= ERR_INSUFFICIENT_BUFFER; ++i)
    TEST((*_exports.GetErrorString)(i) != nullptr);

  for(int i = ERR_FATAL_INVALID_WASM_SECTION_ORDER; i <= ERR_FATAL_NO_START_FUNCTION; ++i)
    TEST((*_exports.GetErrorString)(i) != nullptr);

  for(int i = ERR_VALIDATION_ERROR; i <= ERR_ILLEGAL_C_IMPORT; ++i)
    TEST((*_exports.GetErrorString)(i) != nullptr);

  for(int i = ERR_WAT_INTERNAL_ERROR; i <= ERR_WAT_PARAM_AFTER_RESULT; ++i)
    TEST((*_exports.GetErrorString)(i) != nullptr);

  TEST((*_exports.GetErrorString)(ERR_SUCCESS) != nullptr);
  TEST((*_exports.GetErrorString)(ERR_RUNTIME_INIT_ERROR) != nullptr);
  TEST((*_exports.GetErrorString)(ERR_RUNTIME_TRAP) != nullptr);
  TEST((*_exports.GetErrorString)(ERR_RUNTIME_ASSERT_FAILURE) != nullptr);
}