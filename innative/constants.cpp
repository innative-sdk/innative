// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "constants.h"
#include "innative/schema.h"

__KHASH_IMPL(mapenum, , int, const char*, 1, kh_int_hash_func, kh_int_hash_equal);

const std::array<const char*, OP_CODE_COUNT> innative::utility::OPNAMES = {
  // Control flow operators
  "unreachable", // 0x00
  "nop",         // 0x01
  "block",       // 0x02
  "loop",        // 0x03
  "if",          // 0x04
  "else",        // 0x05
  "RESERVED",    // 0x06
  "RESERVED",    // 0x07
  "RESERVED",    // 0x08
  "RESERVED",    // 0x09
  "RESERVED",    // 0x0a
  "end",         // 0x0b
  "br",          // 0x0c
  "br_if",       // 0x0d
  "br_table",    // 0x0e
  "return",      // 0x0f

  // Call operators
  "call",          // 0x10
  "call_indirect", // 0x11

  "RESERVED", // 0x12
  "RESERVED", // 0x13
  "RESERVED", // 0x14
  "RESERVED", // 0x15
  "RESERVED", // 0x16
  "RESERVED", // 0x17
  "RESERVED", // 0x18
  "RESERVED", // 0x19

  // Parametric operators
  "drop",   // 0x1a
  "select", // 0x1b

  "RESERVED", // 0x1c
  "RESERVED", // 0x1d
  "RESERVED", // 0x1e
  "RESERVED", // 0x1f

  // Variable access
  "local.get",  // 0x20
  "local.set",  // 0x21
  "local.tee",  // 0x22
  "global.get", // 0x23
  "global.set", // 0x24

  "RESERVED", // 0x25
  "RESERVED", // 0x26
  "RESERVED", // 0x27

  // Memory-related operator
  "i32.load",     // 0x28
  "i64.load",     // 0x29
  "f32.load",     // 0x2a
  "f64.load",     // 0x2b
  "i32.load8_s",  // 0x2c
  "i32.load8_u",  // 0x2d
  "i32.load16_s", // 0x2e
  "i32.load16_u", // 0x2f
  "i64.load8_s",  // 0x30
  "i64.load8_u",  // 0x31
  "i64.load16_s", // 0x32
  "i64.load16_u", // 0x33
  "i64.load32_s", // 0x34
  "i64.load32_u", // 0x35
  "i32.store",    // 0x36
  "i64.store",    // 0x37
  "f32.store",    // 0x38
  "f64.store",    // 0x39
  "i32.store8",   // 0x3a
  "i32.store16",  // 0x3b
  "i64.store8",   // 0x3c
  "i64.store16",  // 0x3d
  "i64.store32",  // 0x3e
  "memory.size",  // 0x3f
  "memory.grow",  // 0x40

  // Constants
  "i32.const", // 0x41
  "i64.const", // 0x42
  "f32.const", // 0x43
  "f64.const", // 0x44

  // Comparison operators
  "i32.eqz",  // 0x45
  "i32.eq",   // 0x46
  "i32.ne",   // 0x47
  "i32.lt_s", // 0x48
  "i32.lt_u", // 0x49
  "i32.gt_s", // 0x4a
  "i32.gt_u", // 0x4b
  "i32.le_s", // 0x4c
  "i32.le_u", // 0x4d
  "i32.ge_s", // 0x4e
  "i32.ge_u", // 0x4f

  "i64.eqz",  // 0x50
  "i64.eq",   // 0x51
  "i64.ne",   // 0x52
  "i64.lt_s", // 0x53
  "i64.lt_u", // 0x54
  "i64.gt_s", // 0x55
  "i64.gt_u", // 0x56
  "i64.le_s", // 0x57
  "i64.le_u", // 0x58
  "i64.ge_s", // 0x59
  "i64.ge_u", // 0x5a

  "f32.eq", // 0x5b
  "f32.ne", // 0x5c
  "f32.lt", // 0x5d
  "f32.gt", // 0x5e
  "f32.le", // 0x5f
  "f32.ge", // 0x60

  "f64.eq", // 0x61
  "f64.ne", // 0x62
  "f64.lt", // 0x63
  "f64.gt", // 0x64
  "f64.le", // 0x65
  "f64.ge", // 0x66

  // Numeric operators
  "i32.clz",    // 0x67
  "i32.ctz",    // 0x68
  "i32.popcnt", // 0x69
  "i32.add",    // 0x6a
  "i32.sub",    // 0x6b
  "i32.mul",    // 0x6c
  "i32.div_s",  // 0x6d
  "i32.div_u",  // 0x6e
  "i32.rem_s",  // 0x6f
  "i32.rem_u",  // 0x70
  "i32.and",    // 0x71
  "i32.or",     // 0x72
  "i32.xor",    // 0x73
  "i32.shl",    // 0x74
  "i32.shr_s",  // 0x75
  "i32.shr_u",  // 0x76
  "i32.rotl",   // 0x77
  "i32.rotr",   // 0x78

  "i64.clz",    // 0x79
  "i64.ctz",    // 0x7a
  "i64.popcnt", // 0x7b
  "i64.add",    // 0x7c
  "i64.sub",    // 0x7d
  "i64.mul",    // 0x7e
  "i64.div_s",  // 0x7f
  "i64.div_u",  // 0x80
  "i64.rem_s",  // 0x81
  "i64.rem_u",  // 0x82
  "i64.and",    // 0x83
  "i64.or",     // 0x84
  "i64.xor",    // 0x85
  "i64.shl",    // 0x86
  "i64.shr_s",  // 0x87
  "i64.shr_u",  // 0x88
  "i64.rotl",   // 0x89
  "i64.rotr",   // 0x8a

  "f32.abs",      // 0x8b
  "f32.neg",      // 0x8c
  "f32.ceil",     // 0x8d
  "f32.floor",    // 0x8e
  "f32.trunc",    // 0x8f
  "f32.nearest",  // 0x90
  "f32.sqrt",     // 0x91
  "f32.add",      // 0x92
  "f32.sub",      // 0x93
  "f32.mul",      // 0x94
  "f32.div",      // 0x95
  "f32.min",      // 0x96
  "f32.max",      // 0x97
  "f32.copysign", // 0x98

  "f64.abs",      // 0x99
  "f64.neg",      // 0x9a
  "f64.ceil",     // 0x9b
  "f64.floor",    // 0x9c
  "f64.trunc",    // 0x9d
  "f64.nearest",  // 0x9e
  "f64.sqrt",     // 0x9f
  "f64.add",      // 0xa0
  "f64.sub",      // 0xa1
  "f64.mul",      // 0xa2
  "f64.div",      // 0xa3
  "f64.min",      // 0xa4
  "f64.max",      // 0xa5
  "f64.copysign", // 0xa6

  // Conversions
  "i32.wrap_i64",    // 0xa7
  "i32.trunc_f32_s", // 0xa8
  "i32.trunc_f32_u", // 0xa9
  "i32.trunc_f64_s", // 0xaa
  "i32.trunc_f64_u", // 0xab

  "i64.extend_i32_s", // 0xac
  "i64.extend_i32_u", // 0xad
  "i64.trunc_f32_s",  // 0xae
  "i64.trunc_f32_u",  // 0xaf
  "i64.trunc_f64_s",  // 0xb0
  "i64.trunc_f64_u",  // 0xb1

  "f32.convert_i32_s", // 0xb2
  "f32.convert_i32_u", // 0xb3
  "f32.convert_i64_s", // 0xb4
  "f32.convert_i64_u", // 0xb5
  "f32.demote_f64",    // 0xb6

  "f64.convert_i32_s", // 0xb7
  "f64.convert_i32_u", // 0xb8
  "f64.convert_i64_s", // 0xb9
  "f64.convert_i64_u", // 0xba
  "f64.promote_f32",   // 0xbb

  // Reinterpretations
  "i32.reinterpret_f32", // 0xbc
  "i64.reinterpret_f64", // 0xbd
  "f32.reinterpret_i32", // 0xbe
  "f64.reinterpret_i64"  // 0xbf
};

namespace innative {
  namespace utility {
    kh_mapenum_s* GenMapEnum(std::initializer_list<std::pair<int, const char*>> list)
    {
      auto h = kh_init_mapenum();
      int r;

      for(auto& e : list)
      {
        auto iter       = kh_put_mapenum(h, e.first, &r);
        kh_val(h, iter) = e.second;
      }

      return h;
    }

    const kh_mapenum_s* ERR_ENUM_MAP = GenMapEnum({
      { ERR_SUCCESS, "ERR_SUCCESS" },
      { ERR_PARSE_UNEXPECTED_EOF, "ERR_PARSE_UNEXPECTED_EOF" },
      { ERR_PARSE_INVALID_MAGIC_COOKIE, "ERR_PARSE_INVALID_MAGIC_COOKIE" },
      { ERR_PARSE_INVALID_VERSION, "ERR_PARSE_INVALID_VERSION" },
      { ERR_PARSE_INVALID_FILE_LENGTH, "ERR_PARSE_INVALID_FILE_LENGTH" },
      { ERR_PARSE_INVALID_NAME, "ERR_PARSE_INVALID_NAME" },
      { ERR_UNKNOWN_COMMAND_LINE, "ERR_UNKNOWN_COMMAND_LINE" },
      { ERR_MISSING_COMMAND_LINE_PARAMETER, "ERR_MISSING_COMMAND_LINE_PARAMETER" },
      { ERR_NO_INPUT_FILES, "ERR_NO_INPUT_FILES" },
      { ERR_UNKNOWN_ENVIRONMENT_ERROR, "ERR_UNKNOWN_ENVIRONMENT_ERROR" },
      { ERR_COMMAND_LINE_CONFLICT, "ERR_COMMAND_LINE_CONFLICT" },
      { ERR_UNKNOWN_FLAG, "ERR_UNKNOWN_FLAG" },
      { ERR_MISSING_LOADER, "ERR_MISSING_LOADER" },
      { ERR_INSUFFICIENT_BUFFER, "ERR_INSUFFICIENT_BUFFER" },
      { ERR_FATAL_INVALID_WASM_SECTION_ORDER, "ERR_FATAL_INVALID_WASM_SECTION_ORDER" },
      { ERR_FATAL_INVALID_MODULE, "ERR_FATAL_INVALID_MODULE" },
      { ERR_FATAL_INVALID_ENCODING, "ERR_FATAL_INVALID_ENCODING" },
      { ERR_FATAL_OVERLONG_ENCODING, "ERR_FATAL_OVERLONG_ENCODING" },
      { ERR_FATAL_UNKNOWN_KIND, "ERR_FATAL_UNKNOWN_KIND" },
      { ERR_FATAL_UNKNOWN_INSTRUCTION, "ERR_FATAL_UNKNOWN_INSTRUCTION" },
      { ERR_FATAL_UNKNOWN_SECTION, "ERR_FATAL_UNKNOWN_SECTION" },
      { ERR_FATAL_UNKNOWN_FUNCTION_SIGNATURE, "ERR_FATAL_UNKNOWN_FUNCTION_SIGNATURE" },
      { ERR_FATAL_UNKNOWN_TARGET, "ERR_FATAL_UNKNOWN_TARGET" },
      { ERR_FATAL_EXPECTED_END_INSTRUCTION, "ERR_FATAL_EXPECTED_END_INSTRUCTION" },
      { ERR_FATAL_NULL_POINTER, "ERR_FATAL_NULL_POINTER" },
      { ERR_FATAL_BAD_ELEMENT_TYPE, "ERR_FATAL_BAD_ELEMENT_TYPE" },
      { ERR_FATAL_BAD_HASH, "ERR_FATAL_BAD_HASH" },
      { ERR_FATAL_DUPLICATE_EXPORT, "ERR_FATAL_DUPLICATE_EXPORT" },
      { ERR_FATAL_DUPLICATE_MODULE_NAME, "ERR_FATAL_DUPLICATE_MODULE_NAME" },
      { ERR_FATAL_FILE_ERROR, "ERR_FATAL_FILE_ERROR" },
      { ERR_FATAL_LINK_ERROR, "ERR_FATAL_LINK_ERROR" },
      { ERR_FATAL_TOO_MANY_LOCALS, "ERR_FATAL_TOO_MANY_LOCALS" },
      { ERR_FATAL_OUT_OF_MEMORY, "ERR_FATAL_OUT_OF_MEMORY" },
      { ERR_FATAL_RESOURCE_ERROR, "ERR_FATAL_RESOURCE_ERROR" },
      { ERR_FATAL_NO_OUTPUT_FILE, "ERR_FATAL_NO_OUTPUT_FILE" },
      { ERR_FATAL_NO_MODULES, "ERR_FATAL_NO_MODULES" },
      { ERR_FATAL_NO_START_FUNCTION, "ERR_FATAL_NO_START_FUNCTION" },
      { ERR_VALIDATION_ERROR, "ERR_VALIDATION_ERROR" },
      { ERR_INVALID_FUNCTION_SIG, "ERR_INVALID_FUNCTION_SIG" },
      { ERR_INVALID_FUNCTION_INDEX, "ERR_INVALID_FUNCTION_INDEX" },
      { ERR_INVALID_TABLE_INDEX, "ERR_INVALID_TABLE_INDEX" },
      { ERR_INVALID_MEMORY_INDEX, "ERR_INVALID_MEMORY_INDEX" },
      { ERR_INVALID_GLOBAL_INDEX, "ERR_INVALID_GLOBAL_INDEX" },
      { ERR_INVALID_IMPORT_MEMORY_MINIMUM, "ERR_INVALID_IMPORT_MEMORY_MINIMUM" },
      { ERR_INVALID_IMPORT_MEMORY_MAXIMUM, "ERR_INVALID_IMPORT_MEMORY_MAXIMUM" },
      { ERR_INVALID_IMPORT_TABLE_MINIMUM, "ERR_INVALID_IMPORT_TABLE_MINIMUM" },
      { ERR_INVALID_IMPORT_TABLE_MAXIMUM, "ERR_INVALID_IMPORT_TABLE_MAXIMUM" },
      { ERR_INVALID_IDENTIFIER, "ERR_INVALID_IDENTIFIER" },
      { ERR_INVALID_TABLE_ELEMENT_TYPE, "ERR_INVALID_TABLE_ELEMENT_TYPE" },
      { ERR_INVALID_LIMITS, "ERR_INVALID_LIMITS" },
      { ERR_INVALID_INITIALIZER, "ERR_INVALID_INITIALIZER" },
      { ERR_INVALID_GLOBAL_INITIALIZER, "ERR_INVALID_GLOBAL_INITIALIZER" },
      { ERR_INVALID_INITIALIZER_TYPE, "ERR_INVALID_INITIALIZER_TYPE" },
      { ERR_INVALID_GLOBAL_TYPE, "ERR_INVALID_GLOBAL_TYPE" },
      { ERR_INVALID_GLOBAL_IMPORT_TYPE, "ERR_INVALID_GLOBAL_IMPORT_TYPE" },
      { ERR_INVALID_TABLE_TYPE, "ERR_INVALID_TABLE_TYPE" },
      { ERR_INVALID_MEMORY_TYPE, "ERR_INVALID_MEMORY_TYPE" },
      { ERR_INVALID_START_FUNCTION, "ERR_INVALID_START_FUNCTION" },
      { ERR_INVALID_TABLE_OFFSET, "ERR_INVALID_TABLE_OFFSET" },
      { ERR_INVALID_MEMORY_OFFSET, "ERR_INVALID_MEMORY_OFFSET" },
      { ERR_INVALID_FUNCTION_BODY, "ERR_INVALID_FUNCTION_BODY" },
      { ERR_INVALID_FUNCTION_IMPORT_TYPE, "ERR_INVALID_FUNCTION_IMPORT_TYPE" },
      { ERR_INVALID_VALUE_STACK, "ERR_INVALID_VALUE_STACK" },
      { ERR_EMPTY_VALUE_STACK, "ERR_EMPTY_VALUE_STACK" },
      { ERR_INVALID_TYPE, "ERR_INVALID_TYPE" },
      { ERR_INVALID_TYPE_INDEX, "ERR_INVALID_TYPE_INDEX" },
      { ERR_INVALID_BRANCH_DEPTH, "ERR_INVALID_BRANCH_DEPTH" },
      { ERR_INVALID_LOCAL_INDEX, "ERR_INVALID_LOCAL_INDEX" },
      { ERR_INVALID_ARGUMENT_TYPE, "ERR_INVALID_ARGUMENT_TYPE" },
      { ERR_INVALID_BLOCK_SIGNATURE, "ERR_INVALID_BLOCK_SIGNATURE" },
      { ERR_INVALID_MEMORY_ALIGNMENT, "ERR_INVALID_MEMORY_ALIGNMENT" },
      { ERR_INVALID_RESERVED_VALUE, "ERR_INVALID_RESERVED_VALUE" },
      { ERR_INVALID_UTF8_ENCODING, "ERR_INVALID_UTF8_ENCODING" },
      { ERR_INVALID_DATA_SEGMENT, "ERR_INVALID_DATA_SEGMENT" },
      { ERR_INVALID_MUTABILITY, "ERR_INVALID_MUTABILITY" },
      { ERR_INVALID_EMBEDDING, "ERR_INVALID_EMBEDDING" },
      { ERR_IMMUTABLE_GLOBAL, "ERR_IMMUTABLE_GLOBAL" },
      { ERR_UNKNOWN_SIGNATURE_TYPE, "ERR_UNKNOWN_SIGNATURE_TYPE" },
      { ERR_UNKNOWN_MODULE, "ERR_UNKNOWN_MODULE" },
      { ERR_UNKNOWN_EXPORT, "ERR_UNKNOWN_EXPORT" },
      { ERR_UNKNOWN_BLANK_IMPORT, "ERR_UNKNOWN_BLANK_IMPORT" },
      { ERR_EMPTY_IMPORT, "ERR_EMPTY_IMPORT" },
      { ERR_MULTIPLE_RETURN_VALUES, "ERR_MULTIPLE_RETURN_VALUES" },
      { ERR_MULTIPLE_TABLES, "ERR_MULTIPLE_TABLES" },
      { ERR_MULTIPLE_MEMORIES, "ERR_MULTIPLE_MEMORIES" },
      { ERR_IMPORT_EXPORT_MISMATCH, "ERR_IMPORT_EXPORT_MISMATCH" },
      { ERR_IMPORT_EXPORT_TYPE_MISMATCH, "ERR_IMPORT_EXPORT_TYPE_MISMATCH" },
      { ERR_FUNCTION_BODY_MISMATCH, "ERR_FUNCTION_BODY_MISMATCH" },
      { ERR_MEMORY_MINIMUM_TOO_LARGE, "ERR_MEMORY_MINIMUM_TOO_LARGE" },
      { ERR_MEMORY_MAXIMUM_TOO_LARGE, "ERR_MEMORY_MAXIMUM_TOO_LARGE" },
      { ERR_IF_ELSE_MISMATCH, "ERR_IF_ELSE_MISMATCH" },
      { ERR_END_MISMATCH, "ERR_END_MISMATCH" },
      { ERR_SIGNATURE_MISMATCH, "ERR_SIGNATURE_MISMATCH" },
      { ERR_EXPECTED_ELSE_INSTRUCTION, "ERR_EXPECTED_ELSE_INSTRUCTION" },
      { ERR_ILLEGAL_C_IMPORT, "ERR_ILLEGAL_C_IMPORT" },
      { ERR_WAT_INTERNAL_ERROR, "ERR_WAT_INTERNAL_ERROR" },
      { ERR_WAT_EXPECTED_OPEN, "ERR_WAT_EXPECTED_OPEN" },
      { ERR_WAT_EXPECTED_CLOSE, "ERR_WAT_EXPECTED_CLOSE" },
      { ERR_WAT_EXPECTED_TOKEN, "ERR_WAT_EXPECTED_TOKEN" },
      { ERR_WAT_EXPECTED_NAME, "ERR_WAT_EXPECTED_NAME" },
      { ERR_WAT_EXPECTED_STRING, "ERR_WAT_EXPECTED_STRING" },
      { ERR_WAT_EXPECTED_VALUE, "ERR_WAT_EXPECTED_VALUE" },
      { ERR_WAT_EXPECTED_NUMBER, "ERR_WAT_EXPECTED_NUMBER" },
      { ERR_WAT_EXPECTED_TYPE, "ERR_WAT_EXPECTED_TYPE" },
      { ERR_WAT_EXPECTED_VAR, "ERR_WAT_EXPECTED_VAR" },
      { ERR_WAT_EXPECTED_VALTYPE, "ERR_WAT_EXPECTED_VALTYPE" },
      { ERR_WAT_EXPECTED_FUNC, "ERR_WAT_EXPECTED_FUNC" },
      { ERR_WAT_EXPECTED_OPERATOR, "ERR_WAT_EXPECTED_OPERATOR" },
      { ERR_WAT_EXPECTED_INTEGER, "ERR_WAT_EXPECTED_INTEGER" },
      { ERR_WAT_EXPECTED_FLOAT, "ERR_WAT_EXPECTED_FLOAT" },
      { ERR_WAT_EXPECTED_RESULT, "ERR_WAT_EXPECTED_RESULT" },
      { ERR_WAT_EXPECTED_THEN, "ERR_WAT_EXPECTED_THEN" },
      { ERR_WAT_EXPECTED_ELSE, "ERR_WAT_EXPECTED_ELSE" },
      { ERR_WAT_EXPECTED_END, "ERR_WAT_EXPECTED_END" },
      { ERR_WAT_EXPECTED_LOCAL, "ERR_WAT_EXPECTED_LOCAL" },
      { ERR_WAT_EXPECTED_FUNCREF, "ERR_WAT_EXPECTED_FUNCREF" },
      { ERR_WAT_EXPECTED_MUT, "ERR_WAT_EXPECTED_MUT" },
      { ERR_WAT_EXPECTED_MODULE, "ERR_WAT_EXPECTED_MODULE" },
      { ERR_WAT_EXPECTED_ELEM, "ERR_WAT_EXPECTED_ELEM" },
      { ERR_WAT_EXPECTED_KIND, "ERR_WAT_EXPECTED_KIND" },
      { ERR_WAT_EXPECTED_EXPORT, "ERR_WAT_EXPECTED_EXPORT" },
      { ERR_WAT_EXPECTED_IMPORT, "ERR_WAT_EXPECTED_IMPORT" },
      { ERR_WAT_EXPECTED_BINARY, "ERR_WAT_EXPECTED_BINARY" },
      { ERR_WAT_EXPECTED_QUOTE, "ERR_WAT_EXPECTED_QUOTE" },
      { ERR_WAT_INVALID_TOKEN, "ERR_WAT_INVALID_TOKEN" },
      { ERR_WAT_INVALID_NUMBER, "ERR_WAT_INVALID_NUMBER" },
      { ERR_WAT_INVALID_IMPORT_ORDER, "ERR_WAT_INVALID_IMPORT_ORDER" },
      { ERR_WAT_INVALID_ALIGNMENT, "ERR_WAT_INVALID_ALIGNMENT" },
      { ERR_WAT_INVALID_NAME, "ERR_WAT_INVALID_NAME" },
      { ERR_WAT_INVALID_VAR, "ERR_WAT_INVALID_VAR" },
      { ERR_WAT_INVALID_TYPE, "ERR_WAT_INVALID_TYPE" },
      { ERR_WAT_INVALID_LOCAL, "ERR_WAT_INVALID_LOCAL" },
      { ERR_WAT_UNKNOWN_TYPE, "ERR_WAT_UNKNOWN_TYPE" },
      { ERR_WAT_UNEXPECTED_NAME, "ERR_WAT_UNEXPECTED_NAME" },
      { ERR_WAT_TYPE_MISMATCH, "ERR_WAT_TYPE_MISMATCH" },
      { ERR_WAT_LABEL_MISMATCH, "ERR_WAT_LABEL_MISMATCH" },
      { ERR_WAT_OUT_OF_RANGE, "ERR_WAT_OUT_OF_RANGE" },
      { ERR_WAT_BAD_ESCAPE, "ERR_WAT_BAD_ESCAPE" },
      { ERR_WAT_DUPLICATE_NAME, "ERR_WAT_DUPLICATE_NAME" },
      { ERR_WAT_PARAM_AFTER_RESULT, "ERR_WAT_PARAM_AFTER_RESULT" },
      { ERR_RUNTIME_INIT_ERROR, "ERR_RUNTIME_INIT_ERROR" },
      { ERR_RUNTIME_TRAP, "ERR_RUNTIME_TRAP" },
      { ERR_RUNTIME_ASSERT_FAILURE, "ERR_RUNTIME_ASSERT_FAILURE" },
    });

    const kh_mapenum_s* TYPE_ENCODING_MAP = GenMapEnum({
      { TE_i32, "TE_i32" },
      { TE_i64, "TE_i64" },
      { TE_f32, "TE_f32" },
      { TE_f64, "TE_f64" },
      { TE_funcref, "TE_funcref" },
      { TE_cref, "TE_cref" },
      { TE_func, "TE_func" },
      { TE_void, "TE_void" },
      { TE_iPTR, "TE_iPTR" },
      { TE_NONE, "TE_NONE" },
      { TE_POLY, "TE_POLY" },
    });

    const kh_mapenum_s* WAST_ASSERTION_MAP = GenMapEnum({
      { ERR_WAT_INVALID_ALIGNMENT, "alignment" },
      { ERR_INVALID_MEMORY_ALIGNMENT, "alignment must not be larger than natural" },
      { ERR_INVALID_MEMORY_OFFSET, "out of bounds memory access" },
      { ERR_END_MISMATCH, "unexpected end" },
      { ERR_PARSE_INVALID_MAGIC_COOKIE, "magic header not detected" },
      { ERR_PARSE_INVALID_VERSION, "unknown binary version" },
      { ERR_FATAL_OVERLONG_ENCODING, "integer representation too long" },
      { ERR_FATAL_INVALID_ENCODING, "integer too large" },
      { ERR_INVALID_RESERVED_VALUE, "zero flag expected" },
      { ERR_FATAL_TOO_MANY_LOCALS, "too many locals" },
      { ERR_IMPORT_EXPORT_MISMATCH, "type mismatch" },
      { ERR_INVALID_BLOCK_SIGNATURE, "type mismatch" },
      { ERR_EMPTY_VALUE_STACK, "type mismatch" },
      { ERR_INVALID_VALUE_STACK, "type mismatch" },
      { ERR_INVALID_TYPE, "type mismatch" },
      { ERR_WAT_TYPE_MISMATCH, "inline function type" },
      { ERR_WAT_UNKNOWN_TYPE, "unknown type" },
      { ERR_WAT_LABEL_MISMATCH, "mismatching label" },
      { ERR_INVALID_BRANCH_DEPTH, "unknown label" },
      { ERR_INVALID_FUNCTION_INDEX, "unknown function" },
      { ERR_INVALID_TABLE_INDEX, "unknown table" },
      { ERR_WAT_OUT_OF_RANGE, "constant out of range" },
      { ERR_PARSE_UNEXPECTED_EOF, "unexpected end" },
      { ERR_WAT_EXPECTED_OPERATOR, "unexpected token" },
      { ERR_WAT_UNEXPECTED_NAME, "unexpected token" },
      { ERR_PARSE_INVALID_FILE_LENGTH, "unexpected end" },
      { ERR_FATAL_UNKNOWN_SECTION, "invalid section id" },
      { ERR_FUNCTION_BODY_MISMATCH, "function and code section have inconsistent lengths" },
      { ERR_FATAL_DUPLICATE_EXPORT, "duplicate export name" },
      { ERR_INVALID_MEMORY_INDEX, "unknown memory" },
      { ERR_INVALID_GLOBAL_INDEX, "unknown global" },
      { ERR_INVALID_TABLE_INDEX, "unknown table" },
      { ERR_WAT_INVALID_NUMBER, "unknown operator" },
      { ERR_WAT_INVALID_TOKEN, "unknown operator" },
      { ERR_INVALID_GLOBAL_INITIALIZER, "unknown global" },
      { ERR_INVALID_INITIALIZER_TYPE, "type mismatch" },
      { ERR_INVALID_GLOBAL_TYPE, "type mismatch" },
      { ERR_INVALID_TABLE_TYPE, "type mismatch" },
      { ERR_INVALID_LOCAL_INDEX, "unknown local" },
      { ERR_MULTIPLE_RETURN_VALUES, "invalid result arity" },
      { ERR_INVALID_START_FUNCTION, "start function" },
      { ERR_WAT_EXPECTED_VAR, "unknown operator" },
      { ERR_WAT_EXPECTED_VALTYPE, "unexpected token" },
      { ERR_INVALID_UTF8_ENCODING, "invalid UTF-8 encoding" },
      { ERR_INVALID_DATA_SEGMENT, "data segment does not fit" },
      { ERR_INVALID_TABLE_OFFSET, "elements segment does not fit" },
      { ERR_IMMUTABLE_GLOBAL, "global is immutable" },
      { ERR_INVALID_MUTABILITY, "invalid mutability" },
      { ERR_UNKNOWN_EXPORT, "unknown import" },
      { ERR_INVALID_GLOBAL_IMPORT_TYPE, "incompatible import type" },
      { ERR_INVALID_FUNCTION_IMPORT_TYPE, "incompatible import type" },
      { ERR_WAT_INVALID_IMPORT_ORDER, "invalid import order" },
      { ERR_INVALID_IMPORT_MEMORY_MINIMUM, "incompatible import type" },
      { ERR_INVALID_IMPORT_MEMORY_MAXIMUM, "incompatible import type" },
      { ERR_INVALID_IMPORT_TABLE_MINIMUM, "incompatible import type" },
      { ERR_INVALID_IMPORT_TABLE_MAXIMUM, "incompatible import type" },
      { ERR_MULTIPLE_TABLES, "multiple tables" },
      { ERR_MULTIPLE_MEMORIES, "multiple memories" },
      { ERR_IMPORT_EXPORT_TYPE_MISMATCH, "incompatible import type" },
      { ERR_UNKNOWN_MODULE, "unknown import" },
      { ERR_INVALID_LIMITS, "size minimum must not be greater than maximum" },
      { ERR_MEMORY_MAXIMUM_TOO_LARGE, "memory size must be at most 65536 pages (4GiB)" },
      { ERR_MEMORY_MINIMUM_TOO_LARGE, "memory size must be at most 65536 pages (4GiB)" },
      { ERR_INVALID_INITIALIZER, "constant expression required" },
      { ERR_EMPTY_IMPORT, "unknown import" },
      { ERR_WAT_PARAM_AFTER_RESULT, "unexpected token" },
    });

    const char* EnumToString(const kh_mapenum_s* h, int i, char* buf, size_t n)
    {
      khiter_t iter = kh_get_mapenum(h, i);
      if(kh_exist2(h, iter))
        return kh_val(h, iter);
      if(!buf)
        return nullptr;
      snprintf(buf, n, "%i", i);
      buf[n - 1] = 0;
      return buf;
    }
  }
}
