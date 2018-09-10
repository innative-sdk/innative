// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "serialize.h"
#include <stdarg.h>

using namespace innative;
using namespace utility;
using namespace wat;

Tokens innative::wat::TypeEncodingToken(varsint7 type_encoding)
{
  switch(type_encoding)
  {
  case TE_i32:return TOKEN_i32;
  case TE_i64:return TOKEN_i64;
  case TE_f32:return TOKEN_f32;
  case TE_f64:return TOKEN_f64;
  case TE_anyfunc:return TOKEN_ANYFUNC;
  case TE_func:return TOKEN_FUNC;
  case TE_void:return TOKEN_NONE;
  }
  assert(false);
  return TOKEN_NONE;
}

void innative::wat::PushNewNameToken(Queue<Token>& tokens, const char* format, ...)
{
  va_list args;
  va_start(args, format);

  size_t len = vsnprintf(0, 0, format, args);
  char* s = tmalloc<char>(len + 1);
  vsnprintf(s, len + 1, format, args);
  tokens.Push(Token{ TOKEN_NAME, s, (int64_t)len });

  va_end(args);
}

void innative::wat::PushFunctionName(Queue<Token>& tokens, const Module& m, varuint32 index)
{
  Identifier* name = 0;
  if(index < m.importsection.functions)
    name = &m.importsection.imports[index].func_desc.debug_name;
  else if(index - m.importsection.functions < m.code.n_funcbody)
    name = &m.code.funcbody[index - m.importsection.functions].debug_name;

  if(!name || !name->size())
    PushNewNameToken(tokens, "f%u", index);
  else
    PushIdentifierToken(tokens, *name, TOKEN_NAME);
}

void innative::wat::PushIdentifierToken(Queue<Token>& tokens, const ByteArray& id, Tokens token)
{
  tokens.Push(Token{ token, id.str(), id.size() });
}

void innative::wat::PushLocalName(Queue<Token>& tokens, varuint32 index, const char** names, varuint32 num, char prefix)
{
  if(index < num && names && names[index] != 0)
    PushNewNameToken(tokens, names[index]);
  else
    PushNewNameToken(tokens, "%c%u", prefix, index);
}

void innative::wat::TokenizeInstruction(Queue<Token>& tokens, const Module& m, const Instruction& ins, const FunctionBody* body, const FunctionType* ftype)
{
  if(ins.opcode >= OPNAMECOUNT)
  {
    tokens.Push(Token{ TOKEN_NONE });
    assert(false);
    return;
  }

  tokens.Push(Token{ TOKEN_OPERATOR, 0, ins.opcode });

  switch(ins.opcode)
  {
  case OP_get_local:
  case OP_set_local:
  case OP_tee_local:
    if(body && ftype)
    {
      if(ins.immediates[0]._varuint32 < ftype->n_params)
        PushLocalName(tokens, ins.immediates[0]._varuint32, body->param_names, ftype->n_params, 'p');
      else
        PushLocalName(tokens, ins.immediates[0]._varuint32 - ftype->n_params, body->local_names, body->n_locals, 'l');
      break;
    } // If this isn't actually a function, just write out the integer by falling through
  case OP_get_global:
  case OP_set_global:
  case OP_br:
  case OP_br_if:
    tokens.Push(Token{ TOKEN_INTEGER, 0, ins.immediates[0]._varuint32 });
    break;
  case OP_i32_const:
    tokens.Push(Token{ TOKEN_INTEGER, 0, ins.immediates[0]._varsint32 });
    break;
  case OP_i64_const:
    tokens.Push(Token{ TOKEN_INTEGER, 0, ins.immediates[0]._varsint64 });
    break;
  case OP_f32_const:
    tokens.Push(Token{ TOKEN_FLOAT, 0 });
    tokens.Back().f = ins.immediates[0]._float32;
    break;
  case OP_f64_const:
    tokens.Push(Token{ TOKEN_FLOAT, 0 });
    tokens.Back().f = ins.immediates[0]._float64;
    break;
  case OP_br_table:
    for(uint64_t i = 0; i < ins.immediates[0].n_table; ++i)
    {
      tokens.Push(Token{ TOKEN_INTEGER, 0, ins.immediates[0].table[i] });
    }

    tokens.Push(Token{ TOKEN_INTEGER, 0, ins.immediates[1]._varuint32 });
    break;
  case OP_call:
    PushFunctionName(tokens, m, ins.immediates[0]._varuint32);
    break;
  case OP_call_indirect:
    tokens.Push(Token{ TOKEN_INTEGER, 0, ins.immediates[0]._varuint32 });
    break;
  case OP_i32_load:
  case OP_i64_load:
  case OP_f32_load:
  case OP_f64_load:
  case OP_i32_store:
  case OP_i64_store:
  case OP_f32_store:
  case OP_f64_store:
  case OP_i32_load8_s:
  case OP_i32_load16_s:
  case OP_i64_load8_s:
  case OP_i64_load16_s:
  case OP_i64_load32_s:
  case OP_i32_load8_u:
  case OP_i32_load16_u:
  case OP_i64_load8_u:
  case OP_i64_load16_u:
  case OP_i64_load32_u:
  case OP_i32_store8:
  case OP_i32_store16:
  case OP_i64_store8:
  case OP_i64_store16:
  case OP_i64_store32:
    if(ins.immediates[0]._varuint32 != 0)
    {
      tokens.Push(Token{ TOKEN_ALIGN });
      tokens.Push(Token{ TOKEN_INTEGER, 0, (1ULL << (int64_t)ins.immediates[0]._varuint32) });
    }

    if(ins.immediates[1]._varuptr != 0)
    {
      tokens.Push(Token{ TOKEN_OFFSET });
      tokens.Push(Token{ TOKEN_INTEGER, 0, (1ULL << (int64_t)ins.immediates[1]._varuptr) });
    }
    break;
  }
}

void innative::wat::PushExportToken(Queue<Token>& tokens, const Module& m, WASM_KIND kind, varuint32 index)
{
  if(m.knownsections&(1 << WASM_SECTION_EXPORT))
    for(uint64_t i = 0; i < m.exportsection.n_exports; ++i)
    {
      if(m.exportsection.exports[i].kind == kind && m.exportsection.exports[i].index == index)
      {
        tokens.Push(Token{ TOKEN_OPEN });
        tokens.Push(Token{ TOKEN_EXPORT });
        PushIdentifierToken(tokens, m.exportsection.exports[i].name);
        tokens.Push(Token{ TOKEN_CLOSE }); // do NOT break here, because you can export a function under multiple names
      }
    }
}

void innative::wat::TokenizeModule(Queue<Token>& tokens, const Module& m)
{
  tokens.Push(Token{ TOKEN_OPEN });
  tokens.Push(Token{ TOKEN_MODULE });

  if(m.name.size())
    tokens.Push(Token{ TOKEN_NAME, m.name.str(), m.name.size() });

  if(m.knownsections&(1 << WASM_SECTION_TYPE))
    for(uint64_t i = 0; i < m.type.n_functions; ++i)
    {
      tokens.Push(Token{ TOKEN_OPEN });
      tokens.Push(Token{ TOKEN_TYPE });

      PushNewNameToken(tokens, "t%u", (varuint32)i);

      tokens.Push(Token{ TOKEN_OPEN });
      tokens.Push(Token{ TypeEncodingToken(m.type.functions[i].form) });

      for(uint64_t j = 0; j < m.type.functions[i].n_params; ++j)
      {
        tokens.Push(Token{ TOKEN_OPEN });
        tokens.Push(Token{ TOKEN_PARAM });
        tokens.Push(Token{ TypeEncodingToken(m.type.functions[i].params[j]) });
        tokens.Push(Token{ TOKEN_CLOSE });
      }

      for(uint64_t j = 0; j < m.type.functions[i].n_returns; ++j)
      {
        tokens.Push(Token{ TOKEN_OPEN });
        tokens.Push(Token{ TOKEN_RESULT });
        tokens.Push(Token{ TypeEncodingToken(m.type.functions[i].returns[j]) });
        tokens.Push(Token{ TOKEN_CLOSE });
      }

      tokens.Push(Token{ TOKEN_CLOSE });
      tokens.Push(Token{ TOKEN_CLOSE });
    }

  auto tokenize_limits = [](Queue<Token>& tokens, const ResizableLimits& limits) {
    tokens.Push(Token{ TOKEN_INTEGER, 0, limits.minimum });
    if(limits.flags&WASM_LIMIT_HAS_MAXIMUM)
      tokens.Push(Token{ TOKEN_INTEGER, 0, limits.maximum });
  };

  auto tokenize_global = [](Queue<Token>& tokens, const GlobalDesc& global) {
    if(global.mutability)
    {
      tokens.Push(Token{ TOKEN_OPEN });
      tokens.Push(Token{ TOKEN_MUT });
      tokens.Push(Token{ TypeEncodingToken(global.type) });
      tokens.Push(Token{ TOKEN_CLOSE });
    }
    else
      tokens.Push(Token{ TypeEncodingToken(global.type) });
  };

  if(m.knownsections&(1 << WASM_SECTION_IMPORT))
    for(uint64_t i = 0; i < m.importsection.n_import; ++i)
    {
      auto& imp = m.importsection.imports[i];
      tokens.Push(Token{ TOKEN_OPEN });
      tokens.Push(Token{ TOKEN_IMPORT });
      PushIdentifierToken(tokens, imp.module_name);
      PushIdentifierToken(tokens, imp.export_name);

      tokens.Push(Token{ TOKEN_OPEN });

      switch(imp.kind)
      {
      case WASM_KIND_FUNCTION:
        tokens.Push(Token{ TOKEN_FUNC });
        PushFunctionName(tokens, m, (varuint32)i);

        tokens.Push(Token{ TOKEN_OPEN });
        tokens.Push(Token{ TOKEN_TYPE });
        PushNewNameToken(tokens, "t%u", imp.func_desc.type_index);
        tokens.Push(Token{ TOKEN_CLOSE });

        if(imp.func_desc.param_names)
        {
          auto& fn = m.type.functions[imp.func_desc.type_index];
          for(uint64_t j = 0; j < fn.n_params; ++j)
          {
            tokens.Push(Token{ TOKEN_OPEN });
            tokens.Push(Token{ TOKEN_PARAM });
            if(imp.func_desc.param_names[j])
              PushNewNameToken(tokens, imp.func_desc.param_names[j]);
            tokens.Push(Token{ TypeEncodingToken(fn.params[j]) });
            tokens.Push(Token{ TOKEN_CLOSE });
          }

          for(uint64_t j = 0; j < m.type.functions[i].n_returns; ++j)
          {
            tokens.Push(Token{ TOKEN_OPEN });
            tokens.Push(Token{ TOKEN_RESULT });
            tokens.Push(Token{ TypeEncodingToken(fn.returns[j]) });
            tokens.Push(Token{ TOKEN_CLOSE });
          }
        }
        break;
      case WASM_KIND_TABLE:
        tokens.Push(Token{ TOKEN_TABLE });
        tokenize_limits(tokens, imp.table_desc.resizable);
        tokens.Push(Token{ TypeEncodingToken(imp.table_desc.element_type) });
        break;
      case WASM_KIND_MEMORY:
        tokens.Push(Token{ TOKEN_MEMORY });
        tokenize_limits(tokens, imp.mem_desc.limits);
        break;
      case WASM_KIND_GLOBAL:
        tokens.Push(Token{ TOKEN_GLOBAL });
        tokenize_global(tokens, imp.global_desc);
        break;
      }

      tokens.Push(Token{ TOKEN_CLOSE });
      tokens.Push(Token{ TOKEN_CLOSE });
    }

  if(m.knownsections&(1 << WASM_SECTION_TABLE))
    for(uint64_t i = 0; i < m.table.n_tables; ++i)
    {
      tokens.Push(Token{ TOKEN_OPEN });
      tokens.Push(Token{ TOKEN_TABLE });
      PushExportToken(tokens, m, WASM_KIND_TABLE, (varuint32)i);
      tokenize_limits(tokens, m.table.tables[i].resizable);
      tokens.Push(Token{ TypeEncodingToken(m.table.tables[i].element_type) });
      tokens.Push(Token{ TOKEN_CLOSE });
    }

  if(m.knownsections&(1 << WASM_SECTION_MEMORY))
    for(uint64_t i = 0; i < m.memory.n_memories; ++i)
    {
      tokens.Push(Token{ TOKEN_OPEN });
      tokens.Push(Token{ TOKEN_MEMORY });
      PushExportToken(tokens, m, WASM_KIND_MEMORY, (varuint32)i);
      tokenize_limits(tokens, m.table.tables[i].resizable);
      tokens.Push(Token{ TOKEN_CLOSE });
    }

  if(m.knownsections&(1 << WASM_SECTION_GLOBAL))
    for(uint64_t i = 0; i < m.global.n_globals; ++i)
    {
      tokens.Push(Token{ TOKEN_OPEN });
      tokens.Push(Token{ TOKEN_GLOBAL });
      PushExportToken(tokens, m, WASM_KIND_GLOBAL, (varuint32)i);
      tokenize_global(tokens, m.global.globals[i].desc);
      tokens.Push(Token{ TOKEN_OPEN });
      TokenizeInstruction(tokens, m, m.global.globals[i].init, 0, 0);
      tokens.Push(Token{ TOKEN_CLOSE });
      tokens.Push(Token{ TOKEN_CLOSE });
    }

  if(m.knownsections&(1 << WASM_SECTION_START))
  {
    tokens.Push(Token{ TOKEN_OPEN });
    tokens.Push(Token{ TOKEN_START });
    PushFunctionName(tokens, m, m.start);
    tokens.Push(Token{ TOKEN_CLOSE });
  }

  if(m.knownsections&(1 << WASM_SECTION_ELEMENT))
    for(uint64_t i = 0; i < m.element.n_elements; ++i)
    {
      tokens.Push(Token{ TOKEN_OPEN });
      tokens.Push(Token{ TOKEN_ELEM });
      tokens.Push(Token{ TOKEN_INTEGER, 0, m.element.elements[i].index });

      tokens.Push(Token{ TOKEN_OPEN });
      tokens.Push(Token{ TOKEN_OFFSET });
      TokenizeInstruction(tokens, m, m.element.elements[i].offset, 0, 0);
      tokens.Push(Token{ TOKEN_CLOSE });

      for(uint64_t j = 0; j < m.element.elements[i].n_elements; ++j)
        tokens.Push(Token{ TOKEN_INTEGER, 0, m.element.elements[i].elements[j] });

      tokens.Push(Token{ TOKEN_CLOSE });
    }

  for(uint64_t i = 0; i < m.function.n_funcdecl && i < m.code.n_funcbody; ++i)
  {
    tokens.Push(Token{ TOKEN_OPEN });
    tokens.Push(Token{ TOKEN_FUNC });
    PushFunctionName(tokens, m, (varuint32)(i + m.importsection.functions));
    PushExportToken(tokens, m, WASM_KIND_FUNCTION, (varuint32)(i + m.importsection.functions));

    tokens.Push(Token{ TOKEN_OPEN });
    tokens.Push(Token{ TOKEN_TYPE });
    PushNewNameToken(tokens, "t%u", m.function.funcdecl[i]);
    tokens.Push(Token{ TOKEN_CLOSE });

    if(m.function.funcdecl[i] >= m.type.n_functions)
    {
      PushNewNameToken(tokens, "[invalid function index %u]", m.function.funcdecl[i]);
      continue;
    }

    auto& fn = m.type.functions[m.function.funcdecl[i]];
    for(uint64_t j = 0; j < fn.n_params; ++j)
    {
      tokens.Push(Token{ TOKEN_OPEN });
      tokens.Push(Token{ TOKEN_PARAM });
      PushLocalName(tokens, j, m.code.funcbody[i].param_names, fn.n_params, 'p');
      tokens.Push(Token{ TypeEncodingToken(fn.params[j]) });
      tokens.Push(Token{ TOKEN_CLOSE });
    }

    for(uint64_t j = 0; j < fn.n_returns; ++j)
    {
      tokens.Push(Token{ TOKEN_OPEN });
      tokens.Push(Token{ TOKEN_RESULT });
      tokens.Push(Token{ TypeEncodingToken(fn.returns[j]) });
      tokens.Push(Token{ TOKEN_CLOSE });
    }

    for(uint64_t j = 0; j < m.code.funcbody[i].n_locals; ++j)
    {
      tokens.Push(Token{ TOKEN_OPEN });
      tokens.Push(Token{ TOKEN_LOCAL });
      PushLocalName(tokens, j, m.code.funcbody[i].local_names, m.code.funcbody[i].n_locals, 'l');
      tokens.Push(Token{ TypeEncodingToken(m.code.funcbody[i].locals[j]) });
      tokens.Push(Token{ TOKEN_CLOSE });
    }

    for(uint64_t j = 0; j < m.code.funcbody[i].n_body; ++j)
    {
      TokenizeInstruction(tokens, m, m.code.funcbody[i].body[j], &m.code.funcbody[i], &fn);
    }
    tokens.Push(Token{ TOKEN_CLOSE });
  }

  if(m.knownsections&(1 << WASM_SECTION_DATA))
    for(uint64_t i = 0; i < m.data.n_data; ++i)
    {
      tokens.Push(Token{ TOKEN_OPEN });
      tokens.Push(Token{ TOKEN_DATA });
      tokens.Push(Token{ TOKEN_INTEGER, 0, m.data.data[i].index });

      tokens.Push(Token{ TOKEN_OPEN });
      tokens.Push(Token{ TOKEN_OFFSET });
      TokenizeInstruction(tokens, m, m.data.data[i].offset, 0, 0);
      tokens.Push(Token{ TOKEN_CLOSE });

      tokens.Push(Token{ TOKEN_STRING, m.data.data[i].data.str(), m.data.data[i].data.size() });
      tokens.Push(Token{ TOKEN_CLOSE });
    }

  tokens.Push(Token{ TOKEN_CLOSE });
}

void innative::wat::WriteTokens(Queue<Token> tokens, std::ostream& out)
{
  int depth = 0;
  int stack = 0;
  for(size_t i = 0; i < tokens.Size(); ++i)
  {
    if(i > 0 && tokens[i - 1].id != TOKEN_OPEN && tokens[i].id != TOKEN_CLOSE)
      out << ' ';

    switch(tokens[i].id)
    {
    case TOKEN_RANGE_ERROR:
      out << "[RANGE ERROR]";
      break;
    case TOKEN_NONE:
      if(tokens[i].pos)
      {
        out << "[UNKNOWN TOKEN:";
        out.write(tokens[i].pos, tokens[i].len);
        out << ']';
      }
      else
        out << "[UNKNOWN TOKEN]";
      break;
    case TOKEN_NUMBER:
      out.write(tokens[i].pos, tokens[i].len);
      break;
    case TOKEN_STRING:
      out << '"';
      out.write(tokens[i].pos, tokens[i].len); // TODO: encode non-ascii to make it more portable
      out << '"';
      break;
    case TOKEN_NAME:
      out << '$';
      out.write(tokens[i].pos, tokens[i].len);
      break;
    case TOKEN_OPERATOR:
      if(!stack && tokens[i].i == OP_end)
        break; // Skip the final end in the function
      switch(tokens[i].i)
      {
      case OP_end:
        --stack;
        break;
      case OP_block:
      case OP_if:
      case OP_loop:
        ++stack;
        break;
      }

      if(depth == 2)
      {
        out << "\n    ";
        for(int i = 0; i < stack * 2; ++i)
          out.put(' ');
      }
      if(tokens[i].i < OPNAMECOUNT)
        out << OPNAMES[tokens[i].i];
      else
        out << "[UNKNOWN OPERATOR: " << tokens[i].i << "]";
      break;
    case TOKEN_COMMENT:
      out << "(;";
      out.write(tokens[i].pos, tokens[i].len);
      out << ";)";
      break;
    case TOKEN_INTEGER:
      out << std::to_string(tokens[i].i);
      break;
    case TOKEN_FLOAT:
      out << std::to_string(tokens[i].f);
      break;
    case TOKEN_OPEN:
      ++depth;
      if(depth == 2)
        out << "\n  ";
      out << GetTokenString(tokens[i].id);
      break;
    case TOKEN_CLOSE:
      --depth;
    default:
      out << GetTokenString(tokens[i].id);
      break;
    }
  }

  out << "\n";
}