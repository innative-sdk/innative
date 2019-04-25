// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "serialize.h"
#include <stdarg.h>
#include <ostream>

using namespace innative;
using namespace utility;
using namespace wat;

WatTokens innative::wat::TypeEncodingToken(varsint7 type_encoding)
{
  switch(type_encoding)
  {
  case TE_i32:return TOKEN_i32;
  case TE_i64:return TOKEN_i64;
  case TE_f32:return TOKEN_f32;
  case TE_f64:return TOKEN_f64;
  case TE_funcref:return TOKEN_FUNCREF;
  case TE_func:return TOKEN_FUNC;
  case TE_void:return TOKEN_NONE;
  }
  return TOKEN_NONE;
}

void innative::wat::PushNewNameToken(const Environment& env, Queue<WatToken>& tokens, const char* format, ...)
{
  va_list args;
  va_start(args, format);
  size_t len = vsnprintf(0, 0, format, args);
  va_end(args);

  char* s = tmalloc<char>(env, len + 1);
  va_start(args, format);
  vsnprintf(s, len + 1, format, args);
  va_end(args);
  tokens.Push(WatToken{ TOKEN_NAME, s, 0, 0, (int64_t)len });

}

void innative::wat::PushFunctionName(const Environment& env, Queue<WatToken>& tokens, const Module& m, varuint32 index)
{
  Identifier* name = 0;
  if(index < m.importsection.functions)
    name = &m.importsection.imports[index].func_desc.debug.name;
  else if(index - m.importsection.functions < m.code.n_funcbody)
    name = &m.code.funcbody[index - m.importsection.functions].debug.name;

  if(!name || !name->size())
    PushNewNameToken(env, tokens, "f%u", index);
  else
    PushIdentifierToken(tokens, *name, TOKEN_NAME);
}

void innative::wat::PushIdentifierToken(Queue<WatToken>& tokens, const ByteArray& id, WatTokens token)
{
  tokens.Push(WatToken{ token, id.str(), 0, 0, id.size() });
}

void innative::wat::PushLocalName(const Environment& env, Queue<WatToken>& tokens, varuint32 index, const DebugInfo* names, varuint32 num, char prefix)
{
  if(index < num && names && names[index].name.size() != 0)
    PushIdentifierToken(tokens, names[index].name, TOKEN_NAME);
  else
    PushNewNameToken(env, tokens, "%c%u", prefix, index);
}

void innative::wat::TokenizeInstruction(const Environment& env, Queue<WatToken>& tokens, const Module& m, const Instruction& ins, const FunctionBody* body, const FunctionType* ftype)
{
  if(ins.opcode >= OPNAMES.size())
  {
    tokens.Push(WatToken{ TOKEN_NONE });
    return;
  }

  tokens.Push(WatToken{ TOKEN_OPERATOR, 0, 0, 0, ins.opcode });

  switch(ins.opcode)
  {
  case OP_local_get:
  case OP_local_set:
  case OP_local_tee:
    if(body && ftype)
    {
      if(ins.immediates[0]._varuint32 < ftype->n_params)
        PushLocalName(env, tokens, ins.immediates[0]._varuint32, body->param_names, ftype->n_params, 'p');
      else
        PushLocalName(env, tokens, ins.immediates[0]._varuint32 - ftype->n_params, body->local_names, body->n_locals, 'l');
      break;
    } // If this isn't actually a function, just write out the integer by falling through
  case OP_global_get:
  case OP_global_set:
  case OP_br:
  case OP_br_if:
    tokens.Push(WatToken{ TOKEN_INTEGER, 0, 0, 0, ins.immediates[0]._varuint32 });
    break;
  case OP_i32_const:
    tokens.Push(WatToken{ TOKEN_INTEGER, 0, 0, 0, ins.immediates[0]._varsint32 });
    break;
  case OP_i64_const:
    tokens.Push(WatToken{ TOKEN_INTEGER, 0, 0, 0, ins.immediates[0]._varsint64 });
    break;
  case OP_f32_const:
    tokens.Push(WatToken{ TOKEN_FLOAT, 0 });
    tokens.Back().f = ins.immediates[0]._float32;
    break;
  case OP_f64_const:
    tokens.Push(WatToken{ TOKEN_FLOAT, 0 });
    tokens.Back().f = ins.immediates[0]._float64;
    break;
  case OP_br_table:
    for(uint64_t i = 0; i < ins.immediates[0].n_table; ++i)
    {
      tokens.Push(WatToken{ TOKEN_INTEGER, 0, 0, 0, ins.immediates[0].table[i] });
    }

    tokens.Push(WatToken{ TOKEN_INTEGER, 0, 0, 0, ins.immediates[1]._varuint32 });
    break;
  case OP_call:
    PushFunctionName(env, tokens, m, ins.immediates[0]._varuint32);
    break;
  case OP_call_indirect:
    tokens.Push(WatToken{ TOKEN_INTEGER, 0, 0, 0, ins.immediates[0]._varuint32 });
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
      tokens.Push(WatToken{ TOKEN_ALIGN });
      tokens.Push(WatToken{ TOKEN_INTEGER, 0, 0, 0, (1LL << (int64_t)ins.immediates[0]._varuint32) });
    }

    if(ins.immediates[1]._varuptr != 0)
    {
      tokens.Push(WatToken{ TOKEN_OFFSET });
      tokens.Push(WatToken{ TOKEN_INTEGER, 0, 0, 0,  (1LL << (int64_t)ins.immediates[1]._varuptr) });
    }
    break;
  }
}

void innative::wat::PushExportToken(Queue<WatToken>& tokens, const Module& m, varuint7 kind, varuint32 index, bool outside)
{
  if(m.knownsections&(1 << WASM_SECTION_EXPORT))
    for(uint64_t i = 0; i < m.exportsection.n_exports; ++i)
    {
      if(m.exportsection.exports[i].kind == kind && m.exportsection.exports[i].index == index)
      {
        tokens.Push(WatToken{ TOKEN_OPEN });
        tokens.Push(WatToken{ TOKEN_EXPORT });
        PushIdentifierToken(tokens, m.exportsection.exports[i].name);

        if(outside)
        {
          tokens.Push(WatToken{ TOKEN_OPEN });
          tokens.Push(WatToken{ WatTokenID(TOKEN_FUNC + kind) });
          tokens.Push(WatToken{ TOKEN_INTEGER, 0, 0, 0, index });
          tokens.Push(WatToken{ TOKEN_CLOSE }); // do NOT break here, because you can export a function under multiple names
        }

        tokens.Push(WatToken{ TOKEN_CLOSE }); // do NOT break here, because you can export a function under multiple names
      }
    }
}

void innative::wat::TokenizeModule(const Environment& env, Queue<WatToken>& tokens, const Module& m)
{
  tokens.Push(WatToken{ TOKEN_OPEN });
  tokens.Push(WatToken{ TOKEN_MODULE });

  if(m.name.size())
    tokens.Push(WatToken{ TOKEN_NAME, m.name.str(), 0, 0, m.name.size() });

  if(m.knownsections&(1 << WASM_SECTION_TYPE))
    for(uint64_t i = 0; i < m.type.n_functions; ++i)
    {
      tokens.Push(WatToken{ TOKEN_OPEN });
      tokens.Push(WatToken{ TOKEN_TYPE });

      PushNewNameToken(env, tokens, "t%u", (varuint32)i);

      tokens.Push(WatToken{ TOKEN_OPEN });
      tokens.Push(WatToken{ TypeEncodingToken(m.type.functions[i].form) });

      for(uint64_t j = 0; j < m.type.functions[i].n_params; ++j)
      {
        tokens.Push(WatToken{ TOKEN_OPEN });
        tokens.Push(WatToken{ TOKEN_PARAM });
        tokens.Push(WatToken{ TypeEncodingToken(m.type.functions[i].params[j]) });
        tokens.Push(WatToken{ TOKEN_CLOSE });
      }

      for(uint64_t j = 0; j < m.type.functions[i].n_returns; ++j)
      {
        tokens.Push(WatToken{ TOKEN_OPEN });
        tokens.Push(WatToken{ TOKEN_RESULT });
        tokens.Push(WatToken{ TypeEncodingToken(m.type.functions[i].returns[j]) });
        tokens.Push(WatToken{ TOKEN_CLOSE });
      }

      tokens.Push(WatToken{ TOKEN_CLOSE });
      tokens.Push(WatToken{ TOKEN_CLOSE });
    }

  auto tokenize_limits = [](Queue<WatToken>& t, const ResizableLimits& limits) {
    t.Push(WatToken{ TOKEN_INTEGER, 0, 0, 0, limits.minimum });
    if(limits.flags&WASM_LIMIT_HAS_MAXIMUM)
      t.Push(WatToken{ TOKEN_INTEGER, 0, 0, 0, limits.maximum });
  };

  auto tokenize_global = [](Queue<WatToken>& t, const GlobalDesc& global) {
    if(global.mutability)
    {
      t.Push(WatToken{ TOKEN_OPEN });
      t.Push(WatToken{ TOKEN_MUT });
      t.Push(WatToken{ TypeEncodingToken(global.type) });
      t.Push(WatToken{ TOKEN_CLOSE });
    }
    else
      t.Push(WatToken{ TypeEncodingToken(global.type) });
  };

  if(m.knownsections&(1 << WASM_SECTION_IMPORT))
    for(uint64_t i = 0; i < m.importsection.n_import; ++i)
    {
      auto& imp = m.importsection.imports[i];
      tokens.Push(WatToken{ TOKEN_OPEN });
      tokens.Push(WatToken{ TOKEN_IMPORT });
      PushIdentifierToken(tokens, imp.module_name);
      PushIdentifierToken(tokens, imp.export_name);

      tokens.Push(WatToken{ TOKEN_OPEN });
      varuint32 index = i;

      switch(imp.kind)
      {
      case WASM_KIND_FUNCTION:
        tokens.Push(WatToken{ TOKEN_FUNC });
        PushFunctionName(env, tokens, m, (varuint32)i);

        tokens.Push(WatToken{ TOKEN_OPEN });
        tokens.Push(WatToken{ TOKEN_TYPE });
        PushNewNameToken(env, tokens, "t%u", imp.func_desc.type_index);
        tokens.Push(WatToken{ TOKEN_CLOSE });

        if(imp.func_desc.param_names)
        {
          auto& fn = m.type.functions[imp.func_desc.type_index];
          for(uint64_t j = 0; j < fn.n_params; ++j)
          {
            tokens.Push(WatToken{ TOKEN_OPEN });
            tokens.Push(WatToken{ TOKEN_PARAM });
            if(imp.func_desc.param_names[j].name.size() > 0)
              PushIdentifierToken(tokens, imp.func_desc.param_names[j].name, TOKEN_NAME);
            tokens.Push(WatToken{ TypeEncodingToken(fn.params[j]) });
            tokens.Push(WatToken{ TOKEN_CLOSE });
          }

          for(uint64_t j = 0; j < m.type.functions[imp.func_desc.type_index].n_returns; ++j)
          {
            tokens.Push(WatToken{ TOKEN_OPEN });
            tokens.Push(WatToken{ TOKEN_RESULT });
            tokens.Push(WatToken{ TypeEncodingToken(fn.returns[j]) });
            tokens.Push(WatToken{ TOKEN_CLOSE });
          }
        }
        break;
      case WASM_KIND_TABLE:
        tokens.Push(WatToken{ TOKEN_TABLE });
        tokenize_limits(tokens, imp.table_desc.resizable);
        tokens.Push(WatToken{ TypeEncodingToken(imp.table_desc.element_type) });
        index -= m.importsection.functions;
        break;
      case WASM_KIND_MEMORY:
        tokens.Push(WatToken{ TOKEN_MEMORY });
        tokenize_limits(tokens, imp.mem_desc.limits);
        index -= m.importsection.tables;
        break;
      case WASM_KIND_GLOBAL:
        tokens.Push(WatToken{ TOKEN_GLOBAL });
        tokenize_global(tokens, imp.global_desc);
        index -= m.importsection.memories;
        break;
      }

      tokens.Push(WatToken{ TOKEN_CLOSE });
      tokens.Push(WatToken{ TOKEN_CLOSE });
      PushExportToken(tokens, m, imp.kind, index, true);
    }

  if(m.knownsections&(1 << WASM_SECTION_TABLE))
    for(uint64_t i = 0; i < m.table.n_tables; ++i)
    {
      tokens.Push(WatToken{ TOKEN_OPEN });
      tokens.Push(WatToken{ TOKEN_TABLE });
      PushExportToken(tokens, m, WASM_KIND_TABLE, (varuint32)i + m.importsection.tables - m.importsection.functions, false);
      tokenize_limits(tokens, m.table.tables[i].resizable);
      tokens.Push(WatToken{ TypeEncodingToken(m.table.tables[i].element_type) });
      tokens.Push(WatToken{ TOKEN_CLOSE });
    }

  if(m.knownsections&(1 << WASM_SECTION_MEMORY))
    for(uint64_t i = 0; i < m.memory.n_memories; ++i)
    {
      tokens.Push(WatToken{ TOKEN_OPEN });
      tokens.Push(WatToken{ TOKEN_MEMORY });
      PushExportToken(tokens, m, WASM_KIND_MEMORY, (varuint32)i + m.importsection.memories - m.importsection.tables, false);
      tokenize_limits(tokens, m.memory.memories[i].limits);
      tokens.Push(WatToken{ TOKEN_CLOSE });
    }

  if(m.knownsections&(1 << WASM_SECTION_GLOBAL))
    for(uint64_t i = 0; i < m.global.n_globals; ++i)
    {
      tokens.Push(WatToken{ TOKEN_OPEN });
      tokens.Push(WatToken{ TOKEN_GLOBAL });
      PushExportToken(tokens, m, WASM_KIND_GLOBAL, (varuint32)i + m.importsection.globals - m.importsection.memories, false);
      tokenize_global(tokens, m.global.globals[i].desc);
      tokens.Push(WatToken{ TOKEN_OPEN });
      TokenizeInstruction(env, tokens, m, m.global.globals[i].init, 0, 0);
      tokens.Push(WatToken{ TOKEN_CLOSE });
      tokens.Push(WatToken{ TOKEN_CLOSE });
    }

  if(m.knownsections&(1 << WASM_SECTION_START))
  {
    tokens.Push(WatToken{ TOKEN_OPEN });
    tokens.Push(WatToken{ TOKEN_START });
    PushFunctionName(env, tokens, m, m.start);
    tokens.Push(WatToken{ TOKEN_CLOSE });
  }

  if(m.knownsections&(1 << WASM_SECTION_ELEMENT))
    for(uint64_t i = 0; i < m.element.n_elements; ++i)
    {
      tokens.Push(WatToken{ TOKEN_OPEN });
      tokens.Push(WatToken{ TOKEN_ELEM });
      tokens.Push(WatToken{ TOKEN_INTEGER, 0, 0, 0, m.element.elements[i].index });

      tokens.Push(WatToken{ TOKEN_OPEN });
      tokens.Push(WatToken{ TOKEN_OFFSET });
      TokenizeInstruction(env, tokens, m, m.element.elements[i].offset, 0, 0);
      tokens.Push(WatToken{ TOKEN_CLOSE });

      for(uint64_t j = 0; j < m.element.elements[i].n_elements; ++j)
        tokens.Push(WatToken{ TOKEN_INTEGER, 0, 0, 0, m.element.elements[i].elements[j] });

      tokens.Push(WatToken{ TOKEN_CLOSE });
    }

  for(uint64_t i = 0; i < m.function.n_funcdecl && i < m.code.n_funcbody; ++i)
  {
    tokens.Push(WatToken{ TOKEN_OPEN });
    tokens.Push(WatToken{ TOKEN_FUNC });
    PushFunctionName(env, tokens, m, (varuint32)(i + m.importsection.functions));
    PushExportToken(tokens, m, WASM_KIND_FUNCTION, (varuint32)(i + m.importsection.functions), false);

    tokens.Push(WatToken{ TOKEN_OPEN });
    tokens.Push(WatToken{ TOKEN_TYPE });
    PushNewNameToken(env, tokens, "t%u", m.function.funcdecl[i]);
    tokens.Push(WatToken{ TOKEN_CLOSE });

    if(m.function.funcdecl[i] >= m.type.n_functions)
    {
      PushNewNameToken(env, tokens, "[invalid function index %u]", m.function.funcdecl[i]);
      continue;
    }

    auto& fn = m.type.functions[m.function.funcdecl[i]];
    for(uint64_t j = 0; j < fn.n_params; ++j)
    {
      tokens.Push(WatToken{ TOKEN_OPEN });
      tokens.Push(WatToken{ TOKEN_PARAM });
      PushLocalName(env, tokens, (varuint32)j, m.code.funcbody[i].param_names, fn.n_params, 'p');
      tokens.Push(WatToken{ TypeEncodingToken(fn.params[j]) });
      tokens.Push(WatToken{ TOKEN_CLOSE });
    }

    for(uint64_t j = 0; j < fn.n_returns; ++j)
    {
      tokens.Push(WatToken{ TOKEN_OPEN });
      tokens.Push(WatToken{ TOKEN_RESULT });
      tokens.Push(WatToken{ TypeEncodingToken(fn.returns[j]) });
      tokens.Push(WatToken{ TOKEN_CLOSE });
    }

    for(uint64_t j = 0; j < m.code.funcbody[i].n_locals; ++j)
    {
      tokens.Push(WatToken{ TOKEN_OPEN });
      tokens.Push(WatToken{ TOKEN_LOCAL });
      PushLocalName(env, tokens, (varuint32)j, m.code.funcbody[i].local_names, m.code.funcbody[i].n_locals, 'l');
      tokens.Push(WatToken{ TypeEncodingToken(m.code.funcbody[i].locals[j]) });
      tokens.Push(WatToken{ TOKEN_CLOSE });
    }

    for(uint64_t j = 0; j < m.code.funcbody[i].n_body; ++j)
    {
      TokenizeInstruction(env, tokens, m, m.code.funcbody[i].body[j], &m.code.funcbody[i], &fn);
    }
    tokens.Push(WatToken{ TOKEN_CLOSE });
  }

  if(m.knownsections&(1 << WASM_SECTION_DATA))
    for(uint64_t i = 0; i < m.data.n_data; ++i)
    {
      tokens.Push(WatToken{ TOKEN_OPEN });
      tokens.Push(WatToken{ TOKEN_DATA });
      tokens.Push(WatToken{ TOKEN_INTEGER, 0, 0, 0, m.data.data[i].index });

      tokens.Push(WatToken{ TOKEN_OPEN });
      tokens.Push(WatToken{ TOKEN_OFFSET });
      TokenizeInstruction(env, tokens, m, m.data.data[i].offset, 0, 0);
      tokens.Push(WatToken{ TOKEN_CLOSE });

      tokens.Push(WatToken{ TOKEN_STRING, m.data.data[i].data.str(), 0, 0, m.data.data[i].data.size() });
      tokens.Push(WatToken{ TOKEN_CLOSE });
    }

  tokens.Push(WatToken{ TOKEN_CLOSE });
}

void innative::wat::WriteTokens(Queue<WatToken> tokens, std::ostream& out)
{
  size_t depth = 0;
  size_t stack = 0;
  size_t line = 0;

  for(size_t i = 0; i < tokens.Size(); ++i)
  {
    assert(depth != (size_t)~0);
    assert(stack != (size_t)~0);

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
      if(!stack && tokens[i].u == OP_end)
        break; // Skip the final end in the function
      switch(tokens[i].u)
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
        ++line;
        out << "\n    ";
        for(size_t k = 0; k < stack * 2; ++k)
          out.put(' ');
      }
      if(tokens[i].u < OPNAMES.size())
        out << OPNAMES[tokens[i].u];
      else
        out << "[UNKNOWN OPERATOR: " << tokens[i].u << "]";
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
      {
        ++line;
        out << "\n  ";
      }
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