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
  case TE_i32: return WatTokens::i32;
  case TE_i64: return WatTokens::i64;
  case TE_f32: return WatTokens::f32;
  case TE_f64: return WatTokens::f64;
  case TE_funcref: return WatTokens::FUNCREF;
  case TE_func: return WatTokens::FUNC;
  case TE_void: return WatTokens::NONE;
  }
  return WatTokens::NONE;
}

void innative::wat::PushNewNameToken(const Environment& env, Queue<WatToken>& tokens, const char* format, ...)
{
  va_list args;
  va_start(args, format);
  size_t len = vsnprintf(0, 0, format, args);
  va_end(args);

  char* s = tmalloc<char>(env, len + 1);
  if(!s) // Out of memory, abort
    abort();
  va_start(args, format);
  vsnprintf(s, len + 1, format, args);
  va_end(args);
  tokens.Push(WatToken{ WatTokens::NAME, s, 0, 0, (int64_t)len });
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
    PushIdentifierToken(tokens, *name, WatTokens::NAME);
}

void innative::wat::PushIdentifierToken(Queue<WatToken>& tokens, const ByteArray& id, WatTokens token)
{
  tokens.Push(WatToken{ token, id.str(), 0, 0, id.size() });
}

void innative::wat::PushParamName(const Environment& env, Queue<WatToken>& tokens, varuint32 index, const DebugInfo* names,
                                  varuint32 num, char prefix)
{
  if(index < num && names && names[index].name.size() != 0)
    PushIdentifierToken(tokens, names[index].name, WatTokens::NAME);
  else
    PushNewNameToken(env, tokens, "%c%u", prefix, index);
}

// We can only use the local name if the index is an exact match - names can't be assigned to groups of locals.
void innative::wat::PushLocalName(const Environment& env, Queue<WatToken>& tokens, varuint32 index,
                                  const FunctionBody* body)
{
  varuint32 count        = 0;
  const Identifier* name = nullptr;
  for(varuint32 i = 0; i < body->n_locals; ++i)
  {
    if(count > index)
      break;
    if(count == index)
      name = &body->locals[i].debug.name;
    count += body->locals[i].count;
  }

  if(name && name->size() != 0)
    PushIdentifierToken(tokens, *name, WatTokens::NAME);
  else
    PushNewNameToken(env, tokens, "%c%u", 'l', index);
}

void innative::wat::TokenizeInstruction(const Environment& env, Queue<WatToken>& tokens, const Module& m,
                                        const Instruction& ins, const FunctionBody* body, const FunctionDesc* desc)
{
  if(ins.opcode >= OPNAMES.size())
  {
    tokens.Push(WatToken{ WatTokens::NONE });
    return;
  }

  tokens.Push(WatToken{ WatTokens::OPERATOR, 0, 0, 0, ins.opcode });

  switch(ins.opcode)
  {
  case OP_local_get:
  case OP_local_set:
  case OP_local_tee:
    if(body && desc)
    {
      auto& ftype = m.type.functypes[desc->type_index];
      if(ins.immediates[0]._varuint32 < ftype.n_params)
        PushParamName(env, tokens, ins.immediates[0]._varuint32, desc->param_debug, ftype.n_params, 'p');
      else
        PushLocalName(env, tokens, ins.immediates[0]._varuint32, body);
      break;
    } // If this isn't actually a function, just write out the integer by falling through
  case OP_global_get:
  case OP_global_set:
  case OP_br:
  case OP_br_if: tokens.Push(WatToken{ WatTokens::INTEGER, 0, 0, 0, ins.immediates[0]._varuint32 }); break;
  case OP_i32_const: tokens.Push(WatToken{ WatTokens::INTEGER, 0, 0, 0, ins.immediates[0]._varsint32 }); break;
  case OP_i64_const: tokens.Push(WatToken{ WatTokens::INTEGER, 0, 0, 0, ins.immediates[0]._varsint64 }); break;
  case OP_f32_const:
    tokens.Push(WatToken{ WatTokens::FLOAT, 0 });
    tokens.Back().f = ins.immediates[0]._float32;
    break;
  case OP_f64_const:
    tokens.Push(WatToken{ WatTokens::FLOAT, 0 });
    tokens.Back().f = ins.immediates[0]._float64;
    break;
  case OP_br_table:
    for(varuint32 i = 0; i < ins.immediates[0].n_table; ++i)
    {
      tokens.Push(WatToken{ WatTokens::INTEGER, 0, 0, 0, ins.immediates[0].table[i] });
    }

    tokens.Push(WatToken{ WatTokens::INTEGER, 0, 0, 0, ins.immediates[1]._varuint32 });
    break;
  case OP_call: PushFunctionName(env, tokens, m, ins.immediates[0]._varuint32); break;
  case OP_call_indirect: tokens.Push(WatToken{ WatTokens::INTEGER, 0, 0, 0, ins.immediates[0]._varuint32 }); break;
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
      tokens.Push(WatToken{ WatTokens::ALIGN });
      tokens.Push(WatToken{ WatTokens::INTEGER, 0, 0, 0, (1LL << (int64_t)ins.immediates[0]._varuint32) });
    }

    if(ins.immediates[1]._varuptr != 0)
    {
      tokens.Push(WatToken{ WatTokens::OFFSET });
      tokens.Push(WatToken{ WatTokens::INTEGER, 0, 0, 0, (1LL << (int64_t)ins.immediates[1]._varuptr) });
    }
    break;
  }
}

void innative::wat::PushExportToken(Queue<WatToken>& tokens, const Module& m, varuint7 kind, varuint32 index, bool outside)
{
  if(m.knownsections & (1 << WASM_SECTION_EXPORT))
    for(varuint32 i = 0; i < m.exportsection.n_exports; ++i)
    {
      if(m.exportsection.exports[i].kind == kind && m.exportsection.exports[i].index == index)
      {
        tokens.Push(WatToken{ WatTokens::OPEN });
        tokens.Push(WatToken{ WatTokens::EXPORT });
        PushIdentifierToken(tokens, m.exportsection.exports[i].name);

        if(outside)
        {
          tokens.Push(WatToken{ WatTokens::OPEN });
          tokens.Push(WatToken{ WatTokens((int)WatTokens::FUNC + kind) });
          tokens.Push(WatToken{ WatTokens::INTEGER, 0, 0, 0, index });
          tokens.Push(
            WatToken{ WatTokens::CLOSE }); // do NOT break here, because you can export a function under multiple names
        }

        tokens.Push(
          WatToken{ WatTokens::CLOSE }); // do NOT break here, because you can export a function under multiple names
      }
    }
}

void innative::wat::TokenizeModule(const Environment& env, Queue<WatToken>& tokens, const Module& m)
{
  tokens.Push(WatToken{ WatTokens::OPEN });
  tokens.Push(WatToken{ WatTokens::MODULE });

  if(m.name.size())
    tokens.Push(WatToken{ WatTokens::NAME, m.name.str(), 0, 0, m.name.size() });

  if(m.knownsections & (1 << WASM_SECTION_TYPE))
    for(varuint32 i = 0; i < m.type.n_functypes; ++i)
    {
      tokens.Push(WatToken{ WatTokens::OPEN });
      tokens.Push(WatToken{ WatTokens::TYPE });

      PushNewNameToken(env, tokens, "t%u", i);

      tokens.Push(WatToken{ WatTokens::OPEN });
      tokens.Push(WatToken{ TypeEncodingToken(m.type.functypes[i].form) });

      for(varuint32 j = 0; j < m.type.functypes[i].n_params; ++j)
      {
        tokens.Push(WatToken{ WatTokens::OPEN });
        tokens.Push(WatToken{ WatTokens::PARAM });
        tokens.Push(WatToken{ TypeEncodingToken(m.type.functypes[i].params[j]) });
        tokens.Push(WatToken{ WatTokens::CLOSE });
      }

      for(varuint32 j = 0; j < m.type.functypes[i].n_returns; ++j)
      {
        tokens.Push(WatToken{ WatTokens::OPEN });
        tokens.Push(WatToken{ WatTokens::RESULT });
        tokens.Push(WatToken{ TypeEncodingToken(m.type.functypes[i].returns[j]) });
        tokens.Push(WatToken{ WatTokens::CLOSE });
      }

      tokens.Push(WatToken{ WatTokens::CLOSE });
      tokens.Push(WatToken{ WatTokens::CLOSE });
    }

  auto tokenize_limits = [](Queue<WatToken>& t, const ResizableLimits& limits) {
    t.Push(WatToken{ WatTokens::INTEGER, 0, 0, 0, limits.minimum });
    if(limits.flags & WASM_LIMIT_HAS_MAXIMUM)
      t.Push(WatToken{ WatTokens::INTEGER, 0, 0, 0, limits.maximum });
  };

  auto tokenize_global = [](Queue<WatToken>& t, const GlobalDesc& global) {
    if(global.mutability)
    {
      t.Push(WatToken{ WatTokens::OPEN });
      t.Push(WatToken{ WatTokens::MUT });
      t.Push(WatToken{ TypeEncodingToken(global.type) });
      t.Push(WatToken{ WatTokens::CLOSE });
    }
    else
      t.Push(WatToken{ TypeEncodingToken(global.type) });
  };

  if(m.knownsections & (1 << WASM_SECTION_IMPORT))
    for(varuint32 i = 0; i < m.importsection.n_import; ++i)
    {
      auto& imp = m.importsection.imports[i];
      tokens.Push(WatToken{ WatTokens::OPEN });
      tokens.Push(WatToken{ WatTokens::IMPORT });
      PushIdentifierToken(tokens, imp.module_name);
      PushIdentifierToken(tokens, imp.export_name);

      tokens.Push(WatToken{ WatTokens::OPEN });
      varuint32 index = i;

      switch(imp.kind)
      {
      case WASM_KIND_FUNCTION:
        tokens.Push(WatToken{ WatTokens::FUNC });
        PushFunctionName(env, tokens, m, i);

        tokens.Push(WatToken{ WatTokens::OPEN });
        tokens.Push(WatToken{ WatTokens::TYPE });
        PushNewNameToken(env, tokens, "t%u", imp.func_desc.type_index);
        tokens.Push(WatToken{ WatTokens::CLOSE });

        if(imp.func_desc.param_debug)
        {
          auto& fn = m.type.functypes[imp.func_desc.type_index];
          for(varuint32 j = 0; j < fn.n_params; ++j)
          {
            tokens.Push(WatToken{ WatTokens::OPEN });
            tokens.Push(WatToken{ WatTokens::PARAM });
            if(imp.func_desc.param_debug[j].name.size() > 0)
              PushIdentifierToken(tokens, imp.func_desc.param_debug[j].name, WatTokens::NAME);
            tokens.Push(WatToken{ TypeEncodingToken(fn.params[j]) });
            tokens.Push(WatToken{ WatTokens::CLOSE });
          }

          for(varuint32 j = 0; j < m.type.functypes[imp.func_desc.type_index].n_returns; ++j)
          {
            tokens.Push(WatToken{ WatTokens::OPEN });
            tokens.Push(WatToken{ WatTokens::RESULT });
            tokens.Push(WatToken{ TypeEncodingToken(fn.returns[j]) });
            tokens.Push(WatToken{ WatTokens::CLOSE });
          }
        }
        break;
      case WASM_KIND_TABLE:
        tokens.Push(WatToken{ WatTokens::TABLE });
        tokenize_limits(tokens, imp.table_desc.resizable);
        tokens.Push(WatToken{ TypeEncodingToken(imp.table_desc.element_type) });
        index -= m.importsection.functions;
        break;
      case WASM_KIND_MEMORY:
        tokens.Push(WatToken{ WatTokens::MEMORY });
        tokenize_limits(tokens, imp.mem_desc.limits);
        index -= m.importsection.tables;
        break;
      case WASM_KIND_GLOBAL:
        tokens.Push(WatToken{ WatTokens::GLOBAL });
        tokenize_global(tokens, imp.global_desc);
        index -= m.importsection.memories;
        break;
      }

      tokens.Push(WatToken{ WatTokens::CLOSE });
      tokens.Push(WatToken{ WatTokens::CLOSE });
      PushExportToken(tokens, m, imp.kind, index, true);
    }

  if(m.knownsections & (1 << WASM_SECTION_TABLE))
    for(varuint32 i = 0; i < m.table.n_tables; ++i)
    {
      tokens.Push(WatToken{ WatTokens::OPEN });
      tokens.Push(WatToken{ WatTokens::TABLE });
      PushExportToken(tokens, m, WASM_KIND_TABLE, i + m.importsection.tables - m.importsection.functions, false);
      tokenize_limits(tokens, m.table.tables[i].resizable);
      tokens.Push(WatToken{ TypeEncodingToken(m.table.tables[i].element_type) });
      tokens.Push(WatToken{ WatTokens::CLOSE });
    }

  if(m.knownsections & (1 << WASM_SECTION_MEMORY))
    for(varuint32 i = 0; i < m.memory.n_memories; ++i)
    {
      tokens.Push(WatToken{ WatTokens::OPEN });
      tokens.Push(WatToken{ WatTokens::MEMORY });
      PushExportToken(tokens, m, WASM_KIND_MEMORY, i + m.importsection.memories - m.importsection.tables, false);
      tokenize_limits(tokens, m.memory.memories[i].limits);
      tokens.Push(WatToken{ WatTokens::CLOSE });
    }

  if(m.knownsections & (1 << WASM_SECTION_GLOBAL))
    for(varuint32 i = 0; i < m.global.n_globals; ++i)
    {
      tokens.Push(WatToken{ WatTokens::OPEN });
      tokens.Push(WatToken{ WatTokens::GLOBAL });
      PushExportToken(tokens, m, WASM_KIND_GLOBAL, i + m.importsection.globals - m.importsection.memories, false);
      tokenize_global(tokens, m.global.globals[i].desc);
      tokens.Push(WatToken{ WatTokens::OPEN });
      TokenizeInstruction(env, tokens, m, m.global.globals[i].init, 0, 0);
      tokens.Push(WatToken{ WatTokens::CLOSE });
      tokens.Push(WatToken{ WatTokens::CLOSE });
    }

  if(m.knownsections & (1 << WASM_SECTION_START))
  {
    tokens.Push(WatToken{ WatTokens::OPEN });
    tokens.Push(WatToken{ WatTokens::START });
    PushFunctionName(env, tokens, m, m.start);
    tokens.Push(WatToken{ WatTokens::CLOSE });
  }

  if(m.knownsections & (1 << WASM_SECTION_ELEMENT))
    for(varuint32 i = 0; i < m.element.n_elements; ++i)
    {
      tokens.Push(WatToken{ WatTokens::OPEN });
      tokens.Push(WatToken{ WatTokens::ELEM });
      tokens.Push(WatToken{ WatTokens::INTEGER, 0, 0, 0, m.element.elements[i].index });

      tokens.Push(WatToken{ WatTokens::OPEN });
      tokens.Push(WatToken{ WatTokens::OFFSET });
      TokenizeInstruction(env, tokens, m, m.element.elements[i].offset, 0, 0);
      tokens.Push(WatToken{ WatTokens::CLOSE });

      for(varuint32 j = 0; j < m.element.elements[i].n_elements; ++j)
        tokens.Push(WatToken{ WatTokens::INTEGER, 0, 0, 0, m.element.elements[i].elements[j] });

      tokens.Push(WatToken{ WatTokens::CLOSE });
    }

  for(varuint32 i = 0; i < m.function.n_funcdecl && i < m.code.n_funcbody; ++i)
  {
    tokens.Push(WatToken{ WatTokens::OPEN });
    tokens.Push(WatToken{ WatTokens::FUNC });
    PushFunctionName(env, tokens, m, (i + m.importsection.functions));
    PushExportToken(tokens, m, WASM_KIND_FUNCTION, (i + m.importsection.functions), false);

    tokens.Push(WatToken{ WatTokens::OPEN });
    tokens.Push(WatToken{ WatTokens::TYPE });
    PushNewNameToken(env, tokens, "t%u", m.function.funcdecl[i]);
    tokens.Push(WatToken{ WatTokens::CLOSE });

    if(m.function.funcdecl[i].type_index >= m.type.n_functypes)
    {
      PushNewNameToken(env, tokens, "[invalid function index %u]", m.function.funcdecl[i]);
      continue;
    }

    auto& decl = m.function.funcdecl[i];
    auto& fn  = m.type.functypes[decl.type_index];
    for(varuint32 j = 0; j < fn.n_params; ++j)
    {
      tokens.Push(WatToken{ WatTokens::OPEN });
      tokens.Push(WatToken{ WatTokens::PARAM });
      PushParamName(env, tokens, j, decl.param_debug, fn.n_params, 'p');
      tokens.Push(WatToken{ TypeEncodingToken(fn.params[j]) });
      tokens.Push(WatToken{ WatTokens::CLOSE });
    }

    for(varuint32 j = 0; j < fn.n_returns; ++j)
    {
      tokens.Push(WatToken{ WatTokens::OPEN });
      tokens.Push(WatToken{ WatTokens::RESULT });
      tokens.Push(WatToken{ TypeEncodingToken(fn.returns[j]) });
      tokens.Push(WatToken{ WatTokens::CLOSE });
    }

    for(varuint32 j = 0; j < m.code.funcbody[i].local_size; ++j)
    {
      tokens.Push(WatToken{ WatTokens::OPEN });
      tokens.Push(WatToken{ WatTokens::LOCAL });
      PushLocalName(env, tokens, j, &m.code.funcbody[i]);
      tokens.Push(WatToken{ TypeEncodingToken(m.code.funcbody[i].locals[j].type) });
      tokens.Push(WatToken{ WatTokens::CLOSE });
    }

    for(varuint32 j = 0; j < m.code.funcbody[i].n_body; ++j)
    {
      TokenizeInstruction(env, tokens, m, m.code.funcbody[i].body[j], &m.code.funcbody[i], &decl);
    }
    tokens.Push(WatToken{ WatTokens::CLOSE });
  }

  if(m.knownsections & (1 << WASM_SECTION_DATA))
    for(varuint32 i = 0; i < m.data.n_data; ++i)
    {
      tokens.Push(WatToken{ WatTokens::OPEN });
      tokens.Push(WatToken{ WatTokens::DATA });
      tokens.Push(WatToken{ WatTokens::INTEGER, 0, 0, 0, m.data.data[i].index });

      tokens.Push(WatToken{ WatTokens::OPEN });
      tokens.Push(WatToken{ WatTokens::OFFSET });
      TokenizeInstruction(env, tokens, m, m.data.data[i].offset, 0, 0);
      tokens.Push(WatToken{ WatTokens::CLOSE });

      tokens.Push(WatToken{ WatTokens::STRING, m.data.data[i].data.str(), 0, 0, m.data.data[i].data.size() });
      tokens.Push(WatToken{ WatTokens::CLOSE });
    }

  tokens.Push(WatToken{ WatTokens::CLOSE });
}

void innative::wat::WriteTokens(Queue<WatToken> tokens, std::ostream& out)
{
  size_t depth = 0;
  size_t stack = 0;
  size_t line  = 0;

  for(size_t i = 0; i < tokens.Size(); ++i)
  {
    assert(depth != (size_t)~0);
    assert(stack != (size_t)~0);

    if(i > 0 && tokens[i - 1].id != WatTokens::OPEN && tokens[i].id != WatTokens::CLOSE)
      out << ' ';

    switch(tokens[i].id)
    {
    case WatTokens::RANGE_ERROR: out << "[RANGE ERROR]"; break;
    case WatTokens::NONE:
      if(tokens[i].pos)
      {
        out << "[UNKNOWN TOKEN:";
        out.write(tokens[i].pos, tokens[i].len);
        out << ']';
      }
      else
        out << "[UNKNOWN TOKEN]";
      break;
    case WatTokens::NUMBER: out.write(tokens[i].pos, tokens[i].len); break;
    case WatTokens::STRING:
      out << '"';
      out.write(tokens[i].pos, tokens[i].len); // TODO: encode non-ascii to make it more portable
      out << '"';
      break;
    case WatTokens::NAME:
      out << '$';
      out.write(tokens[i].pos, tokens[i].len);
      break;
    case WatTokens::OPERATOR:
      if(!stack && tokens[i].u == OP_end)
        break; // Skip the final end in the function
      switch(tokens[i].u)
      {
      case OP_end: --stack; break;
      case OP_block:
      case OP_if:
      case OP_loop: ++stack; break;
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
    case WatTokens::COMMENT:
      out << "(;";
      out.write(tokens[i].pos, tokens[i].len);
      out << ";)";
      break;
    case WatTokens::INTEGER: out << std::to_string(tokens[i].i); break;
    case WatTokens::FLOAT: out << std::to_string(tokens[i].f); break;
    case WatTokens::OPEN:
      ++depth;
      if(depth == 2)
      {
        ++line;
        out << "\n  ";
      }
      out << GetTokenString(tokens[i].id);
      break;
    case WatTokens::CLOSE: --depth;
    default: out << GetTokenString(tokens[i].id); break;
    }
  }

  out << "\n";
}