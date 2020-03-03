// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "serialize.h"
#include <stdarg.h>
#include <ostream>

using namespace innative;
using namespace utility;
using namespace wat;

Serializer::Serializer(const Environment& _env, Module& _m, std::ostream* out) :
  env(_env), m(_m), _line(0), _lastp(0), _dump(out), _stack(0), _depth(0), _localbreak(false)
{}

WatTokens Serializer::TypeEncodingToken(varsint7 type_encoding)
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

void Serializer::PushNewNameToken(const char* format, ...)
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

void Serializer::PushFunctionName(varuint32 index)
{
  Identifier* name = 0;
  if(index < m.importsection.functions)
    name = &m.importsection.imports[index].func_desc.debug.name;
  else if(index - m.importsection.functions < m.function.n_funcdecl)
    name = &m.function.funcdecl[index - m.importsection.functions].debug.name;

  if(!name || !name->size())
  {
    // Try to find an export for this function
    for(varuint32 i = 0; i < m.exportsection.n_exports; ++i)
      if(m.exportsection.exports[i].kind == WASM_KIND_FUNCTION && m.exportsection.exports[i].index == index)
      {
        PushIdentifierToken(m.exportsection.exports[i].name, WatTokens::NAME);
        return;
      }

    PushNewNameToken("f%u", index);
  }
  else
    PushIdentifierToken(*name, WatTokens::NAME);
}

void Serializer::PushGlobalName(varuint32 index)
{
  Identifier* name = 0;
  size_t count     = m.importsection.globals - m.importsection.memories;

  if(index < count)
    name = &m.importsection.imports[index + m.importsection.memories].func_desc.debug.name;
  else if(index - count < m.global.n_globals)
    name = &m.global.globals[index - count].desc.debug.name;

  if(!name || !name->size())
  {
    // Try to find an export for this global
    for(varuint32 i = 0; i < m.exportsection.n_exports; ++i)
      if(m.exportsection.exports[i].kind == WASM_KIND_GLOBAL && m.exportsection.exports[i].index == index)
      {
        PushIdentifierToken(m.exportsection.exports[i].name, WatTokens::NAME);
        return;
      }

    PushNewNameToken("g%u", index);
  }
  else
    PushIdentifierToken(*name, WatTokens::NAME);
}

void Serializer::PushIdentifierToken(const ByteArray& id, WatTokens token)
{
  tokens.Push(WatToken{ token, id.str(), 0, 0, id.size() });
}

void Serializer::PushParamName(varuint32 index, const DebugInfo* names, varuint32 num, char prefix)
{
  if(index < num && names && names[index].name.size() != 0)
    PushIdentifierToken(names[index].name, WatTokens::NAME);
  else
    PushNewNameToken("%c%u", prefix, index);
}

// We can only use the local name if the index is an exact match - names can't be assigned to groups of locals.
void Serializer::PushLocalName(varuint32 index, const FunctionBody* body)
{
  varuint32 count        = 0;
  const Identifier* name = nullptr;
  for(varuint32 i = 0; i < body->n_locals; ++i)
  {
    if(count > index)
      break;
    if(count == index)
    {
      name = &body->locals[i].debug.name;
      break;
    }
    count += body->locals[i].count;
  }

  if(name && name->size() != 0)
    PushIdentifierToken(*name, WatTokens::NAME);
  else
    PushNewNameToken("%c%u", 'l', index);
}

void Serializer::PushBlockToken(int index)
{
  if(index < blocktokens.Size())
    tokens.Push(blocktokens[index]);
  else
    tokens.Push(WatToken{ WatTokens::INTEGER, 0, 0, 0, index });
}

void Serializer::TokenizeInstruction(Instruction& ins, const FunctionBody* body, const FunctionDesc* desc, size_t& block,
                                     bool emitdebug)
{
  if(ins.opcode >= OPNAMES.size())
  {
    tokens.Push(WatToken{ WatTokens::NONE });
    return;
  }

  if(emitdebug && ins.line > 0)
    tokens.Push(WatToken{ WatTokens::DEBUG_INFO, 0, ins.line, ins.column });

  DumpTokens(ins.line, ins.column);
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
        PushParamName(ins.immediates[0]._varuint32, desc->param_debug, ftype.n_params, 'p');
      else
        PushLocalName(ins.immediates[0]._varuint32 - ftype.n_params, body);
      break;
    } // If this isn't actually a function, just write out the integer by falling through
  case OP_global_get:
  case OP_global_set: PushGlobalName(ins.immediates[0]._varuint32); break;
  case OP_br:
  case OP_br_if: PushBlockToken(ins.immediates[0]._varuint32); break;
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
      PushBlockToken(ins.immediates[0].table[i]);

    PushBlockToken(ins.immediates[1]._varuint32);
    break;
  case OP_call: PushFunctionName(ins.immediates[0]._varuint32); break;
  case OP_call_indirect:
    if(ins.immediates[0]._varuint32 < m.type.n_functypes)
    {
      tokens.Push(WatToken{ WatTokens::OPEN, 0 });
      tokens.Push(WatToken{ WatTokens::TYPE, 0 });
      PushNewNameToken("t%u", ins.immediates[0]._varuint32);
      tokens.Push(WatToken{ WatTokens::CLOSE, 0 });
    }
    else
      tokens.Push(WatToken{ WatTokens::INTEGER, 0, 0, 0, ins.immediates[0]._varuint32 });
    break;
  case OP_block:
    PushNewNameToken("B%u", block++);
    blocktokens.Push(tokens.Back());
    break;
  case OP_loop:
    PushNewNameToken("L%u", block++);
    blocktokens.Push(tokens.Back());
    break;
  case OP_end:
    if(blocktokens.Size())
      blocktokens.Pop();
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
    if(ins.immediates[1]._varuptr != 0)
    {
      tokens.Push(WatToken{ WatTokens::OFFSET });
      tokens.Push(WatToken{ WatTokens::INTEGER, 0, 0, 0, (int64_t)ins.immediates[1]._varuptr });
    }

    if(ins.immediates[0]._varuint32 != 0)
    {
      size_t s = 0;
      switch(ins.opcode)
      {
      case OP_i32_load8_s:
      case OP_i64_load8_s:
      case OP_i32_load8_u:
      case OP_i64_load8_u:
      case OP_i32_store8:
      case OP_i64_store8: s = 0; break;
      case OP_i32_load16_s:
      case OP_i64_load16_s:
      case OP_i32_load16_u:
      case OP_i64_load16_u:
      case OP_i32_store16:
      case OP_i64_store16: s = 1; break;
      case OP_i32_load:
      case OP_f32_load:
      case OP_i32_store:
      case OP_f32_store:
      case OP_i64_load32_s:
      case OP_i64_load32_u:
      case OP_i64_store32: s = 2; break; // 2^2 == 4
      case OP_i64_load:
      case OP_f64_load:
      case OP_i64_store:
      case OP_f64_store: s = 3; break; // 2^3 == 8
      }

      if(ins.immediates[0]._varuint32 != s)
      {
        tokens.Push(WatToken{ WatTokens::ALIGN });
        tokens.Push(WatToken{ WatTokens::INTEGER, 0, 0, 0, (1LL << (int64_t)ins.immediates[0]._varuint32) });
      }
    }
    break;
  }
}

void Serializer::PushExportToken(varuint7 kind, varuint32 index, bool outside)
{
  if(m.knownsections & (1 << WASM_SECTION_EXPORT))
    for(varuint32 i = 0; i < m.exportsection.n_exports; ++i)
    {
      if(m.exportsection.exports[i].kind == kind && m.exportsection.exports[i].index == index)
      {
        tokens.Push(WatToken{ WatTokens::OPEN });
        tokens.Push(WatToken{ WatTokens::EXPORT });
        PushIdentifierToken(m.exportsection.exports[i].name);

        if(outside)
        {
          tokens.Push(WatToken{ WatTokens::OPEN });
          tokens.Push(WatToken{ WatTokens(static_cast<int>(WatTokens::FUNC) + kind) });
          tokens.Push(WatToken{ WatTokens::INTEGER, 0, 0, 0, index });
          tokens.Push(
            WatToken{ WatTokens::CLOSE }); // do NOT break here, because you can export a function under multiple names
        }

        tokens.Push(
          WatToken{ WatTokens::CLOSE }); // do NOT break here, because you can export a function under multiple names
      }
    }
}

void Serializer::TokenizeModule(bool emitdebug)
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

      PushNewNameToken("t%u", i);

      tokens.Push(WatToken{ WatTokens::OPEN });
      tokens.Push(WatToken{ TypeEncodingToken(m.type.functypes[i].form) });

      if(auto n_params = m.type.functypes[i].n_params; n_params > 0)
      {
        tokens.Push(WatToken{ WatTokens::OPEN });
        tokens.Push(WatToken{ WatTokens::PARAM });
        for(varuint32 j = 0; j < n_params; ++j)
          tokens.Push(WatToken{ TypeEncodingToken(m.type.functypes[i].params[j]) });
        tokens.Push(WatToken{ WatTokens::CLOSE });
      }

      if(auto n_returns = m.type.functypes[i].n_returns; n_returns > 0)
      {
        tokens.Push(WatToken{ WatTokens::OPEN });
        tokens.Push(WatToken{ WatTokens::RESULT });
        for(varuint32 j = 0; j < n_returns; ++j)
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
      PushIdentifierToken(imp.module_name);
      PushIdentifierToken(imp.export_name);

      tokens.Push(WatToken{ WatTokens::OPEN });
      varuint32 index = i;

      switch(imp.kind)
      {
      case WASM_KIND_FUNCTION:
      {
        tokens.Push(WatToken{ WatTokens::FUNC });
        PushFunctionName(i);

        tokens.Push(WatToken{ WatTokens::OPEN });
        tokens.Push(WatToken{ WatTokens::TYPE });
        PushNewNameToken("t%u", imp.func_desc.type_index);
        tokens.Push(WatToken{ WatTokens::CLOSE });

        auto& fn  = m.type.functypes[imp.func_desc.type_index];
        bool open = false;

        if(_dump && !imp.func_desc.param_debug)
        {
          imp.func_desc.param_debug = tmalloc<DebugInfo>(env, fn.n_params);
          memset(imp.func_desc.param_debug, 0, sizeof(DebugInfo) * fn.n_params);
        }

        for(varuint32 j = 0; j < fn.n_params; ++j)
        {
          if(imp.func_desc.param_debug)
            DumpTokens(imp.func_desc.param_debug[j].line, imp.func_desc.param_debug[j].column);
          if(emitdebug && imp.func_desc.param_debug != 0 && imp.func_desc.param_debug[j].line > 0)
            tokens.Push(
              WatToken{ WatTokens::DEBUG_INFO, 0, imp.func_desc.param_debug[j].line, imp.func_desc.param_debug[j].column });
          if(!open)
          {
            tokens.Push(WatToken{ WatTokens::OPEN });
            tokens.Push(WatToken{ WatTokens::PARAM });
            open = true;
          }

          if(imp.func_desc.param_debug && imp.func_desc.param_debug[j].name.size() > 0)
          {
            PushIdentifierToken(imp.func_desc.param_debug[j].name, WatTokens::NAME);
            tokens.Push(WatToken{ TypeEncodingToken(fn.params[j]) });
            tokens.Push(WatToken{ WatTokens::CLOSE });
            open = false;
          }
          else
            tokens.Push(WatToken{ TypeEncodingToken(fn.params[j]) });
        }

        if(open)
          tokens.Push(WatToken{ WatTokens::CLOSE });

        if(auto n_returns = m.type.functypes[imp.func_desc.type_index].n_returns; n_returns > 0)
        {
          tokens.Push(WatToken{ WatTokens::OPEN });
          tokens.Push(WatToken{ WatTokens::RESULT });
          for(varuint32 j = 0; j < n_returns; ++j)
            tokens.Push(WatToken{ TypeEncodingToken(fn.returns[j]) });
          tokens.Push(WatToken{ WatTokens::CLOSE });
        }
        break;
      }
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
      PushExportToken(imp.kind, index, true);
    }

  for(varuint32 i = 0; i < m.function.n_funcdecl && i < m.code.n_funcbody; ++i)
  {
    DumpTokens(m.function.funcdecl[i].debug.line, m.function.funcdecl[i].debug.column);
    if(emitdebug && m.function.funcdecl[i].debug.line > 0)
      tokens.Push(
        WatToken{ WatTokens::DEBUG_INFO, 0, m.function.funcdecl[i].debug.line, m.function.funcdecl[i].debug.column });
    tokens.Push(WatToken{ WatTokens::OPEN });
    tokens.Push(WatToken{ WatTokens::FUNC });
    PushFunctionName(i + m.importsection.functions);
    PushExportToken(WASM_KIND_FUNCTION, (i + m.importsection.functions), false);

    tokens.Push(WatToken{ WatTokens::OPEN });
    tokens.Push(WatToken{ WatTokens::TYPE });
    PushNewNameToken("t%u", m.function.funcdecl[i].type_index);
    tokens.Push(WatToken{ WatTokens::CLOSE });

    if(m.function.funcdecl[i].type_index >= m.type.n_functypes)
    {
      PushNewNameToken("[invalid function index %u]", m.function.funcdecl[i]);
      continue;
    }

    DumpTokens(m.code.funcbody[i].line, m.code.funcbody[i].column);
    if(emitdebug && m.code.funcbody[i].line > 0)
      tokens.Push(WatToken{ WatTokens::DEBUG_INFO, 0, m.code.funcbody[i].line, m.code.funcbody[i].column });

    auto& decl = m.function.funcdecl[i];
    auto& fn   = m.type.functypes[decl.type_index];

    if(_dump && !decl.param_debug)
    {
      decl.param_debug = tmalloc<DebugInfo>(env, fn.n_params);
      memset(decl.param_debug, 0, sizeof(DebugInfo) * fn.n_params);
    }

    for(varuint32 j = 0; j < fn.n_params; ++j)
    {
      if(decl.param_debug)
        DumpTokens(decl.param_debug[j].line, decl.param_debug[j].column);
      if(emitdebug && decl.param_debug && decl.param_debug[j].line > 0)
        tokens.Push(WatToken{ WatTokens::DEBUG_INFO, 0, decl.param_debug[j].line, decl.param_debug[j].column });
      tokens.Push(WatToken{ WatTokens::OPEN });
      tokens.Push(WatToken{ WatTokens::PARAM });
      PushParamName(j, decl.param_debug, fn.n_params, 'p');
      tokens.Push(WatToken{ TypeEncodingToken(fn.params[j]) });
      tokens.Push(WatToken{ WatTokens::CLOSE });
    }

    if(fn.n_returns > 0)
    {
      tokens.Push(WatToken{ WatTokens::OPEN });
      tokens.Push(WatToken{ WatTokens::RESULT });
      for(varuint32 j = 0; j < fn.n_returns; ++j)
        tokens.Push(WatToken{ TypeEncodingToken(fn.returns[j]) });
      tokens.Push(WatToken{ WatTokens::CLOSE });
    }

    varuint32 count = 0;
    for(varuint32 j = 0; j < m.code.funcbody[i].n_locals; ++j)
    {
      auto& local = m.code.funcbody[i].locals[j];
      DumpTokens(local.debug.line, local.debug.column);
      if(emitdebug && local.debug.line > 0)
        tokens.Push(WatToken{ WatTokens::DEBUG_INFO, 0, local.debug.line, local.debug.column });

      for(varuint32 k = 0; k < local.count; ++k)
      {
        tokens.Push(WatToken{ WatTokens::OPEN });
        tokens.Push(WatToken{ WatTokens::LOCAL });
        PushLocalName(count++, &m.code.funcbody[i]);
        tokens.Push(WatToken{ TypeEncodingToken(local.type) });
        tokens.Push(WatToken{ WatTokens::CLOSE });
      }
    }

    size_t block = 0;
    for(varuint32 j = 0; j < m.code.funcbody[i].n_body; ++j)
    {
      TokenizeInstruction(m.code.funcbody[i].body[j], &m.code.funcbody[i], &decl, block, emitdebug);
    }
    tokens.Push(WatToken{ WatTokens::CLOSE });
  }

  if(m.knownsections & (1 << WASM_SECTION_TABLE))
    for(varuint32 i = 0; i < m.table.n_tables; ++i)
    {
      DumpTokens(m.table.tables[i].debug.line, m.table.tables[i].debug.column);
      if(emitdebug && m.table.tables[i].debug.line > 0)
        tokens.Push(WatToken{ WatTokens::DEBUG_INFO, 0, m.table.tables[i].debug.line, m.table.tables[i].debug.column });
      tokens.Push(WatToken{ WatTokens::OPEN });
      tokens.Push(WatToken{ WatTokens::TABLE });
      PushExportToken(WASM_KIND_TABLE, i + m.importsection.tables - m.importsection.functions, false);
      tokenize_limits(tokens, m.table.tables[i].resizable);
      tokens.Push(WatToken{ TypeEncodingToken(m.table.tables[i].element_type) });
      tokens.Push(WatToken{ WatTokens::CLOSE });
    }

  if(m.knownsections & (1 << WASM_SECTION_MEMORY))
    for(varuint32 i = 0; i < m.memory.n_memories; ++i)
    {
      DumpTokens(m.memory.memories[i].debug.line, m.memory.memories[i].debug.column);
      if(emitdebug && m.memory.memories[i].debug.line > 0)
        tokens.Push(
          WatToken{ WatTokens::DEBUG_INFO, 0, m.memory.memories[i].debug.line, m.memory.memories[i].debug.column });
      tokens.Push(WatToken{ WatTokens::OPEN });
      tokens.Push(WatToken{ WatTokens::MEMORY });
      PushExportToken(WASM_KIND_MEMORY, i + m.importsection.memories - m.importsection.tables, false);
      tokenize_limits(tokens, m.memory.memories[i].limits);
      tokens.Push(WatToken{ WatTokens::CLOSE });
    }

  if(m.knownsections & (1 << WASM_SECTION_GLOBAL))
    for(varuint32 i = 0; i < m.global.n_globals; ++i)
    {
      DumpTokens(m.global.globals[i].desc.debug.line, m.global.globals[i].desc.debug.column);
      if(emitdebug && m.global.globals[i].desc.debug.line > 0)
        tokens.Push(
          WatToken{ WatTokens::DEBUG_INFO, 0, m.global.globals[i].desc.debug.line, m.global.globals[i].desc.debug.column });
      tokens.Push(WatToken{ WatTokens::OPEN });
      tokens.Push(WatToken{ WatTokens::GLOBAL });
      PushGlobalName(i);
      PushExportToken(WASM_KIND_GLOBAL, i + m.importsection.globals - m.importsection.memories, false);
      tokenize_global(tokens, m.global.globals[i].desc);
      tokens.Push(WatToken{ WatTokens::OPEN });
      size_t block = 0;
      TokenizeInstruction(m.global.globals[i].init, 0, 0, block, emitdebug);
      tokens.Push(WatToken{ WatTokens::CLOSE });
      tokens.Push(WatToken{ WatTokens::CLOSE });
    }

  if(m.knownsections & (1 << WASM_SECTION_START))
  {
    tokens.Push(WatToken{ WatTokens::OPEN });
    tokens.Push(WatToken{ WatTokens::START });
    PushFunctionName(m.start);
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
      size_t block = 0;
      TokenizeInstruction(m.element.elements[i].offset, 0, 0, block, emitdebug);
      tokens.Push(WatToken{ WatTokens::CLOSE });

      for(varuint32 j = 0; j < m.element.elements[i].n_elements; ++j)
        tokens.Push(WatToken{ WatTokens::INTEGER, 0, 0, 0, m.element.elements[i].elements[j] });

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
      size_t block = 0;
      TokenizeInstruction(m.data.data[i].offset, 0, 0, block, emitdebug);
      tokens.Push(WatToken{ WatTokens::CLOSE });

      tokens.Push(WatToken{ WatTokens::STRING, m.data.data[i].data.str(), 0, 0, m.data.data[i].data.size() });
      tokens.Push(WatToken{ WatTokens::CLOSE });
    }

  tokens.Push(WatToken{ WatTokens::CLOSE });
}

void Serializer::WriteTokens(std::ostream& out)
{
  auto pushline = [](size_t& line, std::streampos& lastp, size_t stack, std::ostream& out) {
    out << "\n";
    lastp = out.tellp();
    ++line;
    out << "    ";
    for(size_t k = 0; k < stack * 2; ++k)
      out.put(' ');
  };

  for(size_t i = 0; i < tokens.Size(); ++i)
  {
    assert(_depth != (size_t)~0);
    assert(_stack != (size_t)~0);

    if(i > 0 && (tokens[i - 1].id == WatTokens::OFFSET || tokens[i - 1].id == WatTokens::ALIGN))
      out << '=';
    else if(i > 0 && tokens[i - 1].id != WatTokens::OPEN && tokens[i].id != WatTokens::CLOSE)
      out << ' ';

    if(((i + 1) < tokens.Size()) && (tokens[i + 1].id == WatTokens::LOCAL) && !_localbreak)
    {
      _localbreak = true;
      pushline(_line, _lastp, _stack, out);
    }

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
      for(int j = 0; j < tokens[i].len; ++j)
      {
        auto c = static_cast<unsigned char>(tokens[i].pos[j]);
        if(isprint(c) && c != '"')
          out << tokens[i].pos[j];
        else
        {
          char buf[4] = { 0 };
          SPRINTF(buf, 4, "\\%.2x", c);
          out << buf;
        }
      }
      out << '"';
      break;
    case WatTokens::NAME:
      out << '$';
      out.write(tokens[i].pos, tokens[i].len);
      break;
    case WatTokens::OPERATOR:
      if(!_stack && tokens[i].u == OP_end)
        break; // Skip the final end in the function
      switch(tokens[i].u)
      {
      case OP_end: --_stack;
      default:
        if(_depth == 2)
          pushline(_line, _lastp, _stack, out);
        break;
      case OP_block:
      case OP_if:
      case OP_loop:
        if(_depth == 2)
          pushline(_line, _lastp, _stack, out);
        ++_stack;
        break;
      }

      if(tokens[i].u < OPNAMES.size())
        out << OPNAMES[static_cast<size_t>(tokens[i].u)];
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
      ++_depth;
      if(_depth == 2)
      {
        ++_line;
        out << "\n  ";
      }
      out << GetTokenString(tokens[i].id);
      break;
    case WatTokens::DEBUG_INFO:
      out << "\n";
      out << '[' << tokens[i].line << ':' << tokens[i].column << ']';
      break;
    case WatTokens::FUNC:
      _localbreak = false;
      out << GetTokenString(tokens[i].id);
      break;
    case WatTokens::CLOSE: --_depth;
    default: out << GetTokenString(tokens[i].id); break;
    }
  }
}

void Serializer::DumpTokens(unsigned int& line, unsigned int& column)
{
  if(_dump)
  {
    WriteTokens(*_dump);
    column = _dump->tellp() - _lastp;
    line   = _line;
    tokens.Clear();
  }
}