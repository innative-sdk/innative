// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "wat.h"
#include "util.h"
#include "parse.h"
#include "validate.h"
#include <limits>

using std::string;
using std::numeric_limits;

using namespace innative;
using namespace utility;

namespace innative {
  namespace wat {
    __KHASH_IMPL(indexname, , StringRef, varuint32, 1, internal::__ac_X31_hash_stringrefins, kh_int_hash_equal);

    template<typename T, int(*FN)(const WatToken&, std::string&, T&)>
    T ResolveInlineToken(WatState& state, const WatToken& token)
    {
      T t;
      int err = (*FN)(token, state.numbuf, t);
      return !err ? t : (T)~0;
    }

    WatState::WatState(Environment& e, Module& mod) : m(mod), env(e)
    {
      typehash = kh_init_indexname();
      funchash = kh_init_indexname();
      tablehash = kh_init_indexname();
      memoryhash = kh_init_indexname();
      globalhash = kh_init_indexname();
    }
    WatState::~WatState()
    {
      kh_destroy_indexname(typehash);
      kh_destroy_indexname(funchash);
      kh_destroy_indexname(tablehash);
      kh_destroy_indexname(memoryhash);
      kh_destroy_indexname(globalhash);
    }

    varuint32 WatState::GetJump(WatState& state, WatToken var)
    {
      if(var.id == TOKEN_NUMBER)
        return ResolveInlineToken<varuint32, &ResolveTokenu32>(state, var);
      if(var.id == TOKEN_NAME)
      {
        utility::StringRef err = { var.pos, var.len };
        for(varuint32 i = 0; i < stack.Size(); ++i)
          if(stack[i] == err)
            return i;
      }

      return (varuint32)~0;
    }

    void WriteUTF32(uint32_t ch, ByteArray& str, varuint32& index)
    {
      static const uint32_t UNI_REPLACEMENT_CHAR = 0x0000FFFD;
      static const uint32_t UNI_MAX_LEGAL_UTF32 = 0x0010FFFF;
      static const uint8_t firstByteMark[7] = { 0x00, 0x00, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC };
      static const uint32_t byteMask = 0xBF;
      static const uint32_t byteMark = 0x80;

      int bytesToWrite;
      if(ch < 0x80)
        bytesToWrite = 1;
      else if(ch < 0x800)
        bytesToWrite = 2;
      else if(ch < 0x10000)
        bytesToWrite = 3;
      else if(ch <= UNI_MAX_LEGAL_UTF32)
        bytesToWrite = 4;
      else
      {
        bytesToWrite = 3;
        ch = UNI_REPLACEMENT_CHAR;
      }


      varuint32 target = index + bytesToWrite;
      switch(bytesToWrite)
      { /* note: everything falls through. */
      case 4: str[--target] = (uint8_t)((ch | byteMark) & byteMask); ch >>= 6;
      case 3: str[--target] = (uint8_t)((ch | byteMark) & byteMask); ch >>= 6;
      case 2: str[--target] = (uint8_t)((ch | byteMark) & byteMask); ch >>= 6;
      case 1: str[--target] = (uint8_t)(ch | firstByteMark[bytesToWrite]);
      }
      index += bytesToWrite;
    }

    int WatString(const Environment& env, ByteArray& str, StringRef t)
    {
      if(!t.s)
        return assert(false), ERR_PARSE_INVALID_NAME;

      varuint32 index = 0;
      if(str.get())
      {
        index = str.size();
        size_t n = str.size() + t.len;
        uint8_t* b = tmalloc<uint8_t>(env, n + 1);
        if(!b)
          return assert(false), ERR_FATAL_OUT_OF_MEMORY;

        tmemcpy(b, n, str.get(), str.size());
        new(&str) ByteArray(b, n);
      }
      else
        str.resize(t.len, true, env);

      if(!t.len)
        return ERR_SUCCESS;

      if(!str.get())
        return assert(false), ERR_FATAL_OUT_OF_MEMORY;

      for(size_t i = 0; i < t.len; ++i)
      {
        if(t.s[i] == '\\')
        {
          switch(t.s[++i])
          {
          case 'n':
            str[index++] = '\n';
            break;
          case 't':
            str[index++] = '\t';
            break;
          case '\\':
            str[index++] = '\\';
            break;
          case '\'':
            str[index++] = '\'';
            break;
          case '"':
            str[index++] = '"';
            break;
          case 'u':
          {
            char* end;
            long u;
            errno = 0;
            u = strtol(t.s + i + 1, &end, 16);
            if(errno == ERANGE)
              return ERR_WAT_OUT_OF_RANGE;

            WriteUTF32(u, str, index);
            i += end - (t.s + i + 1);
            break;
          }
          default:
            if((t.s[i] >= '0' && t.s[i] <= '9') || (t.s[i] >= 'A' && t.s[i] <= 'F') || (t.s[i] >= 'a' && t.s[i] <= 'f'))
            {
              if((t.s[i + 1] >= '0' && t.s[i + 1] <= '9') || (t.s[i + 1] >= 'A' && t.s[i + 1] <= 'F') || (t.s[i + 1] >= 'a' && t.s[i + 1] <= 'f'))
              {
                char buf[3] = { t.s[i], t.s[i + 1], 0 };
                str[index++] = (uint8_t)strtol(buf, 0, 16);
                ++i;
                break;
              }
            }
            return assert(false), ERR_WAT_BAD_ESCAPE;
          }
        }
        else
          str[index++] = t.s[i];
      }
      str.discard(index, true);

      return ERR_SUCCESS;
    }

    int WatName(const Environment& env, ByteArray& name, const WatToken& t)
    {
      if(t.id != TOKEN_NAME || !t.pos || !t.len)
        return assert(false), ERR_PARSE_INVALID_NAME;

      name.resize(t.len, true, env);
      if(!name.get() || t.len > numeric_limits<varuint32>::max())
        return assert(false), ERR_FATAL_OUT_OF_MEMORY;
      tmemcpy((char*)name.get(), name.size(), t.pos, t.len);

      return ERR_SUCCESS;
    }

    template<class T>
    int AppendArray(T item, T*& a, varuint32& n)
    {
      if(!(a = trealloc<T>(a, ++n)))
        return assert(false), ERR_FATAL_OUT_OF_MEMORY;
      a[n - 1] = item;
      return ERR_SUCCESS;
    }

    varsint7 WatValType(WatTokenID id)
    {
      switch(id)
      {
      case TOKEN_i32: return TE_i32;
      case TOKEN_i64: return TE_i64;
      case TOKEN_f32: return TE_f32;
      case TOKEN_f64: return TE_f64;
      }

      return 0;
    }

    int AddWatValType(WatTokenID id, varsint7*& a, varuint32& n)
    {
      varsint7 ty = WatValType(id);
      if(!ty)
        return ERR_WAT_EXPECTED_VALTYPE;
      return AppendArray<varsint7>(ty, a, n);
    }

    int WatFunctionTypeInner(const Environment& env, Queue<WatToken>& tokens, FunctionType& sig, DebugInfo** info, bool anonymous)
    {
      sig.form = TE_func;
      int err;
      while(tokens.Size() > 1 && tokens[0].id == TOKEN_OPEN && tokens[1].id == TOKEN_PARAM)
      {
        WatToken src = tokens[1];
        EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
        EXPECTED(tokens, TOKEN_PARAM, ERR_WAT_EXPECTED_TOKEN);

        if(tokens.Peek().id == TOKEN_NAME)
        {
          if(anonymous)
            return ERR_WAT_UNEXPECTED_NAME;
          if(info) // You are legally allowed to put parameter names in typedefs in WAT, but the names are thrown away.
          {
            if(tokens.Peek().len >= numeric_limits<varuint32>::max())
              return assert(false), ERR_WAT_OUT_OF_RANGE;
            DebugInfo debug = { src.line, src.column };
            WatName(env, debug.name, tokens.Peek());

            varuint32 sz = sig.n_params;
            if(err = AppendArray<DebugInfo>(debug, *info, sz))
              return err;
          }
          tokens.Pop();
          if(err = AddWatValType(tokens.Pop().id, sig.params, sig.n_params))
            return err;
        }
        else
        {
          while(tokens.Peek().id != TOKEN_CLOSE)
          {
            if(info)
            {
              varuint32 sz = sig.n_params;
              if(err = AppendArray<DebugInfo>(DebugInfo{ src.line, src.column }, *info, sz))
                return err;
            }
            if(err = AddWatValType(tokens.Pop().id, sig.params, sig.n_params))
              return err;
          }
        }

        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      }

      while(tokens.Size() > 1 && tokens[0].id == TOKEN_OPEN && tokens[1].id == TOKEN_RESULT)
      {
        EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
        EXPECTED(tokens, TOKEN_RESULT, ERR_WAT_EXPECTED_TOKEN);

        while(tokens.Peek().id != TOKEN_CLOSE)
          if(err = AddWatValType(tokens.Pop().id, sig.returns, sig.n_returns))
            return err;

        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      }

      // We detect this special case because otherwise it can turn into a "type mismatch" error, which is very confusing
      if(tokens.Size() > 1 && tokens[0].id == TOKEN_OPEN && tokens[1].id == TOKEN_PARAM)
        return ERR_WAT_EXPECTED_OPERATOR;
      return ERR_SUCCESS;
    }

    int WatFunctionType(WatState& state, Queue<WatToken>& tokens, varuint32* index)
    {
      EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
      EXPECTED(tokens, TOKEN_FUNC, ERR_WAT_EXPECTED_FUNC);

      FunctionType sig = { 0 };
      int err = WatFunctionTypeInner(state.env, tokens, sig, 0, false);
      if(err != 0)
        return err;
      *index = state.m.type.n_functions;
      state.m.knownsections |= (1 << WASM_SECTION_TYPE);
      if(err = AppendArray<FunctionType>(sig, state.m.type.functions, state.m.type.n_functions))
        return err;

      EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      return ERR_SUCCESS;
    }

    int WatAppendImport(Module& m, const Import& i, varuint32* index)
    {
      if(m.table.n_tables > 0 || m.function.n_funcdecl > 0 || m.global.n_globals > 0 || m.memory.n_memories > 0)
        return ERR_WAT_INVALID_IMPORT_ORDER; // If we're trying to insert an import after declaring a table/func/global/memory, fail.

      *index = 0;
      if(!(m.importsection.imports = trealloc<Import>(m.importsection.imports, ++m.importsection.n_import)))
        return assert(false), ERR_FATAL_OUT_OF_MEMORY;

      // Find the correct index to insert into
      for(varuint32 j = 0; j < m.importsection.n_import - 1; ++j)
      {
        if(i.kind >= m.importsection.imports[j].kind)
          *index = j + 1;
        else
          break;
      }

      m.knownsections |= (1 << WASM_SECTION_IMPORT);
      if((m.importsection.n_import - *index - 1) > 0) // Move things out of the way if we aren't at the end of the array
        memmove(m.importsection.imports + *index + 1, m.importsection.imports + *index, (m.importsection.n_import - *index - 1) * sizeof(Import));

      m.importsection.imports[*index] = i; // Set the value

      // Properly increment counts based on kind
      switch(i.kind)
      {
      case WASM_KIND_FUNCTION:
        ++m.importsection.functions;
      case WASM_KIND_TABLE:
        ++m.importsection.tables;
      case WASM_KIND_MEMORY:
        ++m.importsection.memories;
      case WASM_KIND_GLOBAL: // Skip incrementing the globals count, because we already did it when incrementing n_import
        break;
      }

      switch(i.kind) // Fix the index
      {
      case WASM_KIND_TABLE: *index -= m.importsection.functions; break;
      case WASM_KIND_MEMORY: *index -= m.importsection.tables; break;
      case WASM_KIND_GLOBAL: *index -= m.importsection.memories; break;
      }

      // ValidateImportOrder(m);
      return ERR_SUCCESS;
    }

    varuint32 WatGetFromHash(WatState& state, kh_indexname_t* hash, const WatToken& t)
    {
      if(t.id == TOKEN_NUMBER)
        return ResolveInlineToken<varuint32, &ResolveTokenu32>(state, t);
      else if(t.id == TOKEN_NAME)
      {
        khiter_t iter = kh_get_indexname(hash, StringRef{ t.pos, t.len });

        if(kh_exist2(hash, iter))
          return kh_val(hash, iter);
      }

      return (varuint32)~0;
    }

    // This looks for an identical existing type and returns that ID if it exists, or inserts the signature as a new type
    int MergeFunctionType(WatState& state, const FunctionType& ftype, varuint32& out)
    {
      for(uint64_t i = 0; i < state.m.type.n_functions; ++i) // The WASM spec requires we look for the lowest possible matching index
      {
        if(MatchFunctionType(state.m.type.functions[i], ftype))
        {
          out = i;
          return ERR_SUCCESS;
        }
      }

      out = state.m.type.n_functions;
      state.m.knownsections |= (1 << WASM_SECTION_TYPE);
      return AppendArray<FunctionType>(ftype, state.m.type.functions, state.m.type.n_functions);
    }

    int WatTypeUse(WatState& state, Queue<WatToken>& tokens, varuint32& sig, DebugInfo** info, bool anonymous)
    {
      sig = (varuint32)~0;
      if(tokens.Size() > 1 && tokens[0].id == TOKEN_OPEN && tokens[1].id == TOKEN_TYPE)
      {
        EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
        EXPECTED(tokens, TOKEN_TYPE, ERR_WAT_EXPECTED_TYPE);

        if(tokens.Peek().id != TOKEN_NUMBER && tokens.Peek().id != TOKEN_NAME)
          return assert(false), ERR_WAT_EXPECTED_VAR;

        sig = WatGetFromHash(state, state.typehash, tokens.Pop());

        if(sig >= state.m.type.n_functions)
          AppendError(state.env, state.env.errors, &state.m, ERR_WAT_UNKNOWN_TYPE, "Invalid type signature %u", sig);

        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      }

      if(tokens.Size() > 1 && tokens[0].id == TOKEN_OPEN && (tokens[1].id == TOKEN_PARAM || tokens[1].id == TOKEN_RESULT))
      {
        // Create a type to match this function signature
        FunctionType func = { 0 };
        int err = WatFunctionTypeInner(state.env, tokens, func, info, anonymous);
        if(err)
          return err;

        if(sig != (varuint32)~0) // If we already have a type, compare the two types and make sure they are identical
        {
          if(sig < state.m.type.n_functions && !MatchFunctionType(state.m.type.functions[sig], func))
            return ERR_WAT_TYPE_MISMATCH;
        }
        else
        {
          sig = state.m.type.n_functions;
          return MergeFunctionType(state, func, sig);
        }
      }

      if(sig == (varuint32)~0) // If we still don't have a type, this is an empty type we need to add
      {
        return MergeFunctionType(state, FunctionType{ TE_func }, sig);
      }

      return ERR_SUCCESS;
    }

    // Checks if an integer is a power of two
    inline bool IsPowerOfTwo(varuint32 x) noexcept
    {
      return (x & (x - 1)) == 0;
    }

    // Given an exact power of two, quickly gets the log2 value
    inline uint32_t Power2Log2(uint32_t v) noexcept
    {
      assert(IsPowerOfTwo(v));
#ifdef IR_COMPILER_MSC
      unsigned long r;
      _BitScanReverse(&r, v);
#elif defined(IR_COMPILER_GCC)
      uint32_t r = (sizeof(uint32_t) << 3) - 1 - __builtin_clz(v);
#else
      const uint32_t b[] = { 0xAAAAAAAA, 0xCCCCCCCC, 0xF0F0F0F0, 0xFF00FF00, 0xFFFF0000 };
      uint32_t r = (v & b[0]) != 0;
      r |= ((v & b[4]) != 0) << 4;
      r |= ((v & b[3]) != 0) << 3;
      r |= ((v & b[2]) != 0) << 2;
      r |= ((v & b[1]) != 0) << 1;
#endif
      return r;
    }

    varuint32 WatGetLocal(WatState& state, FunctionBody& f, FunctionType& sig, const WatToken& t)
    {
      if(t.id == TOKEN_NUMBER)
        return ResolveInlineToken<varuint32, &ResolveTokenu32>(state, t);
      else if(t.id == TOKEN_NAME)
      {
        ByteArray n((uint8_t*)t.pos, t.len);

        for(uint64_t i = 0; i < sig.n_params; ++i)
          if(n == f.param_names[i].name)
            return (varuint32)i;

        for(uint64_t i = 0; i < f.n_locals; ++i)
          if(n == f.local_names[i].name)
            return (varuint32)(i + sig.n_params);
      }

      return (varuint32)~0;
    }

    int WatConstantOperator(WatState& state, Queue<WatToken>& tokens, Instruction& op)
    {
      int err = ERR_SUCCESS;
      switch(op.opcode)
      {
      case OP_i32_const:
        err = ResolveTokeni32(tokens.Pop(), state.numbuf, op.immediates[0]._varsint32);
        break;
      case OP_i64_const:
        err = ResolveTokeni64(tokens.Pop(), state.numbuf, op.immediates[0]._varsint64);
        break;
      case OP_f32_const:
        err = ResolveTokenf32(tokens.Pop(), state.numbuf, op.immediates[0]._float32);
        break;
      case OP_f64_const:
        err = ResolveTokenf64(tokens.Pop(), state.numbuf, op.immediates[0]._float64);
        break;
      case OP_global_get: // For constant initializers, this has to be an import, and thus must always already exist by the time we reach it.
        op.immediates[0]._varuint32 = WatGetFromHash(state, state.globalhash, tokens.Pop());
        if(op.immediates[0]._varuint32 == (varuint32)~0)
          return assert(false), ERR_WAT_INVALID_VAR;
        break;
      default:
        return ERR_INVALID_INITIALIZER;
      }

      return err;
    }
    int WatOperator(WatState& state, Queue<WatToken>& tokens, Instruction& op, FunctionBody& f, FunctionType& sig, DeferWatAction& defer)
    {
      if(tokens.Peek().id != TOKEN_OPERATOR)
        return ERR_WAT_EXPECTED_OPERATOR;

      int err;
      if(tokens.Peek().i > 0xFF)
        return ERR_WAT_OUT_OF_RANGE;
      op = { (uint8_t)tokens.Peek().i };
      op.line = tokens.Peek().line;
      op.column = tokens.Pop().column;

      switch(op.opcode)
      {
      case 0xFF:
        return assert(false), ERR_FATAL_UNKNOWN_INSTRUCTION;
      case OP_br:
      case OP_br_if:
        op.immediates[0]._varuint32 = state.GetJump(state, tokens.Pop());
        if(op.immediates[0]._varuint32 == (varuint32)~0)
          return ERR_WAT_EXPECTED_VAR;
        break;
      case OP_local_get:
      case OP_local_set:
      case OP_local_tee:
        op.immediates[0]._varuint32 = WatGetLocal(state, f, sig, tokens.Pop());
        if(op.immediates[0]._varuint32 == (varuint32)~0)
          return ERR_WAT_INVALID_LOCAL;
        break;
      case OP_global_get:
        if(!sig.form) // If this is zero, this is an initializer
        {
          if(err = WatConstantOperator(state, tokens, op))
            return err;
          break;
        }
      case OP_global_set:
      case OP_call:
        defer = DeferWatAction{ op.opcode, tokens.Pop(), 0, 0 };
        break;
      case OP_i32_const:
      case OP_i64_const:
      case OP_f32_const:
      case OP_f64_const:
        if(err = WatConstantOperator(state, tokens, op))
          return err;
        break;
      case OP_br_table:
        do
        {
          varuint32 jump = state.GetJump(state, tokens.Pop());
          if(jump == (varuint32)~0)
            return assert(false), ERR_WAT_EXPECTED_VAR;

          if(err = AppendArray<varuint32>(jump, op.immediates[0].table, op.immediates[0].n_table))
            return err;
        } while(tokens.Peek().id == TOKEN_NAME || tokens.Peek().id == TOKEN_NUMBER);

        op.immediates[1]._varuint32 = op.immediates[0].table[--op.immediates[0].n_table]; // Remove last jump from table and make it the default
        break;
      case OP_call_indirect:
        if(err = WatTypeUse(state, tokens, op.immediates[0]._varuint32, 0, true))
          return err;
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
        if(tokens.Peek().id == TOKEN_OFFSET)
        {
          tokens.Pop();
          if(err = ResolveTokenu32(tokens.Pop(), state.numbuf, op.immediates[1]._varuint32))
          //if(err = ResolveTokenu64(tokens.Pop(), op.immediates[1]._varuptr)) // We can't do this until webassembly actually supports 64-bit
            return err;
        }
        if(tokens.Peek().id == TOKEN_ALIGN)
        {
          tokens.Pop();
          if(err = ResolveTokenu32(tokens.Pop(), state.numbuf, op.immediates[0]._varuint32))
            return assert(false), err;
          if(op.immediates[0]._varuint32 == 0 || !IsPowerOfTwo(op.immediates[0]._varuint32)) // Ensure this alignment is exactly a power of two
            return ERR_WAT_INVALID_ALIGNMENT;
          op.immediates[0]._varuint32 = Power2Log2(op.immediates[0]._varuint32); // Calculate proper power of two
        }

        break;
      }

      return ERR_SUCCESS;
    }

    void WatLabel(WatState& state, Queue<WatToken>& tokens)
    {
      if(tokens.Peek().id == TOKEN_NAME)
      {
        state.stack.Push(StringRef{ tokens.Peek().pos, tokens.Peek().len });
        tokens.Pop();
      }
      else
        state.stack.Push(StringRef{ 0, 0 });
    }

    bool CheckLabel(WatState& state, Queue<WatToken>& tokens)
    {
      if(tokens.Peek().id == TOKEN_NAME)
      {
        WatToken t = tokens.Pop();
        return state.stack.Peek() == StringRef{ t.pos, t.len };
      }

      return true;
    }

    int WatBlockType(Queue<WatToken>& tokens, varsint7& out)
    {
      out = TE_void;
      if(tokens.Size() > 1 && tokens[0].id == TOKEN_OPEN && tokens[1].id == TOKEN_RESULT)
      {
        EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
        EXPECTED(tokens, TOKEN_RESULT, ERR_WAT_EXPECTED_RESULT);

        if(tokens.Peek().id != TOKEN_CLOSE)
        {
          if(!(out = WatValType(tokens.Pop().id)))
            return assert(false), ERR_WAT_EXPECTED_VALTYPE;

          if(tokens.Peek().id != TOKEN_CLOSE)
            return assert(false), ERR_MULTIPLE_RETURN_VALUES;
        }

        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      }

      if(tokens.Size() > 1 && tokens[0].id == TOKEN_OPEN && tokens[1].id == TOKEN_RESULT)
        return assert(false), ERR_MULTIPLE_RETURN_VALUES;
      return ERR_SUCCESS;
    }

    int WatInstruction(WatState& state, Queue<WatToken>& tokens, FunctionBody& f, FunctionType& sig, varuint32 index);

    int WatExpression(WatState& state, Queue<WatToken>& tokens, FunctionBody& f, FunctionType& sig, varuint32 index)
    {
      EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);

      int err;
      varsint7 blocktype;
      switch(tokens[0].id)
      {
      case TOKEN_BLOCK:
      case TOKEN_LOOP:
      {
        WatToken t = tokens.Pop();
        WatLabel(state, tokens);
        if(err = WatBlockType(tokens, blocktype))
          return err;

        {
          Instruction op = { t.id == TOKEN_BLOCK ? (uint8_t)OP_block : (uint8_t)OP_loop };
          op.immediates[0]._varsint7 = blocktype;
          op.line = t.line;
          op.column = t.column;
          if(err = AppendArray<Instruction>(op, f.body, f.n_body))
            return err;
        }

        while(tokens.Peek().id != TOKEN_CLOSE)
          if(err = WatInstruction(state, tokens, f, sig, index))
            return err;

        Instruction op = { OP_end };
        op.line = tokens.Peek().line;
        op.column = tokens.Peek().column;
        if(err = AppendArray<Instruction>(op, f.body, f.n_body))
          return err;
        state.stack.Pop();
        break;
      }
      case TOKEN_IF:
      {
        WatToken t = tokens.Pop();
        WatLabel(state, tokens);
        if(err = WatBlockType(tokens, blocktype))
          return err;

        while(tokens.Size() > 1 && tokens[0].id == TOKEN_OPEN && tokens[1].id != TOKEN_THEN)
          if(err = WatExpression(state, tokens, f, sig, index))
            return err;

        {
          Instruction op = { OP_if };
          op.immediates[0]._varsint7 = blocktype;
          op.line = t.line;
          op.column = t.column;
          if(err = AppendArray<Instruction>(op, f.body, f.n_body)) // We append the if instruction _after_ the optional condition expression
            return err;
        }
      }
      
        EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN); // There must always be a Then branch
        EXPECTED(tokens, TOKEN_THEN, ERR_WAT_EXPECTED_THEN);

        while(tokens.Peek().id != TOKEN_CLOSE)
          if(err = WatInstruction(state, tokens, f, sig, index))
            return err;

        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);

        if(tokens.Peek().id == TOKEN_OPEN) // Must be an else branch if it exists
        {
          EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);

          WatToken t = tokens.Peek();
          Instruction op = { OP_else };
          EXPECTED(tokens, TOKEN_ELSE, ERR_WAT_EXPECTED_ELSE);

          op.line = t.line;
          op.column = t.column;
          if(err = AppendArray<Instruction>(op, f.body, f.n_body))
            return err;

          while(tokens.Peek().id != TOKEN_CLOSE)
            if(err = WatInstruction(state, tokens, f, sig, index))
              return err;

          EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
        }

        {
          Instruction op = { OP_end };
          op.line = tokens.Peek().line;
          op.column = tokens.Peek().column;
          if(err = AppendArray<Instruction>(op, f.body, f.n_body))
            return err;
        }

        state.stack.Pop();
        break;
      default:
      {
        Instruction op;
        DeferWatAction defer = { 0 };
        if(err = WatOperator(state, tokens, op, f, sig, defer))
          return err;

        // Expressions are folded instructions, so we must unfold them before inserting the operator
        while(tokens[0].id != TOKEN_CLOSE)
          if(err = WatExpression(state, tokens, f, sig, index))
            return err;

        if(defer.id) // Only perform the defer after we evaluate the folded instructions, so f.n_body is correct
          state.defer.Push(DeferWatAction{ defer.id, defer.t, index, f.n_body });
        if(err = AppendArray<Instruction>(op, f.body, f.n_body)) // Now we append the operator
          return err;
        break;
      }
      }

      EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      return ERR_SUCCESS;
    }

    int WatInstruction(WatState& state, Queue<WatToken>& tokens, FunctionBody& f, FunctionType& sig, varuint32 index)
    {
      int err;
      varsint7 blocktype;
      switch(tokens[0].id)
      {
      case TOKEN_OPEN: // This must be an expression
        return WatExpression(state, tokens, f, sig, index);
      case TOKEN_BLOCK:
      case TOKEN_LOOP:
      {
        WatToken t = tokens.Pop();
        WatLabel(state, tokens);
        if(err = WatBlockType(tokens, blocktype))
          return err;

        {
          Instruction op = { t.id == TOKEN_BLOCK ? (uint8_t)OP_block : (uint8_t)OP_loop };
          op.immediates[0]._varsint7 = blocktype;
          op.line = t.line;
          op.column = t.column;
          if(err = AppendArray<Instruction>(op, f.body, f.n_body))
            return err;
        }

        while(tokens.Peek().id != TOKEN_END)
          if(err = WatInstruction(state, tokens, f, sig, index))
            return err;

        EXPECTED(tokens, TOKEN_END, ERR_WAT_EXPECTED_END);

        if(!CheckLabel(state, tokens))
          return ERR_WAT_LABEL_MISMATCH;

        {
          Instruction op = { OP_end };
          op.line = tokens.Peek().line;
          op.column = tokens.Peek().column;
          if(err = AppendArray<Instruction>(op, f.body, f.n_body))
            return err;
        }

        state.stack.Pop();
        break;
      }
      case TOKEN_IF:
      {
        WatToken t = tokens.Pop();
        WatLabel(state, tokens);
        if(err = WatBlockType(tokens, blocktype))
          return err;

        {
          Instruction op = { OP_if };
          op.immediates[0]._varsint7 = blocktype;
          op.line = t.line;
          op.column = t.column;
          if(err = AppendArray<Instruction>(op, f.body, f.n_body)) // We append the if instruction _after_ the optional condition expression
            return err;
        }

        while(tokens.Peek().id != TOKEN_ELSE && tokens.Peek().id != TOKEN_END)
          if(err = WatInstruction(state, tokens, f, sig, index))
            return err;

        t = tokens.Pop();
        if(t.id == TOKEN_ELSE) // Handle else branch
        {
          if(!CheckLabel(state, tokens))
            return ERR_WAT_LABEL_MISMATCH;

          Instruction op = { OP_else };
          op.line = t.line;
          op.column = t.column;
          if(err = AppendArray<Instruction>(op, f.body, f.n_body))
            return err;

          while(tokens.Peek().id != TOKEN_END)
            if(err = WatInstruction(state, tokens, f, sig, index))
              return err;

          EXPECTED(tokens, TOKEN_END, ERR_WAT_EXPECTED_END);
        }
      }
      
        if(!CheckLabel(state, tokens))
          return ERR_WAT_LABEL_MISMATCH;

        {
          Instruction op = { OP_end };
          op.line = tokens.Peek().line;
          op.column = tokens.Peek().column;
          if(err = AppendArray<Instruction>(op, f.body, f.n_body))
            return err;
        }

        state.stack.Pop();
        break;
      default:
      {
        Instruction op;
        DeferWatAction defer = { 0 };
        if(err = WatOperator(state, tokens, op, f, sig, defer))
          return err;

        if(defer.id)
          state.defer.Push(DeferWatAction{ defer.id, defer.t, index, f.n_body });
        return AppendArray<Instruction>(op, f.body, f.n_body);
      }
      }

      return ERR_SUCCESS;
    }

    int WatInlineImportExport(const Environment& env, Module& m, Queue<WatToken>& tokens, varuint32* index, varuint7 kind, Import** out)
    {
      int err;
      while(tokens.Size() > 1 && tokens[0].id == TOKEN_OPEN && tokens[1].id == TOKEN_EXPORT)
      {
        EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
        EXPECTED(tokens, TOKEN_EXPORT, ERR_WAT_EXPECTED_EXPORT);

        Export e = { };
        e.kind = kind;
        e.index = *index; // This is fine because you can only import OR export on a declaration statement

        if(err = WatString(env, e.name, tokens.Pop()))
          return err;
        if(!ValidateIdentifier(e.name))
          return ERR_INVALID_UTF8_ENCODING;
        m.knownsections |= (1 << WASM_SECTION_EXPORT);
        if(err = AppendArray<Export>(e, m.exportsection.exports, m.exportsection.n_exports))
          return err;
        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      }
      if(tokens.Size() > 1 && tokens[0].id == TOKEN_OPEN && tokens[1].id == TOKEN_IMPORT)
      {
        EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
        EXPECTED(tokens, TOKEN_IMPORT, ERR_WAT_EXPECTED_IMPORT);

        Import i = { };
        if(err = WatString(env, i.module_name, tokens.Pop()))
          return err;
        if(!ValidateIdentifier(i.module_name))
          return ERR_INVALID_UTF8_ENCODING;
        if(err = WatString(env, i.export_name, tokens.Pop()))
          return err;
        if(!ValidateIdentifier(i.export_name))
          return ERR_INVALID_UTF8_ENCODING;

        i.kind = kind;
        if(err = WatAppendImport(m, i, index))
          return err;

        switch(i.kind) // Fix the index
        {
        case WASM_KIND_FUNCTION: *out = m.importsection.imports + *index; break;
        case WASM_KIND_TABLE: *out = m.importsection.imports + m.importsection.functions + *index; break;
        case WASM_KIND_MEMORY: *out = m.importsection.imports + m.importsection.tables + *index; break;
        case WASM_KIND_GLOBAL: *out = m.importsection.imports + m.importsection.memories + *index; break;
        }

        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      }

      return ERR_SUCCESS;
    }

    int WatLocalAppend(FunctionBody& body, Queue<WatToken>& tokens)
    {
      varsint7 local = WatValType(tokens.Pop().id);
      if(!local)
        return assert(false), ERR_WAT_EXPECTED_VALTYPE;
      return AppendArray<varsint7>(local, body.locals, body.n_locals);
    }

    int WatFunction(WatState& state, Queue<WatToken>& tokens, varuint32* index, StringRef name)
    {
      int err;
      FunctionBody body = { 0 };
      body.debug.line = tokens.Peek().line;
      body.debug.column = tokens.Peek().column;

      *index = state.m.function.n_funcdecl + state.m.importsection.functions;
      Import* i = 0;
      if(err = WatInlineImportExport(state.env, state.m, tokens, index, WASM_KIND_FUNCTION, &i))
        return err;

      if(i) // If this is an import, assemble the aux information and abort.
        return WatTypeUse(state, tokens, i->func_desc.type_index, &i->func_desc.param_names, false);

      varuint32 sig;
      if(err = WatTypeUse(state, tokens, sig, &body.param_names, false))
        return err;

      FunctionType& desc = state.m.type.functions[sig];
      if(name.len > 0)
        if(err = WatString(state.env, body.debug.name, name))
          return err;

      // Read in all the locals
      while(tokens.Size() > 1 && tokens[0].id == TOKEN_OPEN && tokens[1].id == TOKEN_LOCAL)
      {
        WatToken src = tokens[1];
        EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
        EXPECTED(tokens, TOKEN_LOCAL, ERR_WAT_EXPECTED_LOCAL);

        if(tokens.Peek().id == TOKEN_NAME)
        {
          if(tokens.Peek().len > numeric_limits<varuint32>::max())
            return assert(false), ERR_WAT_OUT_OF_RANGE;
          DebugInfo debug = { src.line, src.column };
          WatName(state.env, debug.name, tokens.Pop());

          varuint32 sz = body.n_locals; // n_locals is the count, but we don't want to increment it yet
          if(err = AppendArray<DebugInfo>(debug, body.local_names, sz))
            return err;

          if(err = WatLocalAppend(body, tokens)) // Must have exactly one val_type to associate with the name
            return err;
        }
        else // Otherwise can have zero or more val_types
        {
          while(tokens[0].id != TOKEN_CLOSE)
          {
            varuint32 sz = body.n_locals; // n_locals is the count, but we don't want to increment it yet
            if(err = AppendArray<DebugInfo>(DebugInfo{ src.line, src.column }, body.local_names, sz))
              return err;
            if(err = WatLocalAppend(body, tokens))
              return err;
          }
        }

        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      }

      // Read in all instructions
      assert(state.stack.Size() == 0);
      while(tokens.Peek().id != TOKEN_CLOSE)
      {
        if(err = WatInstruction(state, tokens, body, desc, *index))
          return err;
      }
      assert(state.stack.Size() == 0);
      Instruction op = { OP_end };
      op.line = tokens.Peek().line;
      op.column = tokens.Peek().column;
      if(err = AppendArray(op, body.body, body.n_body))
        return err;

      state.m.knownsections |= (1 << WASM_SECTION_FUNCTION);
      if(err = AppendArray(sig, state.m.function.funcdecl, state.m.function.n_funcdecl))
        return err;

      state.m.knownsections |= (1 << WASM_SECTION_CODE);
      return AppendArray(body, state.m.code.funcbody, state.m.code.n_funcbody);
    }

    int WatResizableLimits(WatState& state, ResizableLimits& limits, Queue<WatToken>& tokens)
    {
      int err = ResolveTokenu32(tokens.Pop(), state.numbuf, limits.minimum);
      if(err)
        return err;
      if(tokens.Peek().id == TOKEN_NUMBER)
      {
        if(err = ResolveTokenu32(tokens.Pop(), state.numbuf, limits.maximum))
          return err;
        limits.flags = 1;
      }

      return ERR_SUCCESS;
    }

    int WatTableDesc(WatState& state, TableDesc& t, Queue<WatToken>& tokens)
    {
      int err;
      if(err = WatResizableLimits(state, t.resizable, tokens))
        return err;

      EXPECTED(tokens, TOKEN_FUNCREF, ERR_WAT_EXPECTED_FUNCREF);

      t.element_type = TE_funcref;
      return ERR_SUCCESS;
    }

    int WatTable(WatState& state, Queue<WatToken>& tokens, varuint32* index)
    {
      int err;
      *index = state.m.table.n_tables + state.m.importsection.tables - state.m.importsection.functions;
      Import* i = 0;
      if(err = WatInlineImportExport(state.env, state.m, tokens, index, WASM_KIND_TABLE, &i))
        return err;

      if(i) // If this is an import, assemble the aux information and abort.
        return WatTableDesc(state, i->table_desc, tokens);

      TableDesc table = { 0 };
      switch(tokens.Peek().id)
      {
      case TOKEN_NUMBER:
        if(err = WatTableDesc(state, table, tokens))
          return err;
        break;
      default:
        EXPECTED(tokens, TOKEN_FUNCREF, ERR_WAT_EXPECTED_FUNCREF);

        table.element_type = TE_funcref;
        table.resizable.flags = 0;
        state.defer.Push(DeferWatAction{ -TOKEN_ELEM, {TOKEN_NONE}, tokens.GetPosition(), *index });
        SkipSection(tokens); // Defer element section to after we know we've loaded everything.
      }

      state.m.knownsections |= (1 << WASM_SECTION_TABLE);
      return AppendArray(table, state.m.table.tables, state.m.table.n_tables);
    }

    int WatInitializerInstruction(WatState& state, Queue<WatToken>& tokens, Instruction& op, bool expr)
    {
      int err;
      FunctionBody blank = { 0 };
      FunctionType typeblank = { 0 };

      if(expr)
      {
        if(err = WatExpression(state, tokens, blank, typeblank, 0))
          return err;
      }
      else
      {
        while(tokens.Peek().id != TOKEN_CLOSE)
        {
          if(err = WatInstruction(state, tokens, blank, typeblank, 0))
            return err;
        }
      }

      if(blank.n_body != 1)
        AppendError(state.env, state.env.errors, 0, ERR_INVALID_INITIALIZER, "Only one instruction is allowed as an initializer");

      if(blank.n_body > 0)
        op = blank.body[0];

      return ERR_SUCCESS;
    }

    int WatInitializer(WatState& state, Queue<WatToken>& tokens, Instruction& op)
    {
      int err = WatInitializerInstruction(state, tokens, op, false);
      if(err < 0)
        return err;

      if(tokens.Peek().id != TOKEN_CLOSE)
        return assert(false), ERR_INVALID_INITIALIZER;

      return ERR_SUCCESS;
    }

    int WatGlobalDesc(GlobalDesc& g, Queue<WatToken>& tokens)
    {
      if(tokens.Peek().id == TOKEN_OPEN)
      {
        EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
        EXPECTED(tokens, TOKEN_MUT, ERR_WAT_EXPECTED_MUT);
        g.mutability = true;
        if(!(g.type = WatValType(tokens.Pop().id)))
          return assert(false), ERR_WAT_EXPECTED_VALTYPE;

        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      }
      else
      {
        g.mutability = false;
        if(!(g.type = WatValType(tokens.Pop().id)))
          return assert(false), ERR_WAT_EXPECTED_VALTYPE;
      }

      return ERR_SUCCESS;
    }

    int WatGlobal(WatState& state, Queue<WatToken>& tokens, varuint32* index)
    {
      int err;
      *index = state.m.global.n_globals + state.m.importsection.globals - state.m.importsection.memories;
      Import* i = 0;
      if(err = WatInlineImportExport(state.env, state.m, tokens, index, WASM_KIND_GLOBAL, &i))
        return err;

      if(i) // If this is an import, assemble the aux information and abort.
        return WatGlobalDesc(i->global_desc, tokens);

      GlobalDecl g = { 0 };
      if(err = WatGlobalDesc(g.desc, tokens))
        return err;

      if(err = WatInitializer(state, tokens, g.init))
        return err;

      state.m.knownsections |= (1 << WASM_SECTION_GLOBAL);
      return AppendArray(g, state.m.global.globals, state.m.global.n_globals);
    }

    int WatMemoryDesc(WatState& state, MemoryDesc& m, Queue<WatToken>& tokens)
    {
      return WatResizableLimits(state, m.limits, tokens);
    }

    int WatMemory(WatState& state, Queue<WatToken>& tokens, varuint32* index)
    {
      int err;
      *index = state.m.memory.n_memories + state.m.importsection.memories - state.m.importsection.tables;
      Import* i = 0;
      if(err = WatInlineImportExport(state.env, state.m, tokens, index, WASM_KIND_MEMORY, &i))
        return err;

      if(i) // If this is an import, assemble the aux information and abort.
        return WatMemoryDesc(state, i->mem_desc, tokens);

      MemoryDesc mem = { 0 };

      if(tokens.Size() > 1 && tokens[0].id == TOKEN_OPEN && tokens[1].id == TOKEN_DATA)
      {
        EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
        EXPECTED(tokens, TOKEN_DATA, ERR_WAT_EXPECTED_TOKEN);
        DataInit init = { 0 };
        init.index = *index;
        init.offset = Instruction{ OP_i32_const, 0 };

        while(tokens[0].id != TOKEN_CLOSE)
        {
          if(tokens[0].id != TOKEN_STRING)
            return assert(false), ERR_WAT_EXPECTED_STRING;
          if(err = WatString(state.env, init.data, tokens.Pop()))
            return err;
        }

        state.m.knownsections |= (1 << WASM_SECTION_DATA);
        if(err = AppendArray(init, state.m.data.data, state.m.data.n_data))
          return err;

        mem.limits.flags = 0;
        mem.limits.minimum = init.data.size();
        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      }
      else if(err = WatMemoryDesc(state, mem, tokens))
        return err;

      state.m.knownsections |= (1 << WASM_SECTION_MEMORY);
      return AppendArray(mem, state.m.memory.memories, state.m.memory.n_memories);
    }

    int AddWatName(kh_indexname_t* h, WatToken t, varuint32 index)
    {
      if(t.id == TOKEN_NAME)
      {
        int r;
        khiter_t iter = kh_put_indexname(h, StringRef{ t.pos, t.len }, &r);
        if(!r)
          return assert(false), ERR_WAT_DUPLICATE_NAME;
        if(iter != kh_end(h))
          kh_val(h, iter) = index;
      }

      return ERR_SUCCESS;
    }

    int WatImport(WatState& state, Queue<WatToken>& tokens)
    {
      Import i = { };
      int err;
      if(err = WatString(state.env, i.module_name, tokens.Pop()))
        return err;
      if(err = WatString(state.env, i.export_name, tokens.Pop()))
        return err;

      EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);

      WatToken t = tokens.Pop();
      WatToken name = GetWatNameToken(tokens);
      kh_indexname_t* hash = 0;
      switch(t.id)
      {
      case TOKEN_FUNC:
        i.kind = WASM_KIND_FUNCTION;
        if(name.id == TOKEN_NAME && (err = WatName(state.env, i.func_desc.debug.name, name)))
          return err;
        if(err = WatTypeUse(state, tokens, i.func_desc.type_index, &i.func_desc.param_names, false))
          return err;
        hash = state.funchash;
        break;
      case TOKEN_GLOBAL:
        i.kind = WASM_KIND_GLOBAL;
        if(err = WatGlobalDesc(i.global_desc, tokens))
          return err;
        hash = state.globalhash;
        break;
      case TOKEN_TABLE:
        i.kind = WASM_KIND_TABLE;
        if(err = WatTableDesc(state, i.table_desc, tokens))
          return err;
        hash = state.tablehash;
        break;
      case TOKEN_MEMORY:
        i.kind = WASM_KIND_MEMORY;
        if(err = WatMemoryDesc(state, i.mem_desc, tokens))
          return err;
        hash = state.memoryhash;
        break;
      default:
        return assert(false), ERR_WAT_EXPECTED_KIND;
      }
      EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);

      varuint32 index;
      if(err = WatAppendImport(state.m, i, &index))
        return err;

      return AddWatName(hash, name, index);
    }

    template<int(*F)(WatState&, Queue<WatToken>&, varuint32*)>
    int WatIndexProcess(WatState& state, Queue<WatToken>& tokens, kh_indexname_t* hash)
    {
      WatToken t = GetWatNameToken(tokens);

      int err;
      varuint32 index = (varuint32)~0;
      if(err = (*F)(state, tokens, &index))
        return err;
      assert(index != (varuint32)~0);

      return AddWatName(hash, t, index);
    }

    int WatExport(WatState& state, Queue<WatToken>& tokens)
    {
      Export e = {};
      int err;
      if(err = WatString(state.env, e.name, tokens.Pop()))
        return err;

      EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
      switch(tokens.Pop().id)
      {
      case TOKEN_FUNC:
        e.kind = WASM_KIND_FUNCTION;
        e.index = WatGetFromHash(state, state.funchash, tokens.Pop());
        break;
      case TOKEN_GLOBAL:
        e.kind = WASM_KIND_GLOBAL;
        e.index = WatGetFromHash(state, state.globalhash, tokens.Pop());
        break;
      case TOKEN_TABLE:
        e.kind = WASM_KIND_TABLE;
        e.index = WatGetFromHash(state, state.tablehash, tokens.Pop());
        break;
      case TOKEN_MEMORY:
        e.kind = WASM_KIND_MEMORY;
        e.index = WatGetFromHash(state, state.memoryhash, tokens.Pop());
        break;
      default:
        return assert(false), ERR_WAT_EXPECTED_KIND;
      }
      EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);

      state.m.knownsections |= (1 << WASM_SECTION_EXPORT);
      return AppendArray(e, state.m.exportsection.exports, state.m.exportsection.n_exports);
    }

    int WatElemData(WatState& state, Queue<WatToken>& tokens, varuint32& index, Instruction& op, kh_indexname_t* hash)
    {
      if(tokens[0].id == TOKEN_NUMBER || tokens[0].id == TOKEN_NAME)
        index = WatGetFromHash(state, hash, tokens.Pop());

      if(index == (varuint32)~0)
        return assert(false), ERR_WAT_INVALID_VAR;

      if(tokens[0].id == TOKEN_OPEN)
      {
        bool offset = tokens.Size() > 1 && tokens[0].id == TOKEN_OPEN && tokens[1].id == TOKEN_OFFSET;
        if(offset)
        {
          EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
          EXPECTED(tokens, TOKEN_OFFSET, ERR_WAT_EXPECTED_TOKEN);
        }

        int err = WatInitializerInstruction(state, tokens, op, !offset); // Without an offset wrapper, only an expression is allowed
        if(err < 0)
          return err;

        if(offset)
          EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      }

      return ERR_SUCCESS;
    }

    int WatElem(WatState& state, TableInit& e, Queue<WatToken>& tokens)
    {
      while(tokens[0].id != TOKEN_CLOSE)
      {
        int err = AppendArray(WatGetFromHash(state, state.funchash, tokens.Pop()), e.elements, e.n_elements);
        if(err)
          return err;
        if(e.elements[e.n_elements - 1] == (varuint32)~0)
          return assert(false), ERR_WAT_INVALID_VAR;
      }

      state.m.knownsections |= (1 << WASM_SECTION_ELEMENT);
      return AppendArray(e, state.m.element.elements, state.m.element.n_elements);
    }

    int WatData(WatState& state, Queue<WatToken>& tokens)
    {
      DataInit d = { 0 };
      int err;
      if(err = WatElemData(state, tokens, d.index, d.offset, state.memoryhash))
        return err;

      while(tokens[0].id != TOKEN_CLOSE)
      {
        if(tokens[0].id != TOKEN_STRING)
          return assert(false), ERR_WAT_EXPECTED_STRING;
        if(err = WatString(state.env, d.data, tokens.Pop()))
          return err;
      }

      state.m.knownsections |= (1 << WASM_SECTION_DATA);
      return AppendArray(d, state.m.data.data, state.m.data.n_data);
    }

    // Skips over an entire section of tokens by counting paranthesis, assuming they are well-formed
    void SkipSection(Queue<WatToken>& tokens, int count)
    {
      while(tokens.Size())
      {
        if(tokens[0].id == TOKEN_OPEN)
          ++count;
        else if(tokens[0].id == TOKEN_CLOSE)
        {
          if(!--count)
            break; // Deliberately do not pop the CLOSE token because we usually need it afterwards
        }
        tokens.Pop();
      }
    }

    int WatModule(Environment& env, Module& m, Queue<WatToken>& tokens, StringRef name, WatToken& internalname)
    {
      int err;
      m = { 0 };
      if(name.s && (err = WatName(env, m.name, WatToken{ TOKEN_NAME, (const char*)name.s, 0, 0, (int64_t)name.len })))
        return err;

      if((tokens.Peek().id == TOKEN_NAME) && (err = WatName(env, m.name, internalname = tokens.Pop())))
        return err;

      WatState state(env, m);

      WatToken t;
      size_t restore = tokens.GetPosition();
      while(tokens.Size() > 0 && tokens.Peek().id != TOKEN_CLOSE)
      {
        EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
        t = tokens.Pop();
        switch(t.id) // This initial pass is for types and function types only
        {
        case TOKEN_TYPE:
          if(err = WatIndexProcess<WatFunctionType>(state, tokens, state.typehash))
            return err;
          break;
        default:
          SkipSection(tokens);
          break;
        }
        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      }

      // This is the main pass for functions/imports/etc. and also identifies illegal tokens
      tokens.SetPosition(restore);
      while(tokens.Size() > 0 && tokens.Peek().id != TOKEN_CLOSE)
      {
        EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
        t = tokens.Pop();
        switch(t.id)
        {
        case TOKEN_FUNC:
        {
          khiter_t iter = kh_end(state.funchash);
          WatToken fname = GetWatNameToken(tokens);
          if(fname.id == TOKEN_NAME)
          {
            int r;
            iter = kh_put_indexname(state.funchash, StringRef{ fname.pos, fname.len }, &r);
            if(!r)
              return assert(false), ERR_WAT_DUPLICATE_NAME;
          }

          StringRef ref = { nullptr, 0 };
          if(iter != kh_end(state.funchash))
            ref = { kh_key(state.funchash, iter).s, kh_key(state.funchash, iter).len };
          varuint32 index;
          if(err = WatFunction(state, tokens, &index, ref))
            return err;

          if(fname.id == TOKEN_NAME)
          {
            if(index < m.importsection.functions)
              WatName(env, m.importsection.imports[index].func_desc.debug.name, fname);
            else if(index - m.importsection.functions < m.code.n_funcbody)
              WatName(env, m.code.funcbody[index - m.importsection.functions].debug.name, fname);
          }

          if(iter != kh_end(state.funchash))
            kh_val(state.funchash, iter) = index;
          break;
        }
        case TOKEN_IMPORT:
          if(err = WatImport(state, tokens))
            return err;
          break;
        case TOKEN_TABLE:
          if(err = WatIndexProcess<WatTable>(state, tokens, state.tablehash))
            return err;
          break;
        case TOKEN_MEMORY:
          if(err = WatIndexProcess<WatMemory>(state, tokens, state.memoryhash))
            return err;
          break;
        case TOKEN_GLOBAL:
          if(err = WatIndexProcess<WatGlobal>(state, tokens, state.globalhash))
            return err;
          break;
        case TOKEN_EXPORT:
        case TOKEN_TYPE:
        case TOKEN_ELEM:
        case TOKEN_DATA:
        case TOKEN_START:
          SkipSection(tokens);
          break;
        default:
          return assert(false), ERR_WAT_INVALID_TOKEN;
        }
        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      }

      // This pass resolves exports, elem, data, and the start function, to minimize deferred actions
      tokens.SetPosition(restore);
      while(tokens.Size() > 0 && tokens.Peek().id != TOKEN_CLOSE)
      {
        EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
        t = tokens.Pop();
        switch(t.id)
        {
        case TOKEN_EXPORT:
          if(err = WatExport(state, tokens))
            return err;
          break;
        case TOKEN_ELEM:
        {
          TableInit init = { 0 };
          if(err = WatElemData(state, tokens, init.index, init.offset, state.tablehash))
            return err;
          if(err = WatElem(state, init, tokens))
            return err;
          break;
        }
        case TOKEN_DATA:
          if(err = WatData(state, tokens))
            return err;
          break;
        case TOKEN_START:
          m.knownsections |= (1 << WASM_SECTION_START);
          if(tokens[0].id != TOKEN_NUMBER && tokens[0].id != TOKEN_NAME)
            return assert(false), ERR_WAT_EXPECTED_VAR;
          m.start = WatGetFromHash(state, state.funchash, tokens.Pop());
          if(m.start == (varuint32)~0)
            return assert(false), ERR_WAT_INVALID_VAR;
          break;
        default:
          SkipSection(tokens);
          break;
        }
        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      }

      auto procRef = [](WatState& s, Module& mod, varuint32 e) {
        if(s.defer[0].func < mod.importsection.functions || s.defer[0].func >= mod.code.n_funcbody + mod.importsection.functions)
          return ERR_INVALID_FUNCTION_INDEX;
        auto& f = mod.code.funcbody[s.defer[0].func - mod.importsection.functions];
        if(s.defer[0].index >= f.n_body)
          return ERR_INVALID_FUNCTION_BODY;
        f.body[s.defer[0].index].immediates[0]._varuint32 = e;
        return ERR_SUCCESS;
      };

      // Process all deferred actions
      while(state.defer.Size() > 0)
      {
        switch(state.defer[0].id)
        {
        case -TOKEN_ELEM:
        {
          size_t cache = tokens.GetPosition();
          tokens.SetPosition(state.defer[0].func);

          TableInit init = { 0 };
          init.index = state.defer[0].index;
          init.offset = Instruction{ OP_i32_const, 0 };

          EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
          EXPECTED(tokens, TOKEN_ELEM, ERR_WAT_EXPECTED_ELEM);
          err = WatElem(state, init, tokens);
          EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);

          state.m.table.tables[state.defer[0].index].resizable.minimum = init.n_elements;
          tokens.SetPosition(cache);
        }
        break;
        case OP_global_get:
        case OP_global_set:
          err = procRef(state, m, WatGetFromHash(state, state.globalhash, state.defer[0].t));
          break;
        case OP_call:
          err = procRef(state, m, WatGetFromHash(state, state.funchash, state.defer[0].t));
          break;
        default:
          return assert(false), ERR_WAT_INVALID_TOKEN;
        }
        if(err)
          return assert(false), err;
        state.defer.Pop();
      }

      m.exports = kh_init_exports();
      assert(m.name.str());
      return ParseExportFixup(m, env.errors, env);
    }

    int WatEnvironment(Environment& env, Queue<WatToken>& tokens)
    {
      return 0;
    }

    // Checks for parse errors in the tokenization process
    int CheckWatTokens(const Environment& env, ValidationError*& errors, Queue<WatToken>& tokens, const char* start)
    {
      int err = ERR_SUCCESS;
      for(size_t i = 0; i < tokens.Size(); ++i)
      {
        switch(tokens[i].id)
        {
        case TOKEN_NONE:
          AppendError(env, errors, nullptr, ERR_WAT_INVALID_TOKEN, "[%zu] Invalid token: %s", WatLineNumber(start, tokens[i].pos), string(tokens[i].pos, tokens[i].len).c_str());
          break;
        case TOKEN_RANGE_ERROR:
          AppendError(env, errors, nullptr, ERR_WAT_OUT_OF_RANGE, "[%zu] Constant out of range: %s", WatLineNumber(start, tokens[i].pos), string(tokens[i].pos, tokens[i].len).c_str());
          break;
        default:
          continue;
        }
        err = ERR_WAT_INVALID_TOKEN;
      }

      return err;
    }

    int ParseWatModule(Environment& env, Module& m, uint8_t* data, size_t sz, StringRef name)
    {
      Queue<WatToken> tokens;
      TokenizeWAT(tokens, (char*)data, (char*)data + sz);
      WatToken nametoken;

      if(!tokens.Size())
        return ERR_FATAL_INVALID_MODULE;

      int err = CheckWatTokens(env, env.errors, tokens, (char*)data);
      if(err < 0)
        return err;

      // If we don't detect "(module", just assume it's an inline module
      if(tokens[0].id != TOKEN_OPEN || tokens[1].id != TOKEN_MODULE)
        return WatModule(env, m, tokens, name, nametoken);

      EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
      EXPECTED(tokens, TOKEN_MODULE, ERR_WAT_EXPECTED_MODULE);
      if(!(err = WatModule(env, m, tokens, name, nametoken)))
        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      return err;
    }

    size_t WatLineNumber(const char* start, const char* pos)
    {
      size_t count = 1;
      while(start < pos)
      {
        if(*(start++) == '\n')
          ++count;
      }
      return count;
    }
  }
}