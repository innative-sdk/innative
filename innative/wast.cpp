// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "wast.h"
#include "validate.h"
#include "compile.h"
#include "queue.h"
#include "parse.h"
#include <signal.h>
#include <setjmp.h>
#include <iostream>

using namespace innative;
using namespace wat;
using namespace utility;
using std::string;
using validate::AppendError;

KHASH_INIT(assertion, int, const char*, 1, kh_int_hash_func, kh_int_hash_equal)

kh_assertion_t* GenAssertions(std::initializer_list<int> code, std::initializer_list<const char*> msg)
{
  kh_assertion_t* h = kh_init_assertion();

  int r;
  assert(code.size() == msg.size());
  auto m = msg.begin();
  for(auto c = code.begin(); c != code.end() && m != msg.end(); ++c, ++m)
  {
    auto iter = kh_put_assertion(h, *c, &r);
    kh_val(h, iter) = *m;
  }

  return h;
}

static kh_assertion_t* assertionhash = GenAssertions({
  ERR_WAT_INVALID_ALIGNMENT,
  ERR_INVALID_MEMORY_ALIGNMENT,
  ERR_INVALID_MEMORY_OFFSET,
  ERR_END_MISMATCH,
  ERR_PARSE_INVALID_MAGIC_COOKIE,
  ERR_PARSE_INVALID_VERSION,
  ERR_FATAL_OVERLONG_ENCODING,
  ERR_FATAL_INVALID_ENCODING,
  ERR_INVALID_RESERVED_VALUE,
  ERR_FATAL_TOO_MANY_LOCALS,
  ERR_IMPORT_EXPORT_MISMATCH,
  ERR_INVALID_BLOCK_SIGNATURE,
  ERR_IMPORT_EXPORT_MISMATCH,
  ERR_WAT_TYPE_MISMATCH,
  ERR_WAT_LABEL_MISMATCH,
  ERR_INVALID_BRANCH_DEPTH,
  ERR_INVALID_FUNCTION_INDEX,
  ERR_INVALID_TABLE_INDEX,
  ERR_WAT_OUT_OF_RANGE,
  ERR_PARSE_UNEXPECTED_EOF,
  },
{
  "alignment",
  "alignment must not be larger than natural",
  "out of bounds memory access",
  "unexpected end",
  "magic header not detected",
  "unknown binary version",
  "integer representation too long",
  "integer too large",
  "zero flag expected",
  "too many locals",
  "type mismatch",
  "type mismatch",
  "type mismatch",
  "type mismatch",
  "mismatching label",
  "unknown label",
  "unknown function",
  "unknown function 0",
  "constant out of range",
  "unexpected end",
  //"invalid section id", 
  //"length out of bounds",
  //"function and code section have inconsistent lengths", "data segment does not fit", "unknown memory 0", "elements segment does not fit",
  //"constant expression required", "duplicate export name", "unknown table", "unknown memory", "unknown operator", "unexpected token",
  //"undefined element", "unknown local", "invalid mutability", "incompatible import type", "unknown import", "integer overflow"
});

size_t GetMapping(kh_modules_t* mapping, const Token& t)
{
  khiter_t iter = kh_get_modules(mapping, string(t.pos, t.len).c_str());
  return kh_exist2(mapping, iter) ? kh_val(mapping, iter) : (size_t)~0;
}

jmp_buf jump_location;

void CrashHandler(int)
{
  longjmp(jump_location, 1);
}

int CompileScript(Environment& env, const char* out, void*& cache)
{
  int r;
  validate::ValidateEnvironment(env);
  if(env.errors)
    return ERR_VALIDATION_ERROR;
  return ERR_SUCCESS;
  if(r = CompileEnvironment(&env, out))
    return r;

  // Prepare to handle exceptions from the initialization
  auto dir = GetWorkingDir();
  dir.Append(out);
  cache = LoadDLL(dir.Get().c_str());
  return !cache ? ERR_RUNTIME_INIT_ERROR : ERR_SUCCESS;
}

struct WatResult
{
  WASM_TYPE_ENCODING type;
  union
  {
    int32_t i32;
    int64_t i64;
    float f32;
    double f64;
  };
};

template<WASM_TYPE_ENCODING R>
bool MatchFuncSig(FunctionSig sig)
{
  if(sig.n_params > 0)
    return false;
  if(R == TE_void && sig.n_returns > 0)
    return false;
  if(R != TE_void && (!sig.n_returns || sig.returns[0] != (varsint7)R))
    return false;
  return true;
}

template<WASM_TYPE_ENCODING R, WASM_TYPE_ENCODING P, WASM_TYPE_ENCODING... Args>
bool MatchFuncSig(FunctionSig sig)
{
  if(!sig.n_params)
    return false;
  if(sig.params[0] != (varsint7)P)
    return false;
  sig.params++;
  sig.n_params--;
  return MatchFuncSig<R, Args...>(sig);
}

void SetTempName(Environment& env, Module& m)
{
  char buf[30] = { 'm', 0 };
  ITOA(env.n_modules, buf + 1, 29, 10);
  size_t len = strlen(buf);
  m.name.resize(len, true);
  tmemcpy((char*)m.name.get(), len, buf, len);
}
int ParseWastModule(Environment& env, Queue<Token>& tokens, kh_modules_t* mapping, Module& m)
{
  EXPECTED(tokens, TOKEN_MODULE, ERR_WAT_EXPECTED_MODULE);
  int r;

  if(tokens[0].id == TOKEN_BINARY || (tokens.Size() > 1 && tokens[1].id == TOKEN_BINARY))
  {
    std::string tempname("m");
    tempname += std::to_string(env.n_modules);
    Token name = GetWatNameToken(tokens);
    if(!name.pos)
    {
      name.pos = tempname.data();
      name.len = tempname.size();
    }

    EXPECTED(tokens, TOKEN_BINARY, ERR_WAT_EXPECTED_TOKEN);
    ByteArray binary;
    while(tokens.Peek().id == TOKEN_STRING)
      if(r = WatString(binary, tokens.Pop()))
        return r;
    parse::Stream s = { binary.get(), binary.size(), 0 };
    if(r = parse::ParseModule(s, m, ByteArray((uint8_t*)name.pos, (varuint32)name.len)))
      return r;
    if(name.id == TOKEN_NAME) // Override name if it exists
      if(r = WatName(m.name, name))
        return r;
  }
  else if(tokens[0].id == TOKEN_QUOTE || (tokens.Size() > 1 && tokens[1].id == TOKEN_QUOTE))
  {
    Token name = GetWatNameToken(tokens);
    EXPECTED(tokens, TOKEN_QUOTE, ERR_WAT_EXPECTED_TOKEN);
    ByteArray quote;
    while(tokens.Peek().id == TOKEN_STRING)
      if(r = WatString(quote, tokens.Pop()))
        return r;
    if(r = ParseWatModule(env, m, quote.get(), quote.size(), StringRef{ name.pos, name.len }))
      return r;
    if(name.id == TOKEN_NAME) // Override name if it exists
      if(r = WatName(m.name, name))
        return r;
  }
  else if(r = WatModule(env, m, tokens, StringRef{ 0,0 }))
    return r;

  if(m.name.size() > 0)
  {
    khiter_t iter = kh_put_modules(mapping, m.name.str(), &r);
    if(!r)
      return ERR_FATAL_DUPLICATE_MODULE_NAME;
    kh_val(mapping, iter) = (varuint32)env.n_modules - 1;
  }
  else // If the module has no name, we must assign a temporary one
    SetTempName(env, m);
  return ERR_SUCCESS;
}

int64_t Homogenize(const Instruction& i)
{
  switch(i.opcode)
  {
  case OP_i32_const: return i.immediates[0]._varsint32;
  case OP_i64_const: return i.immediates[0]._varsint64;
  case OP_f32_const: { double d = i.immediates[0]._float32; return *(int64_t*)&d; }
  case OP_f64_const: return *(int64_t*)&i.immediates[0]._float64;
  }

  assert(false);
  return 0;
}

namespace innative {
  namespace internal {
    template<typename T>
    struct HType { typedef int64_t T; };
  }
}

template<typename... Args>
void GenFuncCall(void* f, WatResult& result, Args... params)
{
  int64_t r = static_cast<int64_t(*)(typename internal::HType<Args>::T...)>(f)(Homogenize(params)...);
  switch(result.type)
  {
  case TE_i32: result.i32 = (int32_t)r; break;
  case TE_i64: result.i64 = r; break;
  case TE_f32: { double d = *(double*)&r; result.f32 = (float)d; break; }
  case TE_f64: result.f64 = *(double*)&r; break;
  default: 
    assert(false); 
    result.type = TE_NONE;
  case TE_void:
    break;
  }
}

int ParseWastAction(Environment& env, Queue<Token>& tokens, kh_modules_t* mapping, Module*& last, void*& cache, WatResult& result)
{
  int r;
  if(!cache) // If cache is null we need to recompile the current environment
  {
    env.flags |= ENV_HOMOGENIZE_FUNCTIONS;
    if(r = CompileScript(env, "wast.dll", cache))
      return r;
    //assert(cache);
  }

  switch(tokens.Pop().id)
  {
  case TOKEN_INVOKE:
  {
    Token name = GetWatNameToken(tokens);
    Module* m = last;
    if(name.id == TOKEN_NAME)
    {
      size_t i = GetMapping(mapping, name);
      if(i >= env.n_modules)
        return ERR_PARSE_INVALID_NAME;
      m = env.modules + i;
    }
    if(!m)
      return ERR_FATAL_INVALID_MODULE;

    ByteArray func;
    if(r = WatString(func, tokens.Pop()))
      return r;

    khiter_t iter = kh_get_exports(m->exports, func.str());
    if(!kh_exist2(m->exports, iter))
      return ERR_INVALID_FUNCTION_INDEX;
    Export& e = m->exportsection.exports[kh_val(m->exports, iter)];
    if(e.kind != WASM_KIND_FUNCTION || e.index >= m->function.n_funcdecl || m->function.funcdecl[e.index] >= m->type.n_functions)
      return ERR_INVALID_FUNCTION_INDEX;

    // Dig up the exported function signature from the module and assemble a C function pointer from it
    FunctionSig& sig = m->type.functions[m->function.funcdecl[e.index]];

    std::vector<Instruction> params;
    while(tokens.Peek().id == TOKEN_OPEN)
    {
      WatState st(*m);
      params.emplace_back();
      EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
      if(r = WatInitializer(st, tokens, params.back()))
        return r;
      EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
    }

    if(params.size() != sig.n_params)
      return ERR_SIGNATURE_MISMATCH;
    for(varuint32 i = 0; i < sig.n_params; ++i)
    {
      varsint7 ty = TE_NONE;
      switch(params[i].opcode)
      {
      case OP_i32_const: ty = TE_i32; break;
      case OP_i64_const: ty = TE_i64; break;
      case OP_f32_const: ty = TE_f32; break;
      case OP_f64_const: ty = TE_f64; break;
      }

      if(sig.params[i] != ty) 
        return ERR_INVALID_TYPE;
    }

    void* f = LoadDLLFunction(cache, MergeName(m->name.str(), func.str()).c_str());
    if(!f)
      //return ERR_INVALID_FUNCTION_INDEX;
      return ERR_RUNTIME_TRAP;

    if(!sig.n_returns)
      result.type = TE_void;
    else
      result.type = (WASM_TYPE_ENCODING)sig.returns[0];

    // Call the function and set the correct result.
    signal(SIGILL, CrashHandler);
    int jmp = setjmp(jump_location);
    if(jmp)
      return ERR_RUNTIME_TRAP;

    switch(sig.n_params)
    {
    case 0: GenFuncCall(f, result); break;
    case 1: GenFuncCall(f, result, params[0]); break;
    case 2: GenFuncCall(f, result, params[0], params[1]); break;
    case 3: GenFuncCall(f, result, params[0], params[1], params[2]); break;
    case 4: GenFuncCall(f, result, params[0], params[1], params[2], params[3]); break;
    case 5: GenFuncCall(f, result, params[0], params[1], params[2], params[3], params[4]); break;
    case 6: GenFuncCall(f, result, params[0], params[1], params[2], params[3], params[4], params[5]); break;
    case 7: GenFuncCall(f, result, params[0], params[1], params[2], params[3], params[4], params[5], params[6]); break;
    case 8: GenFuncCall(f, result, params[0], params[1], params[2], params[3], params[4], params[5], params[6], params[7]); break;
    case 9: GenFuncCall(f, result, params[0], params[1], params[2], params[3], params[4], params[5], params[6], params[7], params[8]); break;
    default:
      assert(false);
      return ERR_FATAL_UNKNOWN_KIND;
    }
    
    signal(SIGILL, SIG_DFL);
    break;
  }
  case TOKEN_GET:
  {
    Token name = GetWatNameToken(tokens);
    Module* m = last;
    if(name.id == TOKEN_NAME)
    {
      size_t i = GetMapping(mapping, name);
      if(i >= env.n_modules)
        return ERR_PARSE_INVALID_NAME;
      m = env.modules + i;
    }
    if(!m)
      return ERR_FATAL_INVALID_MODULE;

    ByteArray global;
    if(r = WatString(global, tokens.Pop()))
      return r;

    khiter_t iter = kh_get_exports(m->exports, global.str());
    if(!kh_exist2(m->exports, iter))
      return ERR_INVALID_GLOBAL_INDEX;
    Export& e = m->exportsection.exports[kh_val(m->exports, iter)];
    if(e.kind != WASM_KIND_GLOBAL || e.index >= m->global.n_globals)
      return ERR_INVALID_GLOBAL_INDEX;

    void* f = LoadDLLFunction(cache, MergeName(m->name.str(), global.str()).c_str());
    if(!f)
      return ERR_INVALID_GLOBAL_INDEX;

    switch(m->global.globals[e.index].desc.type)
    {
    case TE_i32: result.i32 = *(int32_t*)f; break;
    case TE_i64: result.i64 = *(int64_t*)f; break;
    case TE_f32: result.f32 = *(float*)f; break;
    case TE_f64: result.f64 = *(double*)f; break;
    default: return ERR_INVALID_TYPE;
    }

    break;
  }
  default:
    return ERR_WAT_EXPECTED_TOKEN;
  }

  return ERR_SUCCESS;
}

bool WatIsNaN(float f, bool canonical)
{
  if(!isnan(f))
    return false;
  return ((*reinterpret_cast<uint32_t*>(&f) & 0b00000000010000000000000000000000U) != 0) != canonical;
}

bool WatIsNaN(double f, bool canonical)
{
  if(!isnan(f))
    return false;
  return ((*reinterpret_cast<uint64_t*>(&f) & 0b0000000000001000000000000000000000000000000000000000000000000000ULL) != 0) != canonical;
}

inline string GetAssertionString(int code)
{
  string assertcode = "[SUCCESS]";
  if(code < 0)
  {
    khiter_t iter = kh_get_assertion(assertionhash, code);
    if(!kh_exist2(assertionhash, iter))
    {
      assertcode = "[unknown error code ";
      char buf[32] = { 0 };
      ITOA(code, buf, 32, 16);
      assertcode += buf;
      assertcode += "]";
    }
    else
      assertcode = kh_val(assertionhash, iter);
  }
  return assertcode;
}

// This parses an entire extended WAT testing script into an environment
int innative::wat::ParseWast(Environment& env, uint8_t* data, size_t sz)
{
  Queue<Token> tokens;
  const char* start = (char*)data;
  TokenizeWAT(tokens, start, (char*)data + sz);
  ValidationError* errors = nullptr;

  for(size_t i = 0; i < tokens.Size(); ++i)
    if(!tokens[i].id)
      AppendError(errors, nullptr, ERR_WAT_INVALID_TOKEN, "[%zu] Invalid token: %s", WatLineNumber(start, tokens[i].pos), string(tokens[i].pos, tokens[i].len).c_str());

  if(env.errors)
    return ERR_WAT_INVALID_TOKEN;

  int r;
  kh_modules_t* mapping = kh_init_modules(); // This is a special mapping for all modules using the module name itself, not just registered ones.
  Module* last = 0; // For anything not providing a module name, this was the most recently defined module.
  void* cache = 0;

  while(tokens.Size() > 0 && tokens[0].id != TOKEN_CLOSE)
  {
    EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
    switch(tokens[0].id)
    {
    case TOKEN_MODULE:
    {
      Token t = tokens[0];
      env.modules = trealloc<Module>(env.modules, ++env.n_modules);
      last = &env.modules[env.n_modules - 1];

      if(r = ParseWastModule(env, tokens, mapping, *last))
        return r;
      validate::ValidateModule(env, *last);
      if(env.errors)
      {
        //validate::ValidateModule(env, *last);
        return ERR_VALIDATION_ERROR;
      }

      break;
    }
    case TOKEN_REGISTER:
    {
      tokens.Pop();
      size_t i = ~0;
      if(last)
        i = last - env.modules;
      if(tokens[0].id == TOKEN_NAME)
        i = GetMapping(mapping, tokens.Pop());
      if(i == (size_t)~0)
        return ERR_PARSE_INVALID_NAME;

      ByteArray name;
      if(r = WatString(name, tokens.Pop()))
        return r;
      khiter_t iter = kh_put_modules(env.modulemap, name.str(), &r);
      if(!r)
        return ERR_FATAL_DUPLICATE_MODULE_NAME;

      kh_val(env.modulemap, iter) = i;
      break;
    }
    case TOKEN_INVOKE:
    case TOKEN_GET:
    {
      Token t = tokens.Pop();
      EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
      WatResult result;
      if(r = ParseWastAction(env, tokens, mapping, last, cache, result))
      {
        if(r != ERR_RUNTIME_TRAP && r != ERR_RUNTIME_INIT_ERROR)
          return r;
        AppendError(errors, last, r, "[%zu] Runtime error %i while attempting to verify result.", WatLineNumber(start, t.pos), r);
      }
      EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      break;
    }
    case TOKEN_ASSERT_TRAP:
      Token t = tokens.Pop();
      if(tokens.Size() > 1 && tokens[0].id == TOKEN_OPEN && tokens[1].id == TOKEN_MODULE) // Check if we're actually trapping on a module load
      {
        EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
        env.modules = trealloc<Module>(env.modules, ++env.n_modules); // We temporarily add this module to the environment, but don't set the "last" module to it
        if(r = ParseWastModule(env, tokens, mapping, env.modules[env.n_modules - 1]))
          return r;
        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);

        r = CompileScript(env, "wast.dll", cache);
        --env.n_modules; // Remove the module from the environment to avoid poisoning other compilations
        if(r != ERR_RUNTIME_TRAP)
          AppendError(errors, 0, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected trap, but call succeeded", WatLineNumber(start, t.pos));
        EXPECTED(tokens, TOKEN_STRING, ERR_WAT_EXPECTED_STRING);
      }
      else
      {
        EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
        WatResult result;
        r = ParseWastAction(env, tokens, mapping, last, cache, result);
        if(r != ERR_RUNTIME_TRAP)
          AppendError(errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected trap, but call succeeded", WatLineNumber(start, t.pos));
        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
        EXPECTED(tokens, TOKEN_STRING, ERR_WAT_EXPECTED_STRING);
      }
      break;
    case TOKEN_ASSERT_RETURN:
    case TOKEN_ASSERT_RETURN_CANONICAL_NAN:
    case TOKEN_ASSERT_RETURN_ARITHMETIC_NAN:
    {
      Token t = tokens.Pop();
      EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
      WatResult result = { TE_NONE };
      if(r = ParseWastAction(env, tokens, mapping, last, cache, result))
      {
        if(r != ERR_RUNTIME_TRAP && r != ERR_RUNTIME_INIT_ERROR)
          return r;
        AppendError(errors, last, r, "[%zu] Runtime error %i while attempting to verify result.", WatLineNumber(start, t.pos), r);
      }
      EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      Instruction value;
      WatState state(*last);

      switch(t.id)
      {
      case TOKEN_ASSERT_RETURN:
        if(tokens[0].id == TOKEN_CLOSE) // This is valid because it represents a return of nothing
          value.opcode = OP_nop;
        else
        {
          EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
          if(r = WatInitializer(state, tokens, value))
            return r;
          EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
        }

        switch(value.opcode)
        {
        case OP_nop:
          if(result.type != TE_NONE)
            AppendError(errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected no return value but got %i", WatLineNumber(start, t.pos), result.type);
          break;
        case OP_i32_const:
          if(result.type != TE_i32)
            AppendError(errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected i32 type but got %i", WatLineNumber(start, t.pos), result.type);
          else if(result.i32 != value.immediates[0]._varsint32)
            AppendError(errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected %i but got %i", WatLineNumber(start, t.pos), value.immediates[0]._varsint32, result.i32);
          break;
        case OP_i64_const:
          if(result.type != TE_i64)
            AppendError(errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected i64 type but got %i", WatLineNumber(start, t.pos), result.type);
          else if(result.i64 != value.immediates[0]._varsint64)
            AppendError(errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected %i but got %i", WatLineNumber(start, t.pos), value.immediates[0]._varsint64, result.i64);
          break;
        case OP_f32_const:
          if(result.type != TE_f32)
            AppendError(errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected f32 type but got %i", WatLineNumber(start, t.pos), result.type);
          else if(result.f32 != value.immediates[0]._float32)
            AppendError(errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected %i but got %i", WatLineNumber(start, t.pos), value.immediates[0]._float32, result.f32);
          break;
        case OP_f64_const:
          if(result.type != TE_f64)
            AppendError(errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected f64 type but got %i", WatLineNumber(start, t.pos), result.type);
          else if(result.f64 != value.immediates[0]._float64)
            AppendError(errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected %i but got %i", WatLineNumber(start, t.pos), value.immediates[0]._float64, result.f64);
          break;
        }
        break;
      case TOKEN_ASSERT_RETURN_ARITHMETIC_NAN:
      case TOKEN_ASSERT_RETURN_CANONICAL_NAN:
        if(result.type != TE_f32 && result.type != TE_f64)
          AppendError(errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected canonical NaN but got unexpected integer %z", WatLineNumber(start, t.pos), result.i64);
        if(result.type == TE_f32 && !WatIsNaN(result.f32, t.id == TOKEN_ASSERT_RETURN_CANONICAL_NAN))
          AppendError(errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected canonical NaN but got %g", WatLineNumber(start, t.pos), result.f32);
        if(result.type == TE_f64 && !WatIsNaN(result.f64, t.id == TOKEN_ASSERT_RETURN_CANONICAL_NAN))
          AppendError(errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected canonical NaN but got %g", WatLineNumber(start, t.pos), result.f64);
        break;
      }
      break;
    }
    case TOKEN_ASSERT_MALFORMED:
    {
      Token t = tokens.Pop();
      EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
      Module m;
      int code = ParseWastModule(env, tokens, mapping, m);
      EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);

      ByteArray err;
      if(r = WatString(err, tokens.Pop()))
        return r;

      string assertcode = GetAssertionString(code);

      if(STRICMP(assertcode.c_str(), err.str()))
        AppendError(errors, 0, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected '%s' error, but got '%s' instead", WatLineNumber(start, t.pos), err.str(), assertcode.c_str());
      break;
    }
    case TOKEN_ASSERT_INVALID:
    case TOKEN_ASSERT_UNLINKABLE:
    {
      Token t = tokens.Pop();
      EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
      Module m;
      int code = ParseWastModule(env, tokens, mapping, m);
      EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);

      ByteArray err;
      if(r = WatString(err, tokens.Pop()))
        return r;
      
      string assertcode = GetAssertionString(code);

      if(code != ERR_SUCCESS)
        AppendError(errors, 0, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected module parsing success, but got '%s' instead", WatLineNumber(start, t.pos), assertcode.c_str());

      validate::ValidateModule(env, m);
      code = ERR_SUCCESS;
      if(env.errors)
        code = env.errors->code;
      assertcode = GetAssertionString(code);
      env.errors = 0;
      if(STRICMP(assertcode.c_str(), err.str()))
      {
        AppendError(errors, 0, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected '%s' error, but got '%s' instead", WatLineNumber(start, t.pos), err.str(), assertcode.c_str());
        validate::ValidateModule(env, m);
      }
      break;
    }
    case TOKEN_ASSERT_EXHAUSTION:
    {
      Token t = tokens.Pop();
      EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
      WatResult result;
      r = ParseWastAction(env, tokens, mapping, last, cache, result);
      if(r != ERR_RUNTIME_TRAP)
        AppendError(errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected call exhaustion trap, but call succeeded", WatLineNumber(start, t.pos));
      EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      EXPECTED(tokens, TOKEN_STRING, ERR_WAT_EXPECTED_STRING);
      break;
    }
    case TOKEN_SCRIPT:
    case TOKEN_INPUT:
    case TOKEN_OUTPUT:
    {
      assert(false);
      SkipSection(tokens);
      EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      break;
    }
    default:
      return ERR_WAT_EXPECTED_TOKEN;
    }

    EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }

  env.errors = errors;
  if(env.errors)
    internal::ReverseErrorList(env.errors);
  std::cout << "Finished Script" << std::endl;
  return ERR_SUCCESS;
}