// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in native-wasm.h

#include "wast.h"
#include "validate.h"
#include "compile.h"
#include "queue.h"
#include "parse.h"
#include <signal.h>
#include <setjmp.h>

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
  ERR_FATAL_INVALID_ENCODING,
  ERR_FATAL_INVALID_BYTE_LENGTH,
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
  },
{
  "alignment",
  "alignment",
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
  //"invalid section id", 
  //"length out of bounds",
  //"function and code section have inconsistent lengths", "data segment does not fit", "unknown memory 0", "elements segment does not fit",
  //"constant expression required", "duplicate export name", "unknown table", "unknown memory", "unknown operator", "unexpected token",
  //"undefined element", "unknown local", "invalid mutability", "incompatible import type", "unknown import", "integer overflow"
});

size_t GetMapping(kh_modules_t* mapping, const Token& t)
{
  khiter_t iter = kh_get_modules(mapping, std::string(t.pos, t.len).c_str());
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
  ValidateEnvironment(env);
  if(env.errors)
    return ERR_VALIDATION_ERROR;
  if(r = CompileEnvironment(&env, out))
    return r;

  // Prepare to handle exceptions from the initialization
  signal(SIGILL, CrashHandler);
  int jmp = setjmp(jump_location);
  if(jmp)
    return ERR_RUNTIME_INIT_ERROR;
  cache = LoadDLL(out);
  signal(SIGILL, SIG_DFL);
  return !cache ? ERR_RUNTIME_INIT_ERROR : ERR_SUCCESS;
}

struct WatResult
{
  TYPE_ENCODING type;
  union
  {
    int32_t i32;
    int64_t i64;
    float f32;
    double f64;
  };
};

template<TYPE_ENCODING R>
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

template<TYPE_ENCODING R, TYPE_ENCODING P, TYPE_ENCODING... Args>
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

int ParseWastModule(Environment& env, Queue<Token>& tokens, kh_modules_t* mapping, Module*& last, void*& cache)
{
  EXPECTED(tokens, TOKEN_MODULE, ERR_WAT_EXPECTED_MODULE);
  int r;

  cache = 0;
  env.modules = (Module*)realloc(env.modules, ++env.n_modules * sizeof(Module));
  last = &env.modules[env.n_modules - 1];
  if(tokens[0].id == TOKEN_BINARY || (tokens.Size() > 1 && tokens[1].id == TOKEN_BINARY))
  {
    Token name = GetWatNameToken(tokens);
    tokens.Pop();
    ByteArray binary = { 0 };
    if(r = WatString(binary, tokens.Pop()))
      return r;
    Stream s = { (uint8_t*)binary.bytes, binary.n_bytes, 0 };
    if(r = ParseModule(s, *last, ByteArray{ (varuint32)name.len, (uint8_t*)name.pos }))
      return r;
    if(name.id == TOKEN_NAME) // Override name if it exists
      if(r = WatName(last->name, name))
        return r;
  }
  else if(tokens[0].id == TOKEN_QUOTE || (tokens.Size() > 1 && tokens[1].id == TOKEN_QUOTE))
  {
    Token name = GetWatNameToken(tokens);
    tokens.Pop();
    Token t = tokens.Pop();
    if(r = ParseWatModule(env, *last, (uint8_t*)t.pos, t.len, StringRef{ name.pos, name.len }))
      return r;
    if(name.id == TOKEN_NAME) // Override name if it exists
      if(r = WatName(last->name, name))
        return r;
  }
  else if(r = WatModule(env, *last, tokens, StringRef{ 0,0 }))
    return r;

  if(last->name.n_bytes > 0)
  {
    khiter_t iter = kh_put_modules(mapping, (const char*)last->name.bytes, &r);
    if(!r)
      return ERR_FATAL_DUPLICATE_MODULE_NAME;
    kh_val(mapping, iter) = (varuint32)env.n_modules - 1;
  }

  return ERR_SUCCESS;
}

int ParseWastAction(Environment& env, Queue<Token>& tokens, kh_modules_t* mapping, Module*& last, void*& cache, WatResult& result)
{
  int r;
  if(!cache) // If cache is null we need to recompile the current environment
  {
    if(r = CompileScript(env, "wast.dll", cache))
      return r;
    assert(cache);
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

    ByteArray func = { 0 };
    if(r = WatString(func, tokens.Pop()))
      return r;

    khiter_t iter = kh_get_exports(m->exports, (const char*)func.bytes);
    if(!kh_exist2(m->exports, iter))
      return ERR_INVALID_FUNCTION_INDEX;
    Export& e = m->exportsection.exports[kh_val(m->exports, iter)];
    if(e.kind != KIND_FUNCTION || e.index >= m->function.n_funcdecl || m->function.funcdecl[e.index] >= m->type.n_functions)
      return ERR_INVALID_FUNCTION_INDEX;

    void* f = LoadDLLFunction(cache, MergeName((const char*)m->name.bytes, (const char*)func.bytes).c_str());
    if(!f)
      return ERR_INVALID_FUNCTION_INDEX;

    // Dig up the exported function signature from the module and assemble a C function pointer from it
    FunctionSig& sig = m->type.functions[m->function.funcdecl[e.index]];

    if(!sig.n_returns)
      result.type = TE_void;
    else
      result.type = (TYPE_ENCODING)sig.returns[0];

    // Call the function and set the correct result.
    signal(SIGILL, CrashHandler);
    int jmp = setjmp(jump_location);
    if(jmp)
      return ERR_RUNTIME_TRAP;

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

    if(MatchFuncSig<TE_i32, TE_i32>(sig))
      result.i32 = static_cast<int32_t(*)(int32_t)>(f)(params[0].immediates[0]._varsint32);
    else if(MatchFuncSig<TE_i64, TE_i32>(sig))
      result.i64 = static_cast<int64_t(*)(int32_t)>(f)(params[0].immediates[0]._varsint32);
    else if(MatchFuncSig<TE_f32, TE_i32>(sig))
      result.f32 = static_cast<float(*)(int32_t)>(f)(params[0].immediates[0]._varsint32);
    else if(MatchFuncSig<TE_f64, TE_i32>(sig))
      result.f64 = static_cast<double(*)(int32_t)>(f)(params[0].immediates[0]._varsint32);
    else if(MatchFuncSig<TE_i32, TE_i64>(sig))
      result.i32 = static_cast<int32_t(*)(int64_t)>(f)(params[0].immediates[0]._varsint64);
    else if(MatchFuncSig<TE_i64, TE_i64>(sig))
      result.i64 = static_cast<int64_t(*)(int64_t)>(f)(params[0].immediates[0]._varsint64);
    else if(MatchFuncSig<TE_f32, TE_i64>(sig))
      result.f32 = static_cast<float(*)(int64_t)>(f)(params[0].immediates[0]._varsint64);
    else if(MatchFuncSig<TE_f64, TE_i64>(sig))
      result.f64 = static_cast<double(*)(int64_t)>(f)(params[0].immediates[0]._varsint64);
    else if(MatchFuncSig<TE_i32, TE_f32>(sig))
      result.i32 = static_cast<int32_t(*)(float)>(f)(params[0].immediates[0]._float32);
    else if(MatchFuncSig<TE_i64, TE_f32>(sig))
      result.i64 = static_cast<int64_t(*)(float)>(f)(params[0].immediates[0]._float32);
    else if(MatchFuncSig<TE_f32, TE_f32>(sig))
      result.f32 = static_cast<float(*)(float)>(f)(params[0].immediates[0]._float32);
    else if(MatchFuncSig<TE_f64, TE_f32>(sig))
      result.f64 = static_cast<double(*)(float)>(f)(params[0].immediates[0]._float32);
    else if(MatchFuncSig<TE_i32, TE_f64>(sig))
      result.i32 = static_cast<int32_t(*)(double)>(f)(params[0].immediates[0]._float64);
    else if(MatchFuncSig<TE_i64, TE_f64>(sig))
      result.i64 = static_cast<int64_t(*)(double)>(f)(params[0].immediates[0]._float64);
    else if(MatchFuncSig<TE_f32, TE_f64>(sig))
      result.f32 = static_cast<float(*)(double)>(f)(params[0].immediates[0]._float64);
    else if(MatchFuncSig<TE_f64, TE_f64>(sig))
      result.f64 = static_cast<double(*)(double)>(f)(params[0].immediates[0]._float64);
    else if(MatchFuncSig<TE_i32, TE_i32, TE_i32>(sig))
      result.i32 = static_cast<int32_t(*)(int32_t, int32_t)>(f)(params[0].immediates[0]._varsint32, params[0].immediates[0]._varsint32);
    else if(MatchFuncSig<TE_i64, TE_i32, TE_i32>(sig))
      result.i64 = static_cast<int64_t(*)(int32_t, int32_t)>(f)(params[0].immediates[0]._varsint32, params[0].immediates[0]._varsint32);
    else if(MatchFuncSig<TE_f32, TE_i32, TE_i32>(sig))
      result.f32 = static_cast<float(*)(int32_t, int32_t)>(f)(params[0].immediates[0]._varsint32, params[0].immediates[0]._varsint32);
    else if(MatchFuncSig<TE_f64, TE_i32, TE_i32>(sig))
      result.f64 = static_cast<double(*)(int32_t, int32_t)>(f)(params[0].immediates[0]._varsint32, params[0].immediates[0]._varsint32);
    else if(MatchFuncSig<TE_f32, TE_f32, TE_f32>(sig))
      result.f32 = static_cast<float(*)(float, float)>(f)(params[0].immediates[0]._float32, params[0].immediates[0]._float32);
    else if(MatchFuncSig<TE_f64, TE_f64, TE_f64>(sig))
      result.f64 = static_cast<double(*)(double, double)>(f)(params[0].immediates[0]._float64, params[0].immediates[0]._float64);
    else if(MatchFuncSig<TE_i64, TE_i64, TE_i64>(sig))
      result.i64 = static_cast<int64_t(*)(int64_t, int64_t)>(f)(params[0].immediates[0]._varsint64, params[0].immediates[0]._varsint64);
    else
      assert(false);

    signal(SIGILL, SIG_DFL);
    break;
  }
  case TOKEN_GET:
    assert(false); // TODO: We have no way of getting globals out of DLLs yet
    break;
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

inline std::string GetAssertionString(int code)
{
  std::string assertcode = "[SUCCESS]";
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
int ParseWast(Environment& env, uint8_t* data, size_t sz)
{
  Queue<Token> tokens;
  TokenizeWAT(tokens, (char*)data, (char*)data + sz);
  ValidationError* errors;

  for(size_t i = 0; i < tokens.Size(); ++i)
    if(!tokens[i].id)
      AppendError(errors, nullptr, ERR_WAT_INVALID_TOKEN, "Invalid token: %s", std::string(tokens[i].pos, tokens[i].len).c_str());

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
      if(r = ParseWastModule(env, tokens, mapping, last, cache))
        return r;
      break;
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

      ByteArray name = { 0 };
      if(r = WatString(name, tokens.Pop()))
        return r;
      khiter_t iter = kh_put_modules(env.modulemap, (const char*)name.bytes, &r);
      if(!r)
        return ERR_FATAL_DUPLICATE_MODULE_NAME;

      kh_val(env.modulemap, iter) = i;
      break;
    }
    case TOKEN_INVOKE:
    case TOKEN_GET:
    {
      tokens.Pop();
      EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
      WatResult result;
      if(r = ParseWastAction(env, tokens, mapping, last, cache, result))
        return r;
      EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      break;
    }
    case TOKEN_ASSERT_TRAP:
      tokens.Pop();
      if(tokens.Size() > 1 && tokens[0].id == TOKEN_OPEN && tokens[1].id == TOKEN_MODULE) // Check if we're actually trapping on a module load
      {
        EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
        if(r = ParseWastModule(env, tokens, mapping, last, cache))
          return r;
        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);

        r = CompileScript(env, "wast.dll", cache);
        if(r != ERR_RUNTIME_TRAP)
          return ERR_RUNTIME_ASSERT_FAILURE; // TODO: AppendError
      }
      else
      {
        EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
        WatResult result;
        r = ParseWastAction(env, tokens, mapping, last, cache, result);
        if(r != ERR_RUNTIME_TRAP)
          AppendError(errors, last, ERR_RUNTIME_ASSERT_FAILURE, "Expected trap, but call succeeded");
        return ERR_RUNTIME_ASSERT_FAILURE; // TODO: AppendError
        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      }
      break;
    case TOKEN_ASSERT_RETURN:
    case TOKEN_ASSERT_RETURN_CANONICAL_NAN:
    case TOKEN_ASSERT_RETURN_ARITHMETIC_NAN:
    {
      Token t = tokens.Pop();
      EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
      WatResult result;
      if(r = ParseWastAction(env, tokens, mapping, last, cache, result))
        return r;
      EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      Instruction value;
      WatState state(*last);

      switch(t.id)
      {
      case TOKEN_ASSERT_RETURN:
        if(r = WatInitializer(state, tokens, value))
          return r;
        switch(value.opcode)
        {
        case OP_i32_const:
          if(result.type != TE_i32)
            AppendError(errors, last, ERR_RUNTIME_ASSERT_FAILURE, "Expected i32 type but got %i", result.type);
          else if(result.i32 != value.immediates[0]._varsint32)
            AppendError(errors, last, ERR_RUNTIME_ASSERT_FAILURE, "Expected %i but got %i", value.immediates[0]._varsint32, result.i32);
          break;
        case OP_i64_const:
          if(result.type != TE_i64)
            AppendError(errors, last, ERR_RUNTIME_ASSERT_FAILURE, "Expected i64 type but got %i", result.type);
          else if(result.i64 != value.immediates[0]._varsint64)
            AppendError(errors, last, ERR_RUNTIME_ASSERT_FAILURE, "Expected %i but got %i", value.immediates[0]._varsint64, result.i64);
          break;
        case OP_f32_const:
          if(result.type != TE_f32)
            AppendError(errors, last, ERR_RUNTIME_ASSERT_FAILURE, "Expected f32 type but got %i", result.type);
          else if(result.f32 != value.immediates[0]._float32)
            AppendError(errors, last, ERR_RUNTIME_ASSERT_FAILURE, "Expected %i but got %i", value.immediates[0]._float32, result.f32);
          break;
        case OP_f64_const:
          if(result.type != TE_f64)
            AppendError(errors, last, ERR_RUNTIME_ASSERT_FAILURE, "Expected f64 type but got %i", result.type);
          else if(result.f64 != value.immediates[0]._float64)
            AppendError(errors, last, ERR_RUNTIME_ASSERT_FAILURE, "Expected %i but got %i", value.immediates[0]._float64, result.f64);
          break;
        }
        break;
      case TOKEN_ASSERT_RETURN_ARITHMETIC_NAN:
      case TOKEN_ASSERT_RETURN_CANONICAL_NAN:
        if(result.type != TE_f32 && result.type != TE_f64)
          AppendError(errors, last, ERR_RUNTIME_ASSERT_FAILURE, "Expected canonical NaN but got unexpected integer %z", result.i64);
        if(result.type == TE_f32 && !WatIsNaN(result.f32, t.id == TOKEN_ASSERT_RETURN_CANONICAL_NAN))
          AppendError(errors, last, ERR_RUNTIME_ASSERT_FAILURE, "Expected canonical NaN but got %g", result.f32);
        if(result.type == TE_f64 && !WatIsNaN(result.f64, t.id == TOKEN_ASSERT_RETURN_CANONICAL_NAN))
          AppendError(errors, last, ERR_RUNTIME_ASSERT_FAILURE, "Expected canonical NaN but got %g", result.f64);
        break;
      }
      break;
    }
    case TOKEN_ASSERT_MALFORMED:
    case TOKEN_ASSERT_INVALID:
    case TOKEN_ASSERT_UNLINKABLE:
    {
      Token t = tokens.Pop();
      EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
      int code = ParseWastModule(env, tokens, mapping, last, cache);
      EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);

      ByteArray err = { 0 };
      if(r = WatString(err, tokens.Pop()))
        return r;
      
      std::string assertcode = GetAssertionString(code);

      if(t.id == TOKEN_ASSERT_MALFORMED && STRICMP(assertcode.c_str(), (const char*)err.bytes))
        AppendError(errors, last, ERR_RUNTIME_ASSERT_FAILURE, "Expected '%s' error, but got '%s' instead", (const char*)err.bytes, assertcode.c_str());
      if(t.id != TOKEN_ASSERT_MALFORMED && code != ERR_SUCCESS)
        AppendError(errors, last, ERR_RUNTIME_ASSERT_FAILURE, "Expected module parsing success, but got '%s' instead", assertcode.c_str());

      ValidateModule(env, *last);
      code = ERR_SUCCESS;
      if(env.errors)
        code = env.errors->code;
      assertcode = GetAssertionString(code);
      if(t.id == TOKEN_ASSERT_MALFORMED && code != ERR_SUCCESS)
        AppendError(errors, last, ERR_RUNTIME_ASSERT_FAILURE, "Expected module parsing success, but got '%s' instead", assertcode.c_str());
      if(t.id != TOKEN_ASSERT_MALFORMED && STRICMP(assertcode.c_str(), (const char*)err.bytes))
        AppendError(errors, last, ERR_RUNTIME_ASSERT_FAILURE, "Expected '%s' error, but got '%s' instead", (const char*)err.bytes, assertcode.c_str());
      env.errors = 0;
      break;
    }
    case TOKEN_ASSERT_EXHAUSTION:
      assert(false);
      break;
    case TOKEN_SCRIPT:
    case TOKEN_INPUT:
    case TOKEN_OUTPUT:
    {
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
  return ERR_SUCCESS;
}