// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "wast.h"
#include "validate.h"
#include "compile.h"
#include "queue.h"
#include "parse.h"
#include <signal.h>
#include <setjmp.h>

using namespace innative;
using namespace wat;

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

int ParseWastModule(Environment& env, Queue<Token>& tokens, kh_modules_t* mapping, Module*& last, void*& cache)
{
  EXPECTED(tokens, TOKEN_MODULE, ERR_WAT_EXPECTED_MODULE);
  int r;

  cache = 0;
  env.modules = trealloc<Module>(env.modules, ++env.n_modules);
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
  else // If the module has no name, we must assign a temporary one
  {
    char buf[30] = { 'm', 0 };
    ITOA(env.n_modules, buf + 1, 29, 10);
    size_t len = strlen(buf);
    last->name.n_bytes = len;
    last->name.bytes = tmalloc<uint8_t>(len + 1);
    tmemcpy((char*)last->name.bytes, len + 1, buf, len + 1);
  }
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

template<typename T>
struct HType { typedef int64_t T; };

template<typename... Args>
void GenFuncCall(void* f, WatResult& result, Args... params)
{
  int64_t r = static_cast<int64_t(*)(typename HType<Args>::T...)>(f)(Homogenize(params)...);
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
    if(e.kind != WASM_KIND_FUNCTION || e.index >= m->function.n_funcdecl || m->function.funcdecl[e.index] >= m->type.n_functions)
      return ERR_INVALID_FUNCTION_INDEX;

    void* f = LoadDLLFunction(cache, MergeName((const char*)m->name.bytes, (const char*)func.bytes).c_str());
    if(!f)
      return ERR_INVALID_FUNCTION_INDEX;

    // Dig up the exported function signature from the module and assemble a C function pointer from it
    FunctionSig& sig = m->type.functions[m->function.funcdecl[e.index]];

    if(!sig.n_returns)
      result.type = TE_void;
    else
      result.type = (WASM_TYPE_ENCODING)sig.returns[0];

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
    for(int i = 0; i < sig.n_params; ++i)
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

    ByteArray global = { 0 };
    if(r = WatString(global, tokens.Pop()))
      return r;

    khiter_t iter = kh_get_exports(m->exports, (const char*)global.bytes);
    if(!kh_exist2(m->exports, iter))
      return ERR_INVALID_GLOBAL_INDEX;
    Export& e = m->exportsection.exports[kh_val(m->exports, iter)];
    if(e.kind != WASM_KIND_GLOBAL || e.index >= m->global.n_globals)
      return ERR_INVALID_GLOBAL_INDEX;

    void* f = LoadDLLFunction(cache, MergeName((const char*)m->name.bytes, (const char*)global.bytes).c_str());
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
int innative::wat::ParseWast(Environment& env, uint8_t* data, size_t sz)
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
      ValidateModule(env, *last);
      if(env.errors)
        return ERR_VALIDATION_ERROR;
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
          AppendError(errors, last, ERR_RUNTIME_ASSERT_FAILURE, "Expected trap, but call succeeded");
        EXPECTED(tokens, TOKEN_STRING, ERR_WAT_EXPECTED_STRING);
      }
      else
      {
        EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
        WatResult result;
        r = ParseWastAction(env, tokens, mapping, last, cache, result);
        if(r != ERR_RUNTIME_TRAP)
          AppendError(errors, last, ERR_RUNTIME_ASSERT_FAILURE, "Expected trap, but call succeeded");
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
        AppendError(errors, last, r, "Runtime error %i while attempting to verify result.", r);
      }
      EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      Instruction value;
      WatState state(*last);

      switch(t.id)
      {
      case TOKEN_ASSERT_RETURN:
        EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
        if(r = WatInitializer(state, tokens, value))
          return r;
        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);

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
      Token test = tokens.Pop();
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

    Token t = tokens[0];
    EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }

  env.errors = errors;
  return ERR_SUCCESS;
}