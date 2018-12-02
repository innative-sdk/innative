// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "wast.h"
#include "validate.h"
#include "compile.h"
#include "tools.h"
#include "queue.h"
#include "parse.h"
#include "innative/export.h"
#include <signal.h>
#include <setjmp.h>
#include <iostream>
#include <atomic>

using namespace innative;
using namespace wat;
using namespace utility;
using std::string;

KHASH_INIT(assertion, int, const char*, 1, kh_int_hash_func, kh_int_hash_equal)
KHASH_INIT(stringmap, const char*, const char*, 1, kh_str_hash_funcins, kh_str_hash_insequal)

namespace innative {
  namespace wat {
    kh_assertion_t* GenWastAssertions(std::initializer_list<int> code, std::initializer_list<const char*> msg)
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

    static kh_assertion_t* assertionhash = GenWastAssertions({
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
      ERR_EMPTY_VALUE_STACK,
      ERR_INVALID_VALUE_STACK,
      ERR_INVALID_TYPE,
      ERR_WAT_TYPE_MISMATCH,
      ERR_WAT_UNKNOWN_TYPE,
      ERR_WAT_LABEL_MISMATCH,
      ERR_INVALID_BRANCH_DEPTH,
      ERR_INVALID_FUNCTION_INDEX,
      ERR_INVALID_TABLE_INDEX,
      ERR_WAT_OUT_OF_RANGE,
      ERR_PARSE_UNEXPECTED_EOF,
      ERR_WAT_EXPECTED_OPERATOR,
      ERR_WAT_UNEXPECTED_NAME,
      ERR_PARSE_INVALID_FILE_LENGTH,
      ERR_FATAL_UNKNOWN_SECTION,
      ERR_FUNCTION_BODY_MISMATCH,
      ERR_FATAL_DUPLICATE_EXPORT,
      ERR_INVALID_MEMORY_INDEX,
      ERR_INVALID_GLOBAL_INDEX,
      ERR_INVALID_TABLE_INDEX,
      ERR_WAT_INVALID_NUMBER,
      ERR_WAT_INVALID_TOKEN,
      ERR_INVALID_INITIALIZER,
      ERR_INVALID_INITIALIZER_TYPE,
      ERR_INVALID_GLOBAL_TYPE,
      ERR_INVALID_TABLE_TYPE,
      ERR_INVALID_LOCAL_INDEX,
      ERR_MULTIPLE_RETURN_VALUES,
      ERR_INVALID_START_FUNCTION,
      ERR_WAT_EXPECTED_VAR,
      ERR_WAT_EXPECTED_VALTYPE,
      ERR_INVALID_UTF8_ENCODING,
      ERR_INVALID_DATA_SEGMENT,
      ERR_INVALID_TABLE_OFFSET,
      ERR_IMMUTABLE_GLOBAL,
      ERR_INVALID_MUTABILITY,
      ERR_UNKNOWN_EXPORT,
      ERR_INVALID_GLOBAL_IMPORT_TYPE,
      ERR_INVALID_FUNCTION_IMPORT_TYPE,
      ERR_WAT_INVALID_IMPORT_ORDER,
      ERR_INVALID_IMPORT_MEMORY_MINIMUM,
      ERR_INVALID_IMPORT_MEMORY_MAXIMUM,
      ERR_INVALID_IMPORT_TABLE_MINIMUM,
      ERR_INVALID_IMPORT_TABLE_MAXIMUM,
      ERR_MULTIPLE_TABLES,
      ERR_MULTIPLE_MEMORIES,
      ERR_IMPORT_EXPORT_TYPE_MISMATCH,
      ERR_UNKNOWN_MODULE,
      ERR_INVALID_LIMITS,
      ERR_MEMORY_MAXIMUM_TOO_LARGE,
      ERR_MEMORY_MINIMUM_TOO_LARGE,
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
  "type mismatch",
  "inline function type",
  "unknown type",
  "mismatching label",
  "unknown label",
  "unknown function",
  "unknown table",
  "constant out of range",
  "unexpected end",
  "unexpected token",
  "unexpected token",
  "unexpected end",
  "invalid section id",
  "function and code section have inconsistent lengths",
  "duplicate export name",
  "unknown memory",
  "unknown global",
  "unknown table",
  "unknown operator",
  "unknown operator",
  "constant expression required",
  "type mismatch",
  "type mismatch",
  "type mismatch",
  "unknown local",
  "invalid result arity",
  "start function",
  "unknown operator",
  "unexpected token",
  "invalid UTF-8 encoding",
  "data segment does not fit",
  "elements segment does not fit",
  "global is immutable",
  "invalid mutability",
  "unknown import",
  "incompatible import type",
  "incompatible import type",
  "invalid import order",
  "incompatible import type",
  "incompatible import type",
  "incompatible import type",
  "incompatible import type",
  "multiple tables",
  "multiple memories",
  "incompatible import type",
  "unknown import",
  "size minimum must not be greater than maximum",
  "memory size must be at most 65536 pages (4GiB)",
  "memory size must be at most 65536 pages (4GiB)",
});

    kh_stringmap_t* GenWastStringMap(std::initializer_list<const char*> map)
    {
      kh_stringmap_t* h = kh_init_stringmap();

      int r;
      assert(!(map.size() % 2));
      for(auto m = map.begin(); m != map.end(); ++m)
      {
        auto iter = kh_put_stringmap(h, *m, &r);
        kh_val(h, iter) = *++m;
      }

      return h;
    }

    static kh_stringmap_t* assertmap = GenWastStringMap({
        "unknown function 0", "unknown function",
        "unknown memory 0", "unknown memory",
        "unknown table 0", "unknown table",
        "i32 constant", "constant out of range",
        "length out of bounds", "unexpected end",
        "import after function", "invalid import order",
        "import after global", "invalid import order",
        "import after table", "invalid import order",
        "import after memory", "invalid import order",
      });

    size_t GetWastMapping(kh_indexname_t* mapping, const WatToken& t)
    {
      khiter_t iter = kh_get_indexname(mapping, StringRef{ t.pos, t.len });
      return kh_exist2(mapping, iter) ? kh_val(mapping, iter) : (size_t)~0;
    }

    struct WastResult
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

    jmp_buf jump_location;

    void WastCrashHandler(int sig)
    {
      longjmp(jump_location, 1);
    }
  }
}

void InvalidateCache(void*& cache)
{
  if(cache)
    FreeDLL(cache);
  cache = nullptr;
}

int CompileWast(Environment& env, const char* out, void*& cache)
{
  InvalidateCache(cache);

  int err;
  ValidateEnvironment(env);
  if(env.errors)
    return ERR_VALIDATION_ERROR;
  if(err = CompileEnvironment(&env, out))
    return err;

  auto dir = GetWorkingDir();
  dir.Append(out);

  signal(SIGILL, WastCrashHandler);
  signal(SIGFPE, WastCrashHandler); // This catches division by zero on linux

  if(setjmp(jump_location) != 0)
    return ERR_RUNTIME_TRAP;

  cache = LoadDLL(dir.Get().c_str());
  if(!cache)
    return ERR_RUNTIME_INIT_ERROR;

  auto entry = LoadFunction(cache, 0, 0);
  if(!entry)
    return ERR_RUNTIME_INIT_ERROR;

  (*entry)();
  return ERR_SUCCESS;
}

void SetTempName(Environment& env, Module& m)
{
  static std::atomic_size_t modcount(1); // We can't use n_modules in case a module is malformed

  auto buf = std::string("m") + std::to_string(modcount.fetch_add(1, std::memory_order::memory_order_relaxed));
  m.name.resize(buf.size(), true, env);
  tmemcpy((char*)m.name.get(), m.name.size(), buf.data(), buf.size());
}

int ParseWastModule(Environment& env, Queue<WatToken>& tokens, kh_indexname_t* mapping, Module& m, const char* path)
{
  EXPECTED(tokens, TOKEN_MODULE, ERR_WAT_EXPECTED_MODULE);
  int err;
  WatToken name = { TOKEN_NONE };
  m = { 0 }; // We have to ensure this is zeroed, because an error could occur before ParseModule is called

  if(tokens[0].id == TOKEN_BINARY || (tokens.Size() > 1 && tokens[1].id == TOKEN_BINARY))
  {
    std::string tempname("m");
    tempname += std::to_string(env.n_modules);
    name = GetWatNameToken(tokens);
    if(!name.pos)
    {
      name.pos = tempname.data();
      name.len = tempname.size();
    }

    EXPECTED(tokens, TOKEN_BINARY, ERR_WAT_EXPECTED_BINARY);
    ByteArray binary;
    while(tokens.Peek().id == TOKEN_STRING)
      if(err = WatString(env, binary, tokens.Pop()))
        return err;
    Stream s = { binary.get(), binary.size(), 0 };
    if(err = ParseModule(s, env, m, ByteArray((uint8_t*)name.pos, (varuint32)name.len), env.errors))
      return err;
    if(name.id == TOKEN_NAME) // Override name if it exists
      if(err = WatName(env, m.name, name))
        return err;
  }
  else if(tokens[0].id == TOKEN_QUOTE || (tokens.Size() > 1 && tokens[1].id == TOKEN_QUOTE))
  {
    name = GetWatNameToken(tokens);
    EXPECTED(tokens, TOKEN_QUOTE, ERR_WAT_EXPECTED_QUOTE);
    ByteArray quote;
    while(tokens.Peek().id == TOKEN_STRING)
      if(err = WatString(env, quote, tokens.Pop()))
        return err;
    if(err = ParseWatModule(env, m, quote.get(), quote.size(), StringRef{ name.pos, name.len }))
      return err;
    if(name.id == TOKEN_NAME) // Override name if it exists
      if(err = WatName(env, m.name, name))
        return err;
  }
  else if(err = WatModule(env, m, tokens, StringRef{ nullptr, 0 }, name))
    return err;
  m.path = path;

  if(name.id == TOKEN_NAME) // Only add this to our name mapping if an actual name token was specified, regardless of whether the module has a name.
  {
    int r;
    khiter_t iter = kh_put_indexname(mapping, { name.pos, name.len }, &r);
    if(!r)
      return ERR_FATAL_DUPLICATE_MODULE_NAME;
    kh_val(mapping, iter) = (varuint32)env.n_modules - 1;
  }
  else // If the module has no name, we must assign a temporary one
    SetTempName(env, m);
  return ERR_SUCCESS;
}

template<int I, typename... Args>
struct GenWastFunction
{
  inline static void Call(void* f, WastResult& result, const Instruction* param, Args... args)
  {
    switch(param[I - 1].opcode)
    {
    case OP_i32_const: return GenWastFunction<I - 1, int32_t, Args...>::Call(f, result, param, param[I - 1].immediates[0]._varsint32, args...);
    case OP_i64_const: return GenWastFunction<I - 1, int64_t, Args...>::Call(f, result, param, param[I - 1].immediates[0]._varsint64, args...);
    case OP_f32_const: return GenWastFunction<I - 1, float, Args...>::Call(f, result, param, param[I - 1].immediates[0]._float32, args...);
    case OP_f64_const: return GenWastFunction<I - 1, double, Args...>::Call(f, result, param, param[I - 1].immediates[0]._float64, args...);
    }
    assert(false);
  }
};

template<typename... Args>
struct GenWastFunction<0, Args...>
{
  inline static void Call(void* f, WastResult& result, const Instruction* param, Args... args)
  {
    switch(result.type)
    {
    case TE_i32: result.i32 = ((int32_t(*)(Args...))(f))(args...); break;
    case TE_i64: result.i64 = ((int64_t(*)(Args...))(f))(args...); break;
    case TE_f32: result.f32 = ((float(*)(Args...))(f))(args...); break;
    case TE_f64: result.f64 = ((double(*)(Args...))(f))(args...); break;
    default:
      assert(false);
      result.type = TE_NONE;
    case TE_void:
      ((void(*)(Args...))(f))(args...);
      break;
    }
  }
};

int64_t Homogenize(const Instruction& i)
{
  switch(i.opcode)
  {
  case OP_i32_const: return i.immediates[0]._varsint32;
  case OP_i64_const: return i.immediates[0]._varsint64;
  case OP_f32_const: return i.immediates[0]._varsint32;
  case OP_f64_const: return i.immediates[0]._varsint64;
  }

  assert(false);
  return 0;
}

namespace innative {
  namespace internal {
    template<typename X>
    struct HType { typedef int64_t T; };
  }
}

template<typename... Args>
void GenWastFunctionCall(void* f, WastResult& result, Args... params)
{
  int64_t r = reinterpret_cast<int64_t(*)(typename internal::HType<Args>::T...)>(f)(Homogenize(params)...);
  switch(result.type)
  {
  case TE_i32:
  case TE_f32: 
  case TE_f64:
  case TE_i64: result.i64 = r; break;
  default:
    assert(false);
    result.type = TE_NONE;
  case TE_void:
    break;
  }
}

int ParseWastAction(Environment& env, Queue<WatToken>& tokens, kh_indexname_t* mapping, Module*& last, void*& cache, int& counter, const std::string& path, WastResult& result)
{
  int err;
  int cache_err;
  if(!cache) // If cache is null we need to recompile the current environment, but we can't bail on error messages yet or we'll corrupt the parse
    cache_err = CompileWast(env, (path + std::to_string(counter++) + IR_LIBRARY_EXTENSION).c_str(), cache);

  switch(tokens.Pop().id)
  {
  case TOKEN_INVOKE:
  {
    WatToken name = GetWatNameToken(tokens);
    Module* m = last;
    if(name.id == TOKEN_NAME)
    {
      size_t i = GetWastMapping(mapping, name);
      if(i >= env.n_modules)
        return ERR_PARSE_INVALID_NAME;
      m = env.modules + i;
    }
    if(!m)
      return ERR_FATAL_INVALID_MODULE;

    ByteArray func;
    if(err = WatString(env, func, tokens.Pop()))
      return err;

    khiter_t iter = kh_get_exports(m->exports, func);
    if(!kh_exist2(m->exports, iter))
      return ERR_INVALID_FUNCTION_INDEX;
    Export& e = m->exportsection.exports[kh_val(m->exports, iter)];

    // Dig up the exported function signature from the module and assemble a C function pointer from it
    FunctionType* ftype = (e.kind != WASM_KIND_FUNCTION) ? nullptr : ModuleFunction(*m, e.index);
    if(!ftype)
      return ERR_INVALID_FUNCTION_INDEX;

    std::vector<Instruction> params;
    while(tokens.Peek().id == TOKEN_OPEN)
    {
      WatState st(env, *m);
      params.emplace_back();
      EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
      if(err = WatInitializer(st, tokens, params.back()))
        return err;
      EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
    }

    if(params.size() != ftype->n_params)
      return ERR_SIGNATURE_MISMATCH;
    for(varuint32 i = 0; i < ftype->n_params; ++i)
    {
      varsint7 ty = TE_NONE;
      switch(params[i].opcode)
      {
      case OP_i32_const: ty = TE_i32; break;
      case OP_i64_const: ty = TE_i64; break;
      case OP_f32_const: ty = TE_f32; break;
      case OP_f64_const: ty = TE_f64; break;
      }

      if(ftype->params[i] != ty)
        return ERR_INVALID_TYPE;
    }

    if(cache_err != 0)
      return cache_err;
    assert(cache);
    void* f = reinterpret_cast<void*>(LoadFunction(cache, m->name.str(), func.str()));
    if(!f)
      return ERR_INVALID_FUNCTION_INDEX;

    if(!ftype->n_returns)
      result.type = TE_void;
    else
      result.type = (WASM_TYPE_ENCODING)ftype->returns[0];

    // Call the function and set the correct result.
    signal(SIGILL, WastCrashHandler);
    signal(SIGFPE, WastCrashHandler); // This catches division by zero on linux

    if(setjmp(jump_location) != 0)
      return ERR_RUNTIME_TRAP;

#ifdef IR_COMPILER_MSC
    __try // this catches division by zero on windows
    {
#endif
      if(env.flags&ENV_HOMOGENIZE_FUNCTIONS)
      {
        switch(ftype->n_params)
        {
        case 0: GenWastFunctionCall(f, result); break;
        case 1: GenWastFunctionCall(f, result, params[0]); break;
        case 2: GenWastFunctionCall(f, result, params[0], params[1]); break;
        case 3: GenWastFunctionCall(f, result, params[0], params[1], params[2]); break;
        case 4: GenWastFunctionCall(f, result, params[0], params[1], params[2], params[3]); break;
        case 5: GenWastFunctionCall(f, result, params[0], params[1], params[2], params[3], params[4]); break;
        case 6: GenWastFunctionCall(f, result, params[0], params[1], params[2], params[3], params[4], params[5]); break;
        case 7: GenWastFunctionCall(f, result, params[0], params[1], params[2], params[3], params[4], params[5], params[6]); break;
        case 8: GenWastFunctionCall(f, result, params[0], params[1], params[2], params[3], params[4], params[5], params[6], params[7]); break;
        case 9: GenWastFunctionCall(f, result, params[0], params[1], params[2], params[3], params[4], params[5], params[6], params[7], params[8]); break;
        default:
          assert(false);
          return ERR_FATAL_UNKNOWN_KIND;
        }
      }
      else
      {
        switch(ftype->n_params)
        {
        case 0: GenWastFunction<0>::Call(f, result, params.data()); break;
        case 1: GenWastFunction<1>::Call(f, result, params.data()); break;
        case 2: GenWastFunction<2>::Call(f, result, params.data()); break;
        case 3: GenWastFunction<3>::Call(f, result, params.data()); break;
        default:
          assert(false);
          return ERR_FATAL_UNKNOWN_KIND;
        }
      }
#ifdef IR_COMPILER_MSC
    }
    __except(1)
    {
      return ERR_RUNTIME_TRAP;
    }
#endif
    signal(SIGILL, SIG_DFL);
    break;
  }
  case TOKEN_GET:
  {
    WatToken name = GetWatNameToken(tokens);
    Module* m = last;
    if(name.id == TOKEN_NAME)
    {
      size_t i = GetWastMapping(mapping, name);
      if(i >= env.n_modules)
        return ERR_PARSE_INVALID_NAME;
      m = env.modules + i;
    }
    if(!m)
      return ERR_FATAL_INVALID_MODULE;

    ByteArray global;
    if(err = WatString(env, global, tokens.Pop()))
      return err;

    khiter_t iter = kh_get_exports(m->exports, global);
    if(!kh_exist2(m->exports, iter))
      return ERR_INVALID_GLOBAL_INDEX;
    Export& e = m->exportsection.exports[kh_val(m->exports, iter)];
    GlobalDesc* g = nullptr;
    if(e.kind != WASM_KIND_GLOBAL || !(g = ModuleGlobal(*m, e.index)))
      return ERR_INVALID_GLOBAL_INDEX;

    if(cache_err != 0)
      return cache_err;
    assert(cache);
    void* f = innative::LoadGlobal(cache, m->name.str(), global.str());
    if(!f)
      return ERR_INVALID_GLOBAL_INDEX;

    switch(g->type)
    {
    case TE_i32: result.i32 = *(int32_t*)f; result.type = TE_i32; break;
    case TE_i64: result.i64 = *(int64_t*)f; result.type = TE_i64; break;
    case TE_f32: result.f32 = *(float*)f; result.type = TE_f32; break;
    case TE_f64: result.f64 = *(double*)f; result.type = TE_f64; break;
    default: return ERR_INVALID_TYPE;
    }

    break;
  }
  default:
    return ERR_WAT_EXPECTED_TOKEN;
  }

  return ERR_SUCCESS;
}

bool WastIsNaN(float f, bool canonical)
{
  if(!isnan(f))
    return false;
  union { float f; uint32_t i; } u = { f };
  return ((u.i & 0x200000U) != 0) != canonical;
}

bool WastIsNaN(double f, bool canonical)
{
  if(!isnan(f))
    return false;
  union { double f; uint64_t i; } u = { f };
  return ((u.i & 0x4000000000000ULL) != 0) != canonical;
}

inline string GetAssertionString(int code)
{
  string assertcode = "[SUCCESS]";
  if(code < 0)
  {
    khiter_t iter = kh_get_assertion(assertionhash, code);
    if(!kh_exist2(assertionhash, iter))
      assertcode = "[unknown error code " + std::to_string(code) + "]";
    else
      assertcode = kh_val(assertionhash, iter);
  }
  return assertcode;
}

inline const char* MapAssertionString(const char* s)
{
  khiter_t i = kh_get_stringmap(assertmap, s);
  if(kh_exist2(assertmap, i))
    return kh_val(assertmap, i);
  return s;
}

// This parses an entire extended WAT testing script into an environment
int innative::wat::ParseWast(Environment& env, const uint8_t* data, size_t sz, const char* path, bool always_compile)
{
  Queue<WatToken> tokens;
  const char* start = (const char*)data;
  TokenizeWAT(tokens, start, (const char*)data + sz);
  ValidationError* errors = nullptr;
  int counter = 0; // Even if we unload wast.dll, visual studio will keep the .pdb open forever, so we have to generate new DLLs for each new test section.
  std::string targetpath = Path(path).File().RemoveExtension().Get(); // We also have to be sure we don't overlap with any other .wast files, so we name the DLL based on the file path.
  env.flags |= ENV_NO_INIT; // We can't allow the DLL to call _DllInit because we can't catch exceptions from it, so we manually call it instead.

  int err = CheckWatTokens(env, env.errors, tokens, start);
  if(err)
    return err;

  if(env.errors)
    return ERR_WAT_INVALID_TOKEN;

  kh_indexname_t* mapping = kh_init_indexname(); // This is a special mapping for all modules using the module name itself, not just registered ones.
  Module* last = nullptr; // For anything not providing a module name, this was the most recently defined module.
  void* cache = nullptr;

  while(tokens.Size() > 0 && tokens[0].id != TOKEN_CLOSE)
  {
    EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
    switch(tokens[0].id)
    {
    case TOKEN_MODULE:
    {
      InvalidateCache(cache);

      env.modules = trealloc<Module>(env.modules, ++env.n_modules);
      last = &env.modules[env.n_modules - 1];

      if(err = ParseWastModule(env, tokens, mapping, *last, path))
        return err;
      ValidateModule(env, *last);
      if(env.errors)
        return ERR_VALIDATION_ERROR;

      break;
    }
    case TOKEN_REGISTER:
    {
      InvalidateCache(cache);
      tokens.Pop();

      ByteArray name;
      if(err = WatString(env, name, tokens.Pop()))
        return err;
      int r;
      khiter_t iter = kh_put_modules(env.modulemap, name, &r);
      if(!r)
        return ERR_FATAL_DUPLICATE_MODULE_NAME;

      size_t i = ~0;
      if(last)
        i = last - env.modules;
      if(tokens[0].id == TOKEN_NAME)
        i = GetWastMapping(mapping, tokens.Pop());
      if(i == (size_t)~0)
        return ERR_PARSE_INVALID_NAME;

      kh_val(env.modulemap, iter) = i;
      env.modules[i].name = name;
      break;
    }
    case TOKEN_INVOKE:
    case TOKEN_GET:
    {
      WatToken t = tokens.Peek();
      WastResult result;
      if(err = ParseWastAction(env, tokens, mapping, last, cache, counter, targetpath, result))
      {
        if(err != ERR_RUNTIME_TRAP && err != ERR_RUNTIME_INIT_ERROR)
          return err;
        AppendError(env, errors, last, err, "[%zu] Runtime error %i while attempting to verify result.", WatLineNumber(start, t.pos), err);
      }
      break;
    }
    case TOKEN_ASSERT_TRAP:
    {
      WatToken t = tokens.Pop();
      if(tokens.Size() > 1 && tokens[0].id == TOKEN_OPEN && tokens[1].id == TOKEN_MODULE) // Check if we're actually trapping on a module load
      {
        EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
        env.modules = trealloc<Module>(env.modules, ++env.n_modules); // We temporarily add this module to the environment, but don't set the "last" module to it
        if(err = ParseWastModule(env, tokens, mapping, env.modules[env.n_modules - 1], path))
          return err;
        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);

        err = CompileWast(env, (targetpath + std::to_string(counter++) + IR_LIBRARY_EXTENSION).c_str(), cache);
        --env.n_modules; // Remove the module from the environment to avoid poisoning other compilations
        if(err != ERR_RUNTIME_TRAP)
          AppendError(env, errors, 0, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected trap, but call succeeded", WatLineNumber(start, t.pos));
        EXPECTED(tokens, TOKEN_STRING, ERR_WAT_EXPECTED_STRING);
      }
      else
      {
        EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
        WastResult result;
        err = ParseWastAction(env, tokens, mapping, last, cache, counter, targetpath, result);
        if(err != ERR_RUNTIME_TRAP)
          AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected trap, but call succeeded", WatLineNumber(start, t.pos));
        EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
        EXPECTED(tokens, TOKEN_STRING, ERR_WAT_EXPECTED_STRING);
      }
      break;
    }
    case TOKEN_ASSERT_RETURN:
    case TOKEN_ASSERT_RETURN_CANONICAL_NAN:
    case TOKEN_ASSERT_RETURN_ARITHMETIC_NAN:
    {
      WatToken t = tokens.Pop();
      EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
      WastResult result = { TE_NONE };
      if(err = ParseWastAction(env, tokens, mapping, last, cache, counter, targetpath, result))
      {
        if(err != ERR_RUNTIME_TRAP && err != ERR_RUNTIME_INIT_ERROR)
          return err;
        AppendError(env, errors, last, err, "[%zu] Runtime error %i while attempting to verify result.", WatLineNumber(start, t.pos), err);
      }
      EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
      Instruction value;
      WatState state(env, *last);

      switch(t.id)
      {
      case TOKEN_ASSERT_RETURN:
        if(tokens[0].id == TOKEN_CLOSE) // This is valid because it represents a return of nothing
          value.opcode = OP_nop;
        else
        {
          EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
          if(err = WatInitializer(state, tokens, value))
            return err;
          EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
        }

        switch(value.opcode)
        {
        case OP_nop:
          if(result.type != TE_void)
            AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected no return value but got %i", WatLineNumber(start, t.pos), result.type);
          break;
        case OP_i32_const:
          if(result.type != TE_i32)
            AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected i32 type but got %i", WatLineNumber(start, t.pos), result.type);
          else if(result.i32 != value.immediates[0]._varsint32)
            AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected %i but got %i", WatLineNumber(start, t.pos), value.immediates[0]._varsint32, result.i32);
          break;
        case OP_i64_const:
          if(result.type != TE_i64)
            AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected i64 type but got %i", WatLineNumber(start, t.pos), result.type);
          else if(result.i64 != value.immediates[0]._varsint64)
            AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected %lli but got %lli", WatLineNumber(start, t.pos), value.immediates[0]._varsint64, result.i64);
          break;
        case OP_f32_const:
          if(result.type != TE_f32)
            AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected f32 type but got %i", WatLineNumber(start, t.pos), result.type);
          else if(isnan(value.immediates[0]._float32)) // If this is an NAN we must match the exact bit pattern
          {
            if(value.immediates[0]._varsint32 != result.i32)
              AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected %g but got %g", WatLineNumber(start, t.pos), value.immediates[0]._float32, result.f32);
          }
          else if(result.f32 != value.immediates[0]._float32)
            AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected %g but got %g", WatLineNumber(start, t.pos), value.immediates[0]._float32, result.f32);
          break;
        case OP_f64_const:
          if(result.type != TE_f64)
            AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected f64 type but got %i", WatLineNumber(start, t.pos), result.type);
          else if(isnan(value.immediates[0]._float64)) // If this is an NAN we must match the exact bit pattern
          {
            if(value.immediates[0]._varsint64 != result.i64)
              AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected %g but got %g", WatLineNumber(start, t.pos), value.immediates[0]._float64, result.f64);
          }
          else if(result.f64 != value.immediates[0]._float64)
            AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected %g but got %g", WatLineNumber(start, t.pos), value.immediates[0]._float64, result.f64);
          break;
        }
        break;
      case TOKEN_ASSERT_RETURN_ARITHMETIC_NAN:
      case TOKEN_ASSERT_RETURN_CANONICAL_NAN:
      {
        bool canonical = t.id == TOKEN_ASSERT_RETURN_CANONICAL_NAN;
        if(result.type != TE_f32 && result.type != TE_f64)
          AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected %s NaN but got unexpected integer %z", WatLineNumber(start, t.pos), canonical ? "canonical" : "arithmetic", result.i64);
        if(result.type == TE_f32 && !WastIsNaN(result.f32, canonical))
          AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected %s NaN but got %g", WatLineNumber(start, t.pos), canonical ? "canonical" : "arithmetic", result.f32);
        if(result.type == TE_f64 && !WastIsNaN(result.f64, canonical))
          AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected %s NaN but got %g", WatLineNumber(start, t.pos), canonical ? "canonical" : "arithmetic", result.f64);
      }
      break;
      }
      break;
    }
    case TOKEN_ASSERT_MALFORMED:
    {
      WatToken t = tokens.Pop();
      EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
      Module m;
      int code = ParseWastModule(env, tokens, mapping, m, path);
      EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);

      ByteArray error;
      if(err = WatString(env, error, tokens.Pop()))
        return err;

      string assertcode = GetAssertionString(code);

      if(STRICMP(assertcode.c_str(), MapAssertionString(error.str())))
        AppendError(env, errors, 0, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected '%s' error, but got '%s' instead", WatLineNumber(start, t.pos), error.str(), assertcode.c_str());
      env.errors = 0;
      break;
    }
    case TOKEN_ASSERT_INVALID:
    case TOKEN_ASSERT_UNLINKABLE:
    {
      WatToken t = tokens.Pop();
      EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
      Module m;
      int code = ParseWastModule(env, tokens, mapping, m, path);
      EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);

      string assertcode = GetAssertionString(code);

      if(code < 0)
      {
        AppendError(env, errors, 0, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected module parsing success, but got '%s' instead", WatLineNumber(start, t.pos), assertcode.c_str());
        return code; // A parsing failure means we cannot recover
      }

      ByteArray error;
      if(err = WatString(env, error, tokens.Pop()))
        return err;

      if(!env.errors) // Only do additional validation if we didn't find validation errors during the parsing process that must trump our normal validation
        ValidateModule(env, m);
      code = ERR_SUCCESS;
      if(env.errors)
        code = env.errors->code;
      assertcode = GetAssertionString(code);
      env.errors = 0;
      if(STRICMP(assertcode.c_str(), MapAssertionString(error.str())))
        AppendError(env, errors, 0, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected '%s' error, but got '%s' instead", WatLineNumber(start, t.pos), error.str(), assertcode.c_str());
      break;
    }
    case TOKEN_ASSERT_EXHAUSTION:
    {
      SkipSection(tokens); // TODO: figure out how to do this.
      break;
      WatToken t = tokens.Pop();
      EXPECTED(tokens, TOKEN_OPEN, ERR_WAT_EXPECTED_OPEN);
      WastResult result;
      err = ParseWastAction(env, tokens, mapping, last, cache, counter, targetpath, result);
      if(err != ERR_RUNTIME_TRAP)
        AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected call exhaustion trap, but call succeeded", WatLineNumber(start, t.pos));
      env.errors = 0;
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
    {
      // If we get an unexpected token, try to parse it as an inline module
      WatToken t = WatToken{ TOKEN_NONE };
      env.modules = trealloc<Module>(env.modules, ++env.n_modules);
      auto name = "m" + std::to_string(env.n_modules);

      last = &env.modules[env.n_modules - 1];
      tokens.SetPosition(tokens.GetPosition() - 1); // Recover the '('

      if(err = WatModule(env, *last, tokens, StringRef{ name.data(), name.size() }, t))
        return err;

      last->path = path;
      tokens.SetPosition(tokens.GetPosition() - 1); // Recover the ')'
      break;
    }
    }

    EXPECTED(tokens, TOKEN_CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }

  if(always_compile && !cache) // If cache is null we must ensure we've at least tried to compile the test even if there's nothing to run.
  {
    if(err = CompileWast(env, (targetpath + std::to_string(counter++) + IR_LIBRARY_EXTENSION).c_str(), cache))
      return err;
    assert(cache);
  }

  env.errors = errors;
  if(env.errors)
    internal::ReverseErrorList(env.errors);
  return ERR_SUCCESS;
}