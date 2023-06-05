// Copyright (c)2021 Fundament Software
// For conditions of distribution and use, see copyright notice in innative.h

#include "wast.h"
#include "link.h"
#include "tools.h"
#include "queue.h"
#include "parse.h"
#include "innative/export.h"
#include <signal.h>
#include <setjmp.h>
#include <iostream>
#include <atomic>
#include <functional>

#ifdef IN_PLATFORM_WIN32
  #include "../innative/win32.h"
#endif

#ifdef IN_PLATFORM_POSIX
  #define LONGJMP(x, i) siglongjmp(x, i)
  #define SETJMP(x)     sigsetjmp(x, 1)
#else
  #define LONGJMP(x, i) longjmp(x, i)
  #define SETJMP(x)     setjmp(x)
#endif

#pragma warning(disable : 4062)

using namespace innative;
using namespace wat;
using namespace utility;
using std::string;

KHASH_INIT(stringmap, const char*, const char*, 1, kh_str_hash_funcins, kh_str_hash_insequal)

namespace innative {
  namespace internal {
    template<typename X> struct HType
    {
      typedef int64_t T;
    };
  } // namespace internal

  namespace wat {
    path GenUniquePath(Environment& env, const path& src, int& counter);
    kh_stringmap_t* GenWastStringMap(std::initializer_list<std::pair<const char*, const char*>> map);

    static kh_stringmap_t* assertmap = GenWastStringMap({
      { "unknown function 0", "unknown function" },
      { "unknown memory 0", "unknown memory" },
      { "unknown table 0", "unknown table" },
      { "i32 constant", "constant out of range" },
      { "length out of bounds", "unexpected end" },
      { "import after function", "invalid import order" },
      { "import after global", "invalid import order" },
      { "import after table", "invalid import order" },
      { "import after memory", "invalid import order" },
      { "result before parameter", "unexpected token" },
      { "unexpected end of section or function", "unexpected end" },
      { "junk after last section", "multiple start sections" },
      { "unknown data segment 1", "unknown data segment" },
      { "i32 constant out of range", "constant out of range" },
      { "invalid elements segment kind", "invalid elem" },
      //{ "unknown data segment", "data count section required" },
      { "duplicate memory", "duplicate name" },
      { "duplicate table", "duplicate name" },
      { "duplicate global", "duplicate name" },
      { "duplicate func", "duplicate name" },
      { "duplicate local", "duplicate name" },
      { "unknown elem segment 0", "unknown elem segment" },
      { "unknown elem segment 4", "unknown elem segment" },
    });

    size_t GetWastMapping(kh_indexname_t* mapping, const WatToken& t);

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

      template<typename T> static WastResult get(T v)
      {
        WastResult r;
        if constexpr(std::is_same_v<T, int32_t>)
        {
          r.type = TE_i32;
          r.i32  = v;
        }
        else if constexpr(std::is_same_v<T, int64_t>)
        {
          r.type = TE_i64;
          r.i64  = v;
        }
        else if constexpr(std::is_same_v<T, float>)
        {
          r.type = TE_f32;
          r.f32  = v;
        }
        else if constexpr(std::is_same_v<T, double>)
        {
          r.type = TE_f64;
          r.f64  = v;
        }
        else // This has to rely on the template parameter or it goes boom on GCC/clang
          static_assert(std::is_same_v<T, void>, "invalid type");
        return r;
      }
    };

    jmp_buf jump_location;

    void WastCrashHandler(int sig) { LONGJMP(jump_location, 1); }
    void InvalidateCache(Environment& env, void*& cache, path cachepath);
    int IsolateInitCall(Environment& env, void*& cache, const path& out);
    int CompileWast(Environment& env, const path& out, void*& cache, path& cachepath);
    int SetTempName(Environment& env, Module& m);
    int ParseWastModule(Environment& env, Queue<WatToken>& tokens, kh_indexname_t* mapping, Module& m, const path& file);

    // SEH exceptions and destructors don't mix, so we isolate all this signal and exception handling in this function.
    int IsolateFunctionCall(Environment& env, FunctionType* ftype, void* f, std::vector<WastResult>& result,
                            std::vector<Instruction>& params);
    int ParseWastAction(Environment& env, Queue<WatToken>& tokens, kh_indexname_t* mapping, Module*& last, void*& cache,
                        path& cachepath, int& counter, const path& file, std::vector<WastResult>& result);
    bool WastIsNaN(float f, bool canonical);
    bool WastIsNaN(double f, bool canonical);
    inline string GetAssertionString(int code);
    inline const char* MapAssertionString(const char* s);

    typedef void (*IN_GENERAL)(char*, char*);
  } // namespace wat
} // namespace innative

path wat::GenUniquePath(Environment& env, const path& src, int& counter)
{
  auto cur = src;
  cur += std::to_string(counter++);
  cur += (env.abi == IN_ABI_Windows) ? ".dll" : ".so";
  return cur;
}

kh_stringmap_t* wat::GenWastStringMap(std::initializer_list<std::pair<const char*, const char*>> map)
{
  kh_stringmap_t* h = kh_init_stringmap();

  int r;
  for(auto& m : map)
  {
    auto iter       = kh_put_stringmap(h, m.first, &r);
    kh_val(h, iter) = m.second;
  }

  return h;
}

size_t wat::GetWastMapping(kh_indexname_t* mapping, const WatToken& t)
{
  khiter_t iter = kh_get_indexname(mapping, StringSpan{ t.pos, t.len });
  return kh_exist2(mapping, iter) ? kh_val(mapping, iter) : (size_t)~0;
}

void wat::InvalidateCache(Environment& env, void*& cache, path cachepath)
{
  if(cache)
  {
    auto exit = LoadFunction(cache, 0, IN_EXIT_FUNCTION);

    if(exit)
      (*exit)();
    else
      assert(false);

    FreeDLL(cache);
    remove(cachepath);
    cachepath.replace_extension((env.abi == IN_ABI_Windows) ? ".lib" : ".a");
    remove(cachepath);
    cachepath.replace_extension(".pdb");
    remove(cachepath);
  }
  cache = nullptr;
}

// longjmp and exceptions don't always play well with destructors, so we isolate this call
int wat::IsolateInitCall(Environment& env, void*& cache, const path& out)
{
  if(SETJMP(jump_location) != 0)
    return ERR_RUNTIME_TRAP;

  cache = LoadDLL(out);
  if(!cache)
  {
    auto estring = LoadDLLError();
    (*env.loghook)(&env, "Error loading %s: %s\n", out.c_str(), estring);
    LoadDLLErrorFree(estring);
    return ERR_RUNTIME_INIT_ERROR;
  }

  if(env.wasthook != nullptr)
    (*env.wasthook)(&env, cache);

  auto entry = LoadFunction(cache, 0, IN_INIT_FUNCTION);

  if(!entry)
    return ERR_RUNTIME_INIT_ERROR;

#ifdef IN_COMPILER_MSC
  // On windows, signals can sometimes get promoted to SEH exceptions across DLL bounderies.
  __try
  {
    (*entry)();
  }
  __except(GetExceptionCode() == EXCEPTION_ILLEGAL_INSTRUCTION) // Only catch an illegal instruction
  {
    return ERR_RUNTIME_TRAP;
  }
#else
  (*entry)();
#endif

  return ERR_SUCCESS;
}

int wat::CompileWast(Environment& env, const path& out, void*& cache, path& cachepath)
{
  InvalidateCache(env, cache, cachepath);
  env.flags |= ENV_GENERALIZE_FUNCTIONS;

  int err;
  ValidateEnvironment(env);
  if(env.errors)
    return ERR_VALIDATION_ERROR;
  if(err = CompileEnvironment(&env, out.u8string().c_str()))
    return err;

  cachepath = out;
  signal(SIGILL, WastCrashHandler);
  signal(SIGFPE, WastCrashHandler);

  err = IsolateInitCall(env, cache, out);

  signal(SIGILL, SIG_DFL);
  signal(SIGFPE, SIG_DFL);
  return err;
}

int wat::SetTempName(Environment& env, Module& m)
{
  static std::atomic_size_t modcount(1); // We can't use n_modules in case a module is malformed

  auto buf = std::string(IN_TEMP_PREFIX) + std::to_string(modcount.fetch_add(1, std::memory_order::memory_order_relaxed));
  m.name.resize(static_cast<varuint32>(buf.size()), true, env);
  if(!m.name.get())
    return ERR_FATAL_OUT_OF_MEMORY;

  tmemcpy(reinterpret_cast<char*>(m.name.get()), m.name.size(), buf.data(), buf.size());
  return ERR_SUCCESS;
}

int wat::ParseWastModule(Environment& env, Queue<WatToken>& tokens, kh_indexname_t* mapping, Module& m, const path& file)
{
  EXPECTED(env, tokens, WatTokens::MODULE, ERR_WAT_EXPECTED_MODULE);
  int err;
  WatToken name = { WatTokens::NONE };
  m             = { 0 }; // We have to ensure this is zeroed, because an error could occur before ParseModule is called
  std::string tempname(IN_TEMP_PREFIX);
  tempname += std::to_string(env.n_modules);

  if(tokens[0].id == WatTokens::BINARY || (tokens.Size() > 1 && tokens[1].id == WatTokens::BINARY))
  {
    name = WatParser::GetWatNameToken(tokens);
    if(!name.pos)
    {
      name.pos = tempname.data();
      name.len = tempname.size();
    }

    EXPECTED(env, tokens, WatTokens::BINARY, ERR_WAT_EXPECTED_BINARY);
    ByteArray binary;
    while(tokens.Peek().id == WatTokens::STRING)
      if(err = WatParser::WatString(env, binary, tokens.Pop()))
        return err;
    Stream s = { binary.get(), binary.size(), 0 };
    if(err = ParseModule(s, file.u8string().c_str(), env, m, ByteArray::Identifier(name.pos, name.len), env.errors))
      return err;
    if(name.id == WatTokens::NAME) // Override name if it exists
      if(err = WatParser::ParseName(env, m.name, name))
        return err;
  }
  else if(tokens[0].id == WatTokens::QUOTE || (tokens.Size() > 1 && tokens[1].id == WatTokens::QUOTE))
  {
    name = WatParser::GetWatNameToken(tokens);
    if(!name.pos)
    {
      name.pos = tempname.data();
      name.len = tempname.size();
    }

    EXPECTED(env, tokens, WatTokens::QUOTE, ERR_WAT_EXPECTED_QUOTE);
    ByteArray quote;
    while(tokens.Peek().id == WatTokens::STRING)
      if(err = WatParser::WatString(env, quote, tokens.Pop()))
        return err;
    if(err = ParseWatModule(env, file.u8string().c_str(), m, quote.get(), quote.size(), StringSpan{ name.pos, name.len }))
      return err;
    if(name.id == WatTokens::NAME) // Override name if it exists
      if(err = WatParser::ParseName(env, m.name, name))
        return err;
  }
  else if(err = WatParser::ParseModule(env, m, file.u8string().c_str(), tokens,
                                       StringSpan{ tempname.data(), tempname.size() }, name))
    return err;

  if(name.id == WatTokens::NAME) // Only add this to our name mapping if an actual name token was specified, regardless
                                 // of whether the module has a name.
  {
    int r;
    khiter_t iter = kh_put_indexname(mapping, { name.pos, name.len }, &r);
    if(!r)
      return ERR_FATAL_DUPLICATE_MODULE_NAME;
    kh_val(mapping, iter) = static_cast<varuint32>(env.n_modules) - 1;
  }
  else // If the module has no name, we must assign a temporary one
    return SetTempName(env, m);
  return ERR_SUCCESS;
}

// SEH exceptions and destructors don't mix, so we isolate all this signal and exception handling in this function.
int wat::IsolateFunctionCall(Environment& env, FunctionType* ftype, void* f, std::vector<WastResult>& result,
                             std::vector<Instruction>& params)
{
  if(SETJMP(jump_location) != 0)
  {
    return ERR_RUNTIME_TRAP;
  }

#ifdef IN_COMPILER_MSC
  __try // this catches division by zero on windows
  {
#endif
    if(params.size() != ftype->n_params)
      return ERR_FATAL_UNKNOWN_KIND;

    size_t n_parambytes = 0;
    for(auto& p : params)
    {
      switch(p.opcode[0])
      {
      case OP_i32_const:
      case OP_f32_const: n_parambytes += sizeof(int32_t); break;
      case OP_i64_const:
      case OP_f64_const: n_parambytes += sizeof(int64_t); break;
      default: assert(false); break;
      }
    }

    auto parambytes = tmalloc<char>(env, n_parambytes);
    size_t offset   = 0;
    for(auto& p : params)
    {
      switch(p.opcode[0])
      {
      case OP_i32_const:
      case OP_f32_const:
        *reinterpret_cast<int32_t*>(parambytes + offset) = p.immediates[0]._varsint32;
        offset += sizeof(int32_t);
        break;
      case OP_i64_const:
      case OP_f64_const:
        *reinterpret_cast<int64_t*>(parambytes + offset) = p.immediates[0]._varsint64;
        offset += sizeof(int64_t);
        break;
      }
    }

    size_t n_returnbytes = 0;
    for(varuint32 i = 0; i < ftype->n_returns; ++i)
    {
      switch(ftype->returns[i])
      {
      case TE_i32:
      case TE_f32: n_returnbytes += sizeof(int32_t); break;
      case TE_i64:
      case TE_f64: n_returnbytes += sizeof(int64_t); break;
      default: assert(false); break;
      }
    }

    auto returnbytes = tmalloc<char>(env, n_returnbytes);
    memset(returnbytes, 0xFD, n_returnbytes); // make it easier to recognize uninitialized memory

    reinterpret_cast<IN_GENERAL>(f)(parambytes, returnbytes);

    offset = 0;
    for(varuint32 i = ftype->n_returns; i-- > 0;)
    {
      switch(ftype->returns[i])
      {
      case TE_i32:
        result.push_back(WastResult::get(*reinterpret_cast<int32_t*>(returnbytes + offset)));
        offset += sizeof(int32_t);
        break;
      case TE_f32:
        result.push_back(WastResult::get(*reinterpret_cast<float*>(returnbytes + offset)));
        offset += sizeof(int32_t);
        break;
      case TE_i64:
        result.push_back(WastResult::get(*reinterpret_cast<int64_t*>(returnbytes + offset)));
        offset += sizeof(int64_t);
        break;
      case TE_f64:
        result.push_back(WastResult::get(*reinterpret_cast<double*>(returnbytes + offset)));
        offset += sizeof(double);
        break;
      }
    }

#ifdef IN_COMPILER_MSC
  }
  __except(1)
  {
    // This uses unholy black magic to restore the stack guard page in the event of a stack overflow
    if(GetExceptionCode() == EXCEPTION_STACK_OVERFLOW)
    {
      void* lpPage;
  #ifdef IN_CPU_x86
      __asm mov lpPage, esp;
  #elif defined(IN_CPU_x86_64)
      lpPage = reinterpret_cast<void*>(GetRSPValue());
  #else
    #error unsupported CPU architecture
  #endif
      // Get page size of system
      SYSTEM_INFO si;
      GetSystemInfo(&si);

      // Get allocation base of stack
      MEMORY_BASIC_INFORMATION mi;
      VirtualQuery(lpPage, &mi, sizeof(mi));

      // Go to page beyond current page
      lpPage = (LPBYTE)(mi.BaseAddress) - si.dwPageSize;

      // Free portion of stack just abandoned
      if(!VirtualFree(mi.AllocationBase, (LPBYTE)lpPage - (LPBYTE)mi.AllocationBase, MEM_DECOMMIT))
        return ERR_FATAL_UNKNOWN_KIND;

      // Reintroduce the guard page
      DWORD dwOldProtect;
      if(!VirtualProtect(lpPage, si.dwPageSize, PAGE_GUARD | PAGE_READWRITE, &dwOldProtect))
        return ERR_FATAL_UNKNOWN_KIND;
    }

    return ERR_RUNTIME_TRAP;
  }
#endif

  return ERR_SUCCESS;
}

int wat::ParseWastAction(Environment& env, Queue<WatToken>& tokens, kh_indexname_t* mapping, Module*& last, void*& cache,
                         path& cachepath, int& counter, const path& file, std::vector<WastResult>& result)
{
  int err;
  int cache_err = 0;
  if(!cache) // If cache is null we need to recompile the current environment, but we can't bail on error messages yet
             // or we'll corrupt the parse
    cache_err = CompileWast(env, GenUniquePath(env, file, counter), cache, cachepath);

  switch(tokens.Pop().id)
  {
  case WatTokens::INVOKE:
  {
    WatToken name = WatParser::GetWatNameToken(tokens);
    Module* m     = last;
    if(name.id == WatTokens::NAME)
    {
      size_t i = GetWastMapping(mapping, name);
      if(i >= env.n_modules)
        return ERR_PARSE_INVALID_NAME;
      m = env.modules + i;
    }
    if(!m)
      return ERR_FATAL_INVALID_MODULE;

    ByteArray func;
    if(err = WatParser::WatString(env, func, tokens.Pop()))
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
    while(tokens.Peek().id == WatTokens::OPEN)
    {
      WatParser st(env, *m);
      params.emplace_back();
      EXPECTED(env, tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
      if(err = st.ParseInitializer(tokens, params.back()))
        return err;
      EXPECTED(env, tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
    }

    if(params.size() != ftype->n_params)
      return ERR_SIGNATURE_MISMATCH;
    for(varuint32 i = 0; i < ftype->n_params; ++i)
    {
      varsint7 ty = TE_NONE;
      switch(params[i].opcode[0])
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
    void* f = reinterpret_cast<void*>(LoadDLLFunction(
      cache, (utility::CanonicalName(StringSpan::From(m->name), StringSpan::From(func)) + IN_GENERIC_POSTFIX).c_str()));
    if(!f)
      return ERR_INVALID_FUNCTION_INDEX;

    // Call the function and set the correct result.
    signal(SIGILL, WastCrashHandler);
    signal(SIGFPE, WastCrashHandler); // This catches division by zero on linux
    signal(SIGSEGV, WastCrashHandler);

#ifdef IN_PLATFORM_POSIX
    // Catch stack overflow on linux
    struct sigaction sa;
    stack_t ss;

    // Don't need much stack space, we immediately longjmp()
    // back out of the signal handler
    ss.ss_sp    = alloca(MINSIGSTKSZ + 4096);
    ss.ss_size  = MINSIGSTKSZ + 4096;
    ss.ss_flags = 0;

    sigaltstack(&ss, NULL);
    sa.sa_flags   = SA_ONSTACK;
    sa.sa_handler = WastCrashHandler;
    sigemptyset(&sa.sa_mask);
    sigaction(SIGSEGV, &sa, NULL);
#endif
    auto pred = [ftype](const FunctionType& a) {
      auto& b = *ftype;
      if(a.form != b.form || a.n_params != b.n_params || a.n_returns != b.n_returns)
        return false;
      for(varuint32 i = 0; i < a.n_returns; ++i)
        if(a.returns[i] != b.returns[i])
          return false;
      for(varuint32 i = 0; i < a.n_params; ++i)
        if(a.params[i] != b.params[i])
          return false;
      return true;
    };

    err = IsolateFunctionCall(env, ftype, f, result, params);

    signal(SIGILL, SIG_DFL);
    signal(SIGFPE, SIG_DFL);
    signal(SIGSEGV, SIG_DFL);

    if(err != ERR_SUCCESS)
      return err;
    break;
  }
  case WatTokens::GET:
  {
    WatToken name = WatParser::GetWatNameToken(tokens);
    Module* m     = last;
    if(name.id == WatTokens::NAME)
    {
      size_t i = GetWastMapping(mapping, name);
      if(i >= env.n_modules)
        return ERR_PARSE_INVALID_NAME;
      m = env.modules + i;
    }
    if(!m)
      return ERR_FATAL_INVALID_MODULE;

    ByteArray global;
    if(err = WatParser::WatString(env, global, tokens.Pop()))
      return err;

    khiter_t iter = kh_get_exports(m->exports, global);
    if(!kh_exist2(m->exports, iter))
      return ERR_INVALID_GLOBAL_INDEX;
    Export& e     = m->exportsection.exports[kh_val(m->exports, iter)];
    GlobalDesc* g = nullptr;
    if(e.kind != WASM_KIND_GLOBAL || !(g = ModuleGlobal(*m, e.index)))
      return ERR_INVALID_GLOBAL_INDEX;

    if(cache_err != 0)
      return cache_err;
    assert(cache);
    void* f = LoadGlobal(cache, m->name.str(), global.str());
    if(!f)
      return ERR_INVALID_GLOBAL_INDEX;

    switch(g->type)
    {
    case TE_i32: result.push_back(WastResult::get(*reinterpret_cast<int32_t*>(f))); break;
    case TE_i64: result.push_back(WastResult::get(*reinterpret_cast<int64_t*>(f))); break;
    case TE_f32: result.push_back(WastResult::get(*reinterpret_cast<float*>(f))); break;
    case TE_f64: result.push_back(WastResult::get(*reinterpret_cast<double*>(f))); break;
    default: return ERR_INVALID_TYPE;
    }

    break;
  }
  default: return ERR_WAT_EXPECTED_TOKEN;
  }

  return ERR_SUCCESS;
}

bool wat::WastIsNaN(float f, bool canonical)
{
  if(!isnan(f))
    return false;
  if(!canonical)
    return true; // Due to webassembly's NaN requirements not mapping to hardware, we ignore this subcase right now.
  union
  {
    float f;
    uint32_t i;
  } u = { f };
  return ((u.i & 0x200000U) != 0) != canonical;
}

bool wat::WastIsNaN(double f, bool canonical)
{
  if(!isnan(f))
    return false;
  if(!canonical)
    return true; // Due to webassembly's NaN requirements not mapping to hardware, we ignore this subcase right now.
  union
  {
    double f;
    uint64_t i;
  } u = { f };
  return ((u.i & 0x4000000000000ULL) != 0) != canonical;
}

inline string wat::GetAssertionString(int code)
{
  string assertcode = "[SUCCESS]";
  if(code < 0)
  {
    khiter_t iter = kh_get_mapenum(WAST_ASSERTION_MAP, code);
    if(!kh_exist2(WAST_ASSERTION_MAP, iter))
      assertcode = "[unknown error code " + std::to_string(code) + "]";
    else
      assertcode = kh_val(WAST_ASSERTION_MAP, iter);
  }
  return assertcode;
}

inline const char* wat::MapAssertionString(const char* s)
{
  khiter_t i = kh_get_stringmap(assertmap, s);
  if(kh_exist2(assertmap, i))
    return kh_val(assertmap, i);
  return s;
}

inline bool CheckSpecialNaN(Queue<WatToken>& tokens, Instruction& value, bool& canonical)
{
  if(tokens.Size() < 2)
    return false;

  auto tokenEq = [&](int i, const char* s) {
    auto checklen = strlen(s);
    if(tokens[i].len != checklen)
      return false;
    return !strncmp(tokens[i].pos, s, checklen);
  };

  if(tokens[0].id != WatTokens::OPERATOR)
    return false;
  if(tokens[0].i != OP_f32_const && tokens[0].i != OP_f64_const)
    return false;

  value.opcode[0] = (uint8_t)tokens[0].i;

  // this is a dirty hack i know but ugh this whole thing is just an annoying special case
  if(tokenEq(1, "nan:canonical"))
    canonical = true;
  else if(tokenEq(1, "nan:arithmetic"))
    canonical = false;
  else
    return false;

  tokens.Pop();
  tokens.Pop();
  return true;
}

// This parses an entire extended WAT testing script into an environment
int innative::ParseWast(Environment& env, const uint8_t* data, size_t sz, const path& file, bool always_compile,
                        const path& output)
{
  Queue<WatToken> tokens;
  const char* start = reinterpret_cast<const char*>(data);
  TokenizeWAT(tokens, start, start + sz);
  ValidationError* errors = nullptr;
  int counter = 0; // Even if we unload wast.dll, visual studio will keep the .pdb open forever, so we have to generate new
                   // DLLs for each new test section.
  path targetpath = output / file.stem(); // We also have to be sure we don't overlap with any other .wast files, so we name
                                          // the DLL based on the file path.
  env.flags |= ENV_NO_INIT; // We can't allow the DLL to call _DllInit because we can't catch exceptions from it, so we
                            // manually call it instead.

  int err = CheckWatTokens(env, env.errors, tokens, start);
  if(err)
    return err;

  if(env.errors)
    return ERR_WAT_INVALID_TOKEN;

  kh_indexname_t* mapping =
    kh_init_indexname(); // This is a special mapping for all modules using the module name itself, not just registered ones.
  auto lastlog = env.loglevel;
  DeferLambda<std::function<void()>> defer([&]() {
    kh_destroy_indexname(mapping);
    env.loglevel = lastlog;
  });

  Module* last = nullptr; // For anything not providing a module name, this was the most recently defined module.
  void* cache  = nullptr;
  path cachepath;

  while(tokens.Size() > 0 && tokens[0].id != WatTokens::CLOSE)
  {
    EXPECTED(env, tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
    switch(tokens[0].id)
    {
    case WatTokens::MODULE:
    {
      InvalidateCache(env, cache, cachepath);

      env.modules = trealloc<Module>(env.modules, ++env.n_modules);
      if(!env.modules)
        return ERR_FATAL_OUT_OF_MEMORY;
      last = &env.modules[env.n_modules - 1];

      if(err = ParseWastModule(env, tokens, mapping, *last, file))
        return err;
      ValidateModule(env, *last);
      if(env.errors)
        return ERR_VALIDATION_ERROR;

      break;
    }
    case WatTokens::REGISTER:
    {
      InvalidateCache(env, cache, cachepath);
      tokens.Pop();

      ByteArray name;
      if(err = WatParser::WatString(env, name, tokens.Pop()))
        return err;
      int r;
      khiter_t iter = kh_put_modules(env.modulemap, name, &r);
      if(!r)
        return ERR_FATAL_DUPLICATE_MODULE_NAME;

      size_t i = ~0;
      if(last)
        i = last - env.modules;
      if(tokens[0].id == WatTokens::NAME)
        i = GetWastMapping(mapping, tokens.Pop());
      if(i == (size_t)~0)
        return ERR_PARSE_INVALID_NAME;

      kh_val(env.modulemap, iter) = i;
      env.modules[i].name         = name;
      break;
    }
    case WatTokens::INVOKE:
    case WatTokens::GET:
    {
      WatToken t = tokens.Peek();
      std::vector<WastResult> result;
      if(err = ParseWastAction(env, tokens, mapping, last, cache, cachepath, counter, targetpath, result))
      {
        if(err != ERR_RUNTIME_TRAP && err != ERR_RUNTIME_INIT_ERROR)
          return err;
        char buf[32];
        AppendError(env, errors, last, err, "[%zu] Runtime error %s while attempting to verify result.",
                    WatLineNumber(start, t.pos), EnumToString(ERR_ENUM_MAP, err, buf, 32));
      }
      break;
    }
    case WatTokens::ASSERT_EXHAUSTION:
      if(env.optimize != 0)
      {
        env.optimize = 0; // If we need to catch stack overflows, we must disable optimizations
        InvalidateCache(env, cache, cachepath);
        DeleteContext(env, false);
      }
    case WatTokens::ASSERT_TRAP:
    {
      WatToken t = tokens.Pop();
      if(tokens.Size() > 1 && tokens[0].id == WatTokens::OPEN &&
         tokens[1].id == WatTokens::MODULE) // Check if we're actually trapping on a module load
      {
        EXPECTED(env, tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
        env.modules = trealloc<Module>(
          env.modules,
          ++env.n_modules); // We temporarily add this module to the environment, but don't set the "last" module to it
        if(!env.modules)
          return ERR_FATAL_OUT_OF_MEMORY;
        if(err = ParseWastModule(env, tokens, mapping, env.modules[env.n_modules - 1], file))
          return err;
        EXPECTED(env, tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);

        err = CompileWast(env, GenUniquePath(env, targetpath, counter), cache, cachepath);
        --env.n_modules; // Remove the module from the environment to avoid poisoning other compilations
        if(err != ERR_RUNTIME_TRAP)
          AppendError(env, errors, 0, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected trap, but call succeeded",
                      WatLineNumber(start, t.pos));
        EXPECTED(env, tokens, WatTokens::STRING, ERR_WAT_EXPECTED_STRING);
      }
      else
      {
        EXPECTED(env, tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
        std::vector<WastResult> result;
        err = ParseWastAction(env, tokens, mapping, last, cache, cachepath, counter, targetpath, result);
        if(err != ERR_RUNTIME_TRAP)
          AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected trap, but call succeeded",
                      WatLineNumber(start, t.pos));
        EXPECTED(env, tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
        EXPECTED(env, tokens, WatTokens::STRING, ERR_WAT_EXPECTED_STRING);
      }
      break;
    }
    case WatTokens::ASSERT_RETURN:
    case WatTokens::ASSERT_RETURN_CANONICAL_NAN:
    case WatTokens::ASSERT_RETURN_ARITHMETIC_NAN:
    {
      WatToken t = tokens.Pop();
      EXPECTED(env, tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
      std::vector<WastResult> result;
      if(err = ParseWastAction(env, tokens, mapping, last, cache, cachepath, counter, targetpath, result))
      {
        if(err != ERR_RUNTIME_TRAP && err != ERR_RUNTIME_INIT_ERROR)
          return err;
        char buf[32];
        AppendError(env, errors, last, err, "[%zu] Runtime error %s while attempting to verify result.",
                    WatLineNumber(start, t.pos), EnumToString(ERR_ENUM_MAP, err, buf, 32));
      }
      EXPECTED(env, tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
      Instruction value = {};
      WatParser state(env, *last);
      bool specialNan = false;
      bool nanCanonical = false;

      switch(t.id)
      {
      case WatTokens::ASSERT_RETURN:
        char typebuf[10];

        while(tokens[0].id == WatTokens::OPEN)
        {
          EXPECTED(env, tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
          if(CheckSpecialNaN(tokens, value, nanCanonical))
            specialNan = true;
          else if(err = state.ParseInitializer(tokens, value))
            return err;

          if(result.empty())
          {
            AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Ran out of return values!",
                        WatLineNumber(start, t.pos));
          }
          else
          {
            switch(value.opcode[0])
            {
            case OP_i32_const:
              if(result.back().type != TE_i32)
                AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected i32 type but got %s",
                            WatLineNumber(start, t.pos), EnumToString(TYPE_ENCODING_MAP, result.back().type, typebuf, 10));
              else if(result.back().i32 != value.immediates[0]._varsint32)
                AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected %i but got %i",
                            WatLineNumber(start, t.pos), value.immediates[0]._varsint32, result.back().i32);
              break;
            case OP_i64_const:
              if(result.back().type != TE_i64)
                AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected i64 type but got %s",
                            WatLineNumber(start, t.pos), EnumToString(TYPE_ENCODING_MAP, result.back().type, typebuf, 10));
              else if(result.back().i64 != value.immediates[0]._varsint64)
                AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected %" PRId64 " but got %" PRId64 "",
                            WatLineNumber(start, t.pos), value.immediates[0]._varsint64, result.back().i64);
              break;
            case OP_f32_const:
              if(result.back().type != TE_f32)
                AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected f32 type but got %s",
                            WatLineNumber(start, t.pos), EnumToString(TYPE_ENCODING_MAP, result.back().type, typebuf, 10));
              else if(specialNan)
              {
                if(!WastIsNaN(result.back().f32, nanCanonical))
                  AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected %s NaN but got %g",
                              WatLineNumber(start, t.pos), nanCanonical ? "canonical" : "arithmetic", result.back().f32);
              }
              else if(isnan(value.immediates[0]._float32)) // If this is an NAN we must match the exact bit pattern
              {
                if(value.immediates[0]._varsint32 != result.back().i32)
                  AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected %g but got %g",
                              WatLineNumber(start, t.pos), value.immediates[0]._float32, result.back().f32);
              }
              else if(result.back().f32 != value.immediates[0]._float32)
                AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected %g but got %g",
                            WatLineNumber(start, t.pos), value.immediates[0]._float32, result.back().f32);
              break;
            case OP_f64_const:
              if(result.back().type != TE_f64)
                AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected f64 type but got %s",
                            WatLineNumber(start, t.pos), EnumToString(TYPE_ENCODING_MAP, result.back().type, typebuf, 10));
              else if(specialNan)
              {
                if(!WastIsNaN(result.back().f64, nanCanonical))
                  AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected %s NaN but got %g",
                              WatLineNumber(start, t.pos), nanCanonical ? "canonical" : "arithmetic", result.back().f64);
              }
              else if(isnan(value.immediates[0]._float64)) // If this is an NAN we must match the exact bit pattern
              {
                if(value.immediates[0]._varsint64 != result.back().i64)
                  AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected %g but got %g",
                              WatLineNumber(start, t.pos), value.immediates[0]._float64, result.back().f64);
              }
              else if(result.back().f64 != value.immediates[0]._float64)
                AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected %g but got %g",
                            WatLineNumber(start, t.pos), value.immediates[0]._float64, result.back().f64);
              break;
            default: assert(false);
            }
            result.pop_back();
          }

          EXPECTED(env, tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
        }

        if(result.size())
          AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected no return value but got %zu",
                      WatLineNumber(start, t.pos), EnumToString(TYPE_ENCODING_MAP, result.size(), typebuf, 10));

        break;
      case WatTokens::ASSERT_RETURN_ARITHMETIC_NAN:
      case WatTokens::ASSERT_RETURN_CANONICAL_NAN:
      {
        bool canonical = t.id == WatTokens::ASSERT_RETURN_CANONICAL_NAN;
        if(result.empty())
        {
          AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Ran out of return values!",
                      WatLineNumber(start, t.pos));
          break;
        }
        if(result.back().type != TE_f32 && result.back().type != TE_f64)
          AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected %s NaN but got unexpected integer %z",
                      WatLineNumber(start, t.pos), canonical ? "canonical" : "arithmetic", result.back().i64);
        if(result.back().type == TE_f32 && !WastIsNaN(result.back().f32, canonical))
          AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected %s NaN but got %g",
                      WatLineNumber(start, t.pos), canonical ? "canonical" : "arithmetic", result.back().f32);
        if(result.back().type == TE_f64 && !WastIsNaN(result.back().f64, canonical))
          AppendError(env, errors, last, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected %s NaN but got %g",
                      WatLineNumber(start, t.pos), canonical ? "canonical" : "arithmetic", result.back().f64);
      }
      break;
      }
      break;
    }
    case WatTokens::ASSERT_MALFORMED:
    {
      env.loglevel = LOG_NONE; // Suppress output because we're expecting it
      WatToken t   = tokens.Pop();
      EXPECTED(env, tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
      Module m;
      int code = ParseWastModule(env, tokens, mapping, m, file);
      EXPECTED(env, tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);

      ByteArray error;
      if(err = WatParser::WatString(env, error, tokens.Pop()))
        return err;

      string assertcode = GetAssertionString(code);

      if(STRICMP(assertcode.c_str(), MapAssertionString(error.str())))
        AppendError(env, errors, 0, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected '%s' error, but got '%s' instead",
                    WatLineNumber(start, t.pos), error.str(), assertcode.c_str());
      env.errors   = 0;
      env.loglevel = lastlog;
      break;
    }
    case WatTokens::ASSERT_INVALID:
    case WatTokens::ASSERT_UNLINKABLE:
    {
      env.loglevel = LOG_NONE; // Suppress output because we're expecting it
      WatToken t   = tokens.Pop();
      EXPECTED(env, tokens, WatTokens::OPEN, ERR_WAT_EXPECTED_OPEN);
      Module m;
      int code = ParseWastModule(env, tokens, mapping, m, file);
      EXPECTED(env, tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);

      string assertcode = GetAssertionString(code);

      if(code < 0)
      {
        AppendError(env, errors, 0, ERR_RUNTIME_ASSERT_FAILURE,
                    "[%zu] Expected module parsing success, but got '%s' instead", WatLineNumber(start, t.pos),
                    assertcode.c_str());
        return code; // A parsing failure means we cannot recover
      }

      ByteArray error;
      if(err = WatParser::WatString(env, error, tokens.Pop()))
        return err;

      if(!env.errors) // Only do additional validation if we didn't find validation errors during the parsing process that
                      // must trump our normal validation
        ValidateModule(env, m);
      code         = ERR_SUCCESS;
      env.loglevel = lastlog;

      while(env.errors)
      {
        code       = env.errors->code;
        assertcode = GetAssertionString(code);
        if(!STRICMP(assertcode.c_str(), MapAssertionString(error.str())))
          break;
        env.errors = env.errors->next;
      }

      if(!env.errors)
        AppendError(env, errors, 0, ERR_RUNTIME_ASSERT_FAILURE, "[%zu] Expected '%s' error, but got '%s' instead",
                    WatLineNumber(start, t.pos), error.str(), assertcode.c_str());
      else
        env.errors = 0;
      break;
    }
    case WatTokens::SCRIPT:
    case WatTokens::INPUT:
    case WatTokens::OUTPUT:
    {
      assert(false);
      WatSkipSection(tokens);
      EXPECTED(env, tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
      break;
    }
    default:
    {
      // If we get an unexpected token, try to parse it as an inline module
      WatToken t  = WatToken{ WatTokens::NONE };
      env.modules = trealloc<Module>(env.modules, ++env.n_modules);
      if(!env.modules)
        return ERR_FATAL_OUT_OF_MEMORY;
      auto name = IN_TEMP_PREFIX + std::to_string(env.n_modules);

      last = &env.modules[env.n_modules - 1];
      tokens.SetPosition(tokens.GetPosition() - 1); // Recover the '('

      if(err =
           WatParser::ParseModule(env, *last, file.u8string().c_str(), tokens, StringSpan{ name.data(), name.size() }, t))
        return err;

      tokens.SetPosition(tokens.GetPosition() - 1); // Recover the ')'
      break;
    }
    }

    EXPECTED(env, tokens, WatTokens::CLOSE, ERR_WAT_EXPECTED_CLOSE);
  }

  // If cache is null we must ensure we've at least tried to compile the test even if there's nothing to run.
  if(always_compile && !cache)
  {
    if(err = CompileWast(env, GenUniquePath(env, targetpath, counter), cache, cachepath))
      return err;
    assert(cache);
  }

  InvalidateCache(env, cache, cachepath);
  env.errors = errors;
  if(env.errors)
    internal::ReverseErrorList(env.errors);
  return ERR_SUCCESS;
}
