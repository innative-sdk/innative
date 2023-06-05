#include "innative/innative.h"

#ifdef IN_PLATFORM_WIN32
  #include "../innative/win32.h"
  #include <shellapi.h>

#else
#endif

#include "uv.h"
#include "uvwasi.h"
#include <malloc.h>

IN_COMPILER_DLLEXPORT extern void* __innative_internal_uvwasi_register_context()
{
  uvwasi_t* uvwasi = calloc(1, sizeof(uvwasi_t));
  uvwasi_options_t init_options;
  uvwasi_errno_t err;

  /* Setup the initialization options. */
  init_options.in            = 0;
  init_options.out           = 1;
  init_options.err           = 2;
  init_options.fd_table_size = 3;
  init_options.envp          = NULL;
  init_options.preopenc      = 0;
  init_options.allocator     = NULL;

#ifdef IN_PLATFORM_WIN32
  {
    int argc;
    LPWSTR* argw      = CommandLineToArgvW(GetCommandLineW(), &argc);
    init_options.argc = argc;

    if(!argc)
      init_options.argv = NULL;
    else
    {
      init_options.argv = (const char**)alloca(argc * sizeof(char*));

      for(int i = 0; i < argc; ++i)
      {
        int len              = (int)wcslen(argw[i]);
        auto sz              = WideCharToMultiByte(CP_UTF8, 0, argw[i], len, NULL, 0, NULL, NULL);
        init_options.argv[i] = (const char*)alloca(sz + 1);
        WideCharToMultiByte(CP_UTF8, 0, argw[i], len, (char*)init_options.argv[i], sz, NULL, NULL);
      }
    }

    LocalFree(argw);
  }
#else
  #error "not implemented"
#endif

  /* Initialize the sandbox. */
  err = uvwasi_init(uvwasi, &init_options);
  return err == UVWASI_ESUCCESS ? uvwasi : NULL;
}

IN_COMPILER_DLLEXPORT extern void __innative_internal_uvwasi_destroy_context(void* ctx)
{
  if(ctx)
  {
    uvwasi_t* ptr   = (uvwasi_t*)ctx;
    uvwasi_destroy(ptr);
    free(ptr);
  }
}