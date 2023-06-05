// Copyright (c)2023 Fundament Software
// For conditions of distribution and use, see copyright notice in innative.h

#include "test.h"
#include <functional>
#include "uvwasi.h"

using namespace innative;

extern "C" {
void* __innative_internal_uvwasi_register_context();
void __innative_internal_uvwasi_destroy_context(void* ctx);
}

int TestHarness::do_wasi(void* assembly) { return ERR_SUCCESS; }

void TestHarness::test_wasi()
{
  {
    // First call uvwasi directly and test it
    /* uvwasi_t* uvwasi = static_cast<uvwasi_t*>(__innative_internal_uvwasi_register_context());
    TEST(uvwasi != nullptr);
    if(uvwasi != nullptr)
    {
      TEST(uvwasi->argc >= 1);
      TEST(uvwasi->argv != nullptr);
      uvwasi_timestamp_t res = (uvwasi_timestamp_t)~0;
      uvwasi_clock_res_get(uvwasi, UVWASI_CLOCK_REALTIME, &res);
      TEST(res != (uvwasi_timestamp_t)~0);
    }
    __innative_internal_uvwasi_destroy_context(uvwasi);*/
  }

  auto lambda = [&](Environment* env) -> int {
    int err = (*_exports.SetupWASI)(env, IN_WASI_PREVIEW_1, (env->flags & ENV_DEBUG));

    TEST(!err);

    return err;
  };

  TEST(CompileWASM("../scripts/wasi-hello-world.wat", &TestHarness::do_wasi, "", lambda) == ERR_SUCCESS);
}