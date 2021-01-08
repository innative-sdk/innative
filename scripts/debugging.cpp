// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include <stdint.h>
#ifdef _MSC_VER
  #define MODULE_EXPORT
#else
  #define MODULE_EXPORT __attribute__((visibility("default")))
#endif

extern "C" void* malloc(uintptr_t n);
extern "C" void free(void* p);
void* operator new(uintptr_t size) { return malloc(size); }
void operator delete(void* p) noexcept { free(p); }
void* operator new[](uintptr_t size) { return malloc(size); }
void operator delete[](void* p) noexcept { free(p); }

struct CTest
{
  int a;
  float b;
  struct CTest* c;
  short z[3];

  CTest() : a(0), b(1.0) {}
  explicit CTest(int i) : a(i), b(1.0) {}
};

struct FooBar : CTest
{
  long long d;
};

enum FOOENUM
{
  ENUM_FAKE = 0,
  ENUM_USED = 1,
};

class CBase
{
public:
  CBase() : a(1234) {}

  int a;
};

union CIntFloat
{
  int i;
  float f;
};

namespace Test {
  class CDerived : CBase
  {
  public:
    CIntFloat a;
  };

  class CTask
  {
  public:
    int Run(void)
    {
      CBase* pBase       = new CBase;
      CDerived* pDerived = new CDerived;
      pDerived->a.i        = pBase->a;
      int a              = pDerived->a.i;
      CIntFloat* b = &pDerived->a;
      delete pDerived;
      delete pBase;
      return a;
    }
  };
}

extern "C" int converti(CTest* p) { return (int)(long long)p; }
extern "C" float convertf(int p) { return (float)p; }
extern "C" int addi(int a, int b) { return a + b; }

float (*testfn)(int) = nullptr;

extern "C" MODULE_EXPORT int debug(int n)
{
  auto f     = new CTest[3];
  int test[] = { 1, 2, 3 };
  f->z[0]    = test[0];
  f->z[1]    = test[1];
  f->z[2]    = test[2];
  f[0].a     = n + f->z[0];
  f[1].a     = n * n;
  f[2].a     = n * n * n;

  testfn = &convertf;
  auto testfn2 = testfn;
  f[0].b = (*testfn2)(converti(&f[1]));
  
  auto& f1 = f[1].a;
  f1       = n * n + ENUM_USED;

  auto lambda = [&]() {
    FooBar* foobar = (FooBar*)malloc(sizeof(FooBar));
    f[0].c         = foobar;
  };
  lambda();

  CTest** pf = &f;
  *pf        = f;

  f[0].c = &f[1];
  f[1].c = &f[2];
  f[2].c = &f[0];

  CBase* pBase2 = new CBase;
  Test::CTask t;
  int result;
  switch(t.Run())
  {
  case 0:
  case 1:
    result = 50;
    break;
  case 1234:
    result = 1; 
    break;
  default: 
    result = 0;
    break;
  }

  return f[0].c->c->c->a + addi(f[0].c->a, f[0].a) + result;
}

/*extern "C" MODULE_EXPORT int debug(int n)
{
  int a;
  a = 2;
  a = 4;
  int res = a;
  return res;
}*/

  // Only used to build this as a normal EXE for comparing debug information
//int main() { return debug(4); }