// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

struct Complex
{
  int a;
  float b;
  struct Complex* c;

  Complex() : a(0), b(1.0) {}
  explicit Complex(int i) : a(i), b(1.0) {}
};

/*struct Foo : virtual Complex
{
  virtual ~Foo() {}
  virtual float get() { return 3.0f + a; }
};

struct Bar : virtual Complex
{
  virtual ~Bar() {}
  virtual float get() { return 5.0f + a; }
};

struct FooBar : Foo, Bar
{
  FooBar(int i) { a = i; }
  virtual ~FooBar() {}
  virtual float get() { return Foo::get(); }
};*/

struct FooBar : Complex
{
  long long d;
};

extern "C" int converti(Complex* p) { return (int)(long long)p; }
extern "C" float convertf(int p) { return (float)p; }
extern "C" int addi(int a, int b) { return a + b; }

#ifdef TESTING_WASM
  #include "benchmark.h"
  #include <stdint.h>

float (*testfn)(int) = nullptr;

int Benchmarks::debug(int n)
#else
  #include <stdint.h>

__attribute__((visibility("default"))) float (*testfn)(int) = nullptr;

extern "C" void* malloc(uintptr_t n);
extern "C" __attribute__((visibility("default"))) int debug(int n)
#endif
{
  Complex* f = (Complex*)malloc(sizeof(Complex) * 3);
  f[0].a     = n + 1;
  f[1].a     = n * n;
  f[2].a     = n * n * n;

  testfn = &convertf;
  f[0].b = (*testfn)(converti(&f[1]));

  auto& f1 = f[1].a;
  f1       = n * n + 1;

  FooBar* foobar = (FooBar*)malloc(sizeof(FooBar));
  f[0].c         = foobar;

  f[0].c = &f[1];
  f[1].c = &f[2];
  f[2].c = &f[0];

  return f[0].c->c->c->a + addi(f[0].c->a, f[0].a);
}