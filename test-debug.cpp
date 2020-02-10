// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

struct complex {
  int a;
  float b;
  struct complex* c;
  
  complex() : a(0), b(1.0) {}
  explicit complex(int i) : a(i), b(1.0) {}
  virtual ~complex() { delete c; }
  virtual float get() { return a + b; }
  virtual float get2() { return c->b + b; }
};

struct foo : virtual complex {
  virtual ~foo() {}
  virtual float get() { return 3.0f + a; }
};

struct bar : virtual complex {
  virtual ~bar() {}
  virtual float get() { return 5.0f + a; }
};

struct foobar : foo, bar {
  foobar(int i) { a = i; }
  virtual ~foobar() {}
  float get3() { return get2(); }
  virtual float get() { return foo::get(); }
};

#ifdef TESTING_WASM
  #include "benchmark.h"

int64_t Benchmarks::debug(int64_t n)
#else
extern "C" __attribute__((visibility("default"))) int debug(int n)
#endif
{
  if(n < 0)
    return n;
  
  n += debug(foobar{n - 1}.a) + debug(foobar{n - 2}.a);
  return n;
}
