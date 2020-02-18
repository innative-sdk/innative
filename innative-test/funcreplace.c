__attribute__((visibility("default"))) int replace_me(int a, int b) { return a; }

int (*testfn)(int, int) = &replace_me;

__attribute__((visibility("default"))) extern int test(int i, int j)
{
  return (*testfn)(i, j);
}
