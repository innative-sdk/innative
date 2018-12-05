// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "test.h"
#include "innative/path.h"

using namespace innative;

void TestHarness::test_path()
{
  static const char* FOO = "foo";
  static const char* BAR = "bar";
  static const char* FOOBAR = "foobar";
  static const char* FILE = "file.ext";
  static const char* EXT = ".ext";

  Path empty;
  TEST(*empty.c_str() == 0);
  TEST(!empty.IsAbsolute());
  TEST(*empty.RemoveExtension().c_str() == 0);
  TEST(*empty.Extension().c_str() == 0);
  TEST(*empty.BaseDir().c_str() == 0);
  TEST(*empty.File().c_str() == 0);

  Path p(FOO);
  TEST(!strcmp(p.c_str(), FOO));
  TEST(!p.IsAbsolute());
  TEST(!strcmp(p.RemoveExtension().c_str(), FOO));
  TEST(*p.Extension().c_str() == 0);
  TEST(*p.BaseDir().c_str() == 0);
  TEST(!strcmp(p.File().c_str(), FOO));

  p += empty;
  TEST(!strcmp(p.c_str(), FOO));

  auto p2 = p + empty;
  TEST(!strcmp(p2.c_str(), FOO));
  TEST(!p2.IsAbsolute());
  TEST(!strcmp(p2.RemoveExtension().c_str(), FOO));
  TEST(*p2.Extension().c_str() == 0);
  TEST(*p2.BaseDir().c_str() == 0);
  TEST(!strcmp(p2.File().c_str(), FOO));

  TEST(!strcmp((empty + p).c_str(), FOO));

  Path f(FILE);
  TEST(!strcmp(f.c_str(), FILE));
  TEST(!f.IsAbsolute());
  TEST(!strcmp(f.RemoveExtension().c_str(), "file"));
  TEST(!strcmp(f.Extension().c_str(), "ext"));
  TEST(*f.BaseDir().c_str() == 0);
  TEST(!strcmp(f.File().c_str(), FILE));

  auto ftest1 = [this](const Path& x) {
    TEST(!x.IsAbsolute());
    char test[5] = "foo1";
    test[3] = Path::SEPERATOR;
    TEST(!strcmp(x.BaseDir().c_str(), test));
    TEST(*x.BaseDir().BaseDir().c_str() == 0);
    TEST(!strcmp(x.File().c_str(), FILE));
    TEST(!strcmp(x.File().File().c_str(), FILE));
    TEST(!strcmp(x.File().RemoveExtension().c_str(), "file"));
    TEST(!strcmp(x.Extension().c_str(), "ext"));
  };

  ftest1(p2 + f);
  ftest1(p2 + FILE);
  ftest1(Path("foo") + FILE);
  ftest1(Path("foo/") + FILE);
  ftest1(Path("foo\\") + FILE);
  ftest1(Path("foo") + Path("/file.ext"));
  ftest1(Path("foo/") + Path("/file.ext"));
  ftest1(Path("foo\\") + Path("/file.ext"));
  ftest1(Path("foo") + Path("\\file.ext"));
  ftest1(Path("foo/") + Path("\\file.ext"));
  ftest1(Path("foo\\") + Path("\\file.ext"));
  ftest1(Path("foo/file.ext"));
  ftest1(Path("foo\\file.ext"));

  p2 += FILE;
  ftest1(p2);

#ifdef IR_PLATFORM_WIN32
  Path rootyes("C:\\");
  Path rootno("/");
#else
  Path rootno("C:\\");
  Path rootyes("/");
#endif

  TEST(rootyes.IsAbsolute());
  TEST(!rootno.IsAbsolute());

  rootyes += p2;
  rootno += "foo\\file.ext";

  TEST(rootyes.IsAbsolute());
  TEST(!strcmp(rootyes.File().c_str(), FILE));
  TEST(!strcmp(rootyes.File().File().c_str(), FILE));
  TEST(!strcmp(rootyes.File().RemoveExtension().c_str(), "file"));
  TEST(!strcmp(rootyes.Extension().c_str(), "ext"));
  TEST(!rootno.IsAbsolute());
  TEST(!strcmp(rootno.File().c_str(), FILE));
  TEST(!strcmp(rootno.File().File().c_str(), FILE));
  TEST(!strcmp(rootno.File().RemoveExtension().c_str(), "file"));
  TEST(!strcmp(rootno.Extension().c_str(), "ext"));

  rootyes = rootyes.BaseDir();
  rootno = rootno.BaseDir();

  TEST(rootyes.IsAbsolute());
  TEST(!rootno.IsAbsolute());

  rootyes = rootyes.BaseDir();
  rootno = rootno.BaseDir();

  TEST(rootyes.IsAbsolute());
  TEST(!rootno.IsAbsolute());

#ifdef IR_PLATFORM_WIN32
  Path p3(FOOBAR);
  p3 += p2;
  TEST(!strcmp(p3.c_str(), "foobar\\foo\\file.ext"));
  p3 += "test";
  TEST(!strcmp(p3.c_str(), "foobar\\foo\\file.ext\\test"));
  TEST(!strcmp(rootyes.c_str(), "C:\\"));
  TEST(!strcmp(rootno.c_str(), "\\"));
#else
  Path p3(FOOBAR);
  p3 += p2;
  TEST(!strcmp(p3.c_str(), "foobar/foo/file.ext"));
  p3 += "test";
  TEST(!strcmp(p3.c_str(), "foobar/foo/file.ext/test"));
  TEST(!strcmp(rootno.c_str(), "C:/"));
  TEST(!strcmp(rootyes.c_str(), "/"));
#endif
}