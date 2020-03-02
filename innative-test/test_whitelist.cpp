// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "test.h"
#include "../innative/utility.h"
#include <functional>

using namespace innative;
using namespace utility;

void TestHarness::test_whitelist()
{
  typedef std::pair<const char*, const char*> namepair;

  auto fn = [this](std::initializer_list<namepair> pairs, void (*setup)(Environment*, const INExports&)) -> int {
    std::string out            = "(module $whitelist ";
    constexpr char importdef[] = "\n  (import \"%s\" \"%s\" (func $%s (result i32)))";

    for(auto& p : pairs)
    {
      std::string buf;
      buf.resize(snprintf(0, 0, importdef, p.first, p.second, p.second) + 1);
      buf.resize(snprintf(const_cast<char*>(buf.data()), buf.size(), importdef, p.first, p.second, p.second) + 1);
      out += buf;
    }

    out += "\n)";

    Environment* env = (*_exports.CreateEnvironment)(1, 0, 0);
    env->flags       = ENV_LIBRARY | ENV_STRICT | ENV_WHITELIST | ENV_ENABLE_WAT;
    env->features    = ENV_FEATURE_ALL;
    env->loglevel    = LOG_FATAL;
    (*setup)(env, _exports);

    int err = 0;
    (*_exports.AddModule)(env, out.data(), out.size(), "whitelist", &err);
    TEST(err == 0);
    (*_exports.FinalizeEnvironment)(env);
#if defined(IN_PLATFORM_WIN32) && defined(IN_CPU_x86)
    AddCImport(*env, "_asdf");
    AddCImport(*env, "_asdf2");
    AddCImport(*env, "_a");
    AddCImport(*env, "_A");
    AddCImport(*env, "_AB");
    AddCImport(*env, "_yz");
    AddCImport(*env, "_[fake]");
#else
    AddCImport(*env, "asdf");
    AddCImport(*env, "asdf2");
    AddCImport(*env, "a");
    AddCImport(*env, "A");
    AddCImport(*env, "AB");
    AddCImport(*env, "yz");
    AddCImport(*env, "[fake]");
#endif
    err = (*_exports.Validate)(env);
    if(err != ERR_SUCCESS)
      err = env->errors->code;

    (*_exports.DestroyEnvironment)(env);

    return err;
  };

  TEST(fn({ { "asdf", "asdf" } }, [](Environment* e, const INExports&) { e->flags &= ~ENV_WHITELIST; }) ==
       ERR_UNKNOWN_MODULE);
  TEST(fn({ { "", "asdf" } }, [](Environment* e, const INExports&) { e->flags &= ~ENV_WHITELIST; }) == ERR_SUCCESS);
  TEST(fn({ { "sys", "asdf" } }, [](Environment* e, const INExports&) {
         e->system = "sys";
         e->flags &= ~ENV_WHITELIST;
       }) == ERR_SUCCESS);

  TEST(fn({ { "sys", "asdf" } }, [](Environment* e, const INExports&) {}) == ERR_UNKNOWN_MODULE);
  TEST(fn({ { "sys", "asdf" } }, [](Environment* e, const INExports&) { e->system = "sys"; }) == ERR_ILLEGAL_C_IMPORT);
  TEST(fn({ { "asdf", "asdf" } }, [](Environment* e, const INExports&) {}) == ERR_UNKNOWN_MODULE);
  TEST(fn({ { "", "asdf" } }, [](Environment* e, const INExports&) {}) == ERR_ILLEGAL_C_IMPORT);
  TEST(fn({ { "", "_innative_to_c" } }, [](Environment* e, const INExports&) {}) == ERR_ILLEGAL_C_IMPORT);
  TEST(fn({ { "", "_innative_from_c" } }, [](Environment* e, const INExports&) {}) == ERR_ILLEGAL_C_IMPORT);

  TEST(fn({ { "", "asdf" } }, [](Environment* e, const INExports& exp) { (*exp.AddWhitelist)(e, "", "asdf"); }) ==
       ERR_SUCCESS);
  TEST(fn({ { "", "asdf" } }, [](Environment* e, const INExports& exp) { (*exp.AddWhitelist)(e, "asdf", "asdf"); }) ==
       ERR_ILLEGAL_C_IMPORT);
  TEST(fn({ { "", "asdf" } }, [](Environment* e, const INExports& exp) { (*exp.AddWhitelist)(e, "", "asdf2"); }) ==
       ERR_ILLEGAL_C_IMPORT);
  TEST(fn({ { "", "asdf" } }, [](Environment* e, const INExports& exp) {
         e->system = "sys";
         (*exp.AddWhitelist)(e, "sys", "asdf");
       }) == ERR_SUCCESS);
  TEST(fn({ { "sys", "asdf" } }, [](Environment* e, const INExports& exp) {
         e->system = "sys";
         (*exp.AddWhitelist)(e, "", "asdf");
       }) == ERR_SUCCESS);
  TEST(fn({ { "sys", "asdf" } }, [](Environment* e, const INExports& exp) {
         e->system = "sys";
         (*exp.AddWhitelist)(e, "sys", "asdf");
       }) == ERR_SUCCESS);
  TEST(fn({ { "", "asdf" } }, [](Environment* e, const INExports& exp) {
         (*exp.AddWhitelist)(e, "", "a");
         (*exp.AddWhitelist)(e, "", "b");
       }) == ERR_ILLEGAL_C_IMPORT);

  // These whitelist names are designed to create hash collisions to expose a certain class of bugs.
  TEST(fn({ { "", "asdf" } }, [](Environment* e, const INExports& exp) {
         (*exp.AddWhitelist)(e, "", "a");
         (*exp.AddWhitelist)(e, "", "A");
       }) == ERR_ILLEGAL_C_IMPORT);
  TEST(fn({ { "", "a" }, { "", "A" } }, [](Environment* e, const INExports& exp) {
         (*exp.AddWhitelist)(e, "", "a");
         (*exp.AddWhitelist)(e, "", "A");
         (*exp.AddWhitelist)(e, "", "AB");
       }) == ERR_SUCCESS);
  TEST(fn({ { "", "a" }, { "", "A" }, { "", "yz" } }, [](Environment* e, const INExports& exp) {
         (*exp.AddWhitelist)(e, "", "a");
         (*exp.AddWhitelist)(e, "", "A");
         (*exp.AddWhitelist)(e, "", "AB");
       }) == ERR_ILLEGAL_C_IMPORT);
  TEST(fn({ { "", "asdf" } }, [](Environment* e, const INExports& exp) {
         e->system = "sys";
         (*exp.AddWhitelist)(e, "", "a");
         (*exp.AddWhitelist)(e, "", "A");
         (*exp.AddWhitelist)(e, "sys", "asdf");
       }) == ERR_SUCCESS);

  TEST(fn({ { "sys", "asdf" } }, [](Environment* e, const INExports& exp) {
         e->system = "sys";
         (*exp.AddWhitelist)(e, "asdf", "asdf");
       }) == ERR_ILLEGAL_C_IMPORT);
}