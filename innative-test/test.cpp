// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "innative/export.h"
#include <iostream>
#include <filesystem>
#include <fstream>

using namespace std::filesystem;

// This defines the testing environment that we need to inject
const char testenv[] = "(module $spectest "
"\n  (global $global_i32 (export \"global_i32\") i32 i32.const 0)"
"\n  (global $global_i64 (export \"global_i64\") i64 i64.const 0)"
"\n  (global $global_f32 (export \"global_f32\") f32 f32.const 0)"
"\n  (global $global_f64 (export \"global_f64\") f64 f64.const 0)"
"\n  (memory $memory1 (export \"memory\") 1)"
"\n  (table $table10 (export \"table\") 10 anyfunc)"
"\n  (func $print (export \"print\"))"
"\n  (func $print_i32 (export \"print_i32\") (param i32))"
"\n  (func $print_i64 (export \"print_i64\") (param i64))"
"\n  (func $print_f32 (export \"print_f32\") (param f32))"
"\n  (func $print_f64 (export \"print_f64\") (param f64))"
"\n  (func $print_i32_f32 (export \"print_i32_f32\") (param i32 f32))"
"\n  (func $print_f64_f64 (export \"print_f64_f64\") (param f64 f64))"
"\n) (register \"spectest\" $spectest)";

int main(int argc, char *argv[])
{
  innative_set_work_dir_to_bin(!argc ? 0 : argv[0]);
  IRExports exports;
  innative_runtime(&exports);

  //std::ostream& target = std::cout;
  std::ofstream target("out.txt", std::fstream::binary | std::fstream::out);
  target << "inNative v" << INNATIVE_VERSION_MAJOR << "." << INNATIVE_VERSION_MINOR << "." << INNATIVE_VERSION_REVISION << " Test Utility" << std::endl;
  target << std::endl;

  path testdir("../spec/test/core");
  std::vector<path> testfiles;

  for(auto& p : recursive_directory_iterator(testdir, directory_options::skip_permission_denied))
  {
    if(!STRICMP(p.path().extension().u8string().data(), ".wast"))
      testfiles.push_back(p.path());
  }

  testfiles = { "../spec/test/core/call.wast" };
  target << "Running through " << testfiles.size() << " official webassembly spec tests." << std::endl;
  //testfiles.erase(testfiles.begin(), testfiles.begin() + 50);

  for(auto file : testfiles)
  {
    Environment* env = (*exports.CreateEnvironment)(ENV_DEBUG | ENV_EMIT_LLVM | ENV_STRICT, 1, 0, (!argc ? 0 : argv[0]));
    env->linker = "C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\VC\\Tools\\MSVC\\14.15.26726\\bin\\Hostx64\\x64\\link.exe";
    int err = (*exports.AddEmbedding)(env, 0, (void*)INNATIVE_DEFAULT_ENVIRONMENT, 0);

    if(err >= 0)
      err = innative_compile_script(reinterpret_cast<const uint8_t*>(testenv), sizeof(testenv), env);
    if(err < 0)
      return assert(false), -1; // If the environment injection fails, abort everything

    err = innative_compile_script((const uint8_t*)file.generic_u8string().data(), 0, env);
    if(!err && !env->errors)
      target << file << ": SUCCESS" << std::endl;
    else
    {
      if(err < 0)
        target << "Error running script " << file << ": " << err << std::endl;
      target << file << ": FAILED" << std::endl;
      while(env->errors != nullptr)
      {
        target << "  " << env->errors->error << std::endl;
        env->errors = env->errors->next;
      }
      target << std::endl;
    }
    (*exports.DestroyEnvironment)(env);
  }

  // Test compiling EXE
  // Test compiling DLL with no entry point, ensure init function is called
  // Test compiling DLL with entry point that gets called in the init function
  // Test compiling DLL with entry point that doesn't get called in init function

  std::cout << std::endl << "Finished running tests, press enter to exit." << std::endl;
  getchar();
  return 0;
}