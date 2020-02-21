// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef INCLUDE_STD_FILESYSTEM_EXPERIMENTAL

#if defined(__cpp_lib_filesystem)
  #define INCLUDE_STD_FILESYSTEM_EXPERIMENTAL 0
#elif defined(__cpp_lib_experimental_filesystem)
  #define INCLUDE_STD_FILESYSTEM_EXPERIMENTAL 1
#elif !defined(__has_include)
  #define INCLUDE_STD_FILESYSTEM_EXPERIMENTAL 1
#elif __has_include(<filesystem>)
  #ifdef _MSC_VER
    #if __has_include(<yvals_core.h>)
      #include <yvals_core.h>
      #if defined(_HAS_CXX17) && _HAS_CXX17
        #define INCLUDE_STD_FILESYSTEM_EXPERIMENTAL 0
      #endif
    #endif

    #ifndef INCLUDE_STD_FILESYSTEM_EXPERIMENTAL
      #define INCLUDE_STD_FILESYSTEM_EXPERIMENTAL 1
    #endif

  #else
    #define INCLUDE_STD_FILESYSTEM_EXPERIMENTAL 0
  #endif

#elif __has_include(<experimental/filesystem>)
  #define INCLUDE_STD_FILESYSTEM_EXPERIMENTAL 1
#else
  #error Cant determine if filesystem is experimental or not
#endif

#if INCLUDE_STD_FILESYSTEM_EXPERIMENTAL
  #include <experimental/filesystem>
using namespace std::experimental::filesystem;
#else
  #include <filesystem>
using namespace std::filesystem;
#endif

#endif
