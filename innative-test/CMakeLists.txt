cmake_minimum_required(VERSION 3.13.4)
project(innative-test LANGUAGES CXX VERSION ${PROJECT_VERSION})

file(GLOB_RECURSE test_SOURCES "./*.cpp")

if(WIN32)
  list(APPEND test_SOURCES "innative-test.rc")
else()
  find_package(Threads REQUIRED)
  find_library(LIB_RT rt REQUIRED)
endif()

add_compile_definitions(TESTING_WASM)

if(MSVC)
  string(REGEX REPLACE "/EH[a-z]+" "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
else()
  string(REPLACE "-fexceptions" "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
endif()

# add library and include directories
add_executable(innative-test ${test_SOURCES} ${CMAKE_SOURCE_DIR}/scripts/wasm_malloc.c)
target_include_directories(innative-test PUBLIC ${CMAKE_SOURCE_DIR}/include)
set_target_properties(innative-test PROPERTIES 
  POSITION_INDEPENDENT_CODE OFF
  C_STANDARD 17
  CXX_STANDARD 17
  CXX_EXTENSIONS OFF
  CXX_STANDARD_REQUIRED ON
  VERBOSE_MAKEFILE TRUE
)

if(MSVC)
  target_link_libraries(innative-test innative innative-env)
  target_compile_options(innative-test PRIVATE /Zc:preprocessor $<$<CONFIG:Release>:/Oi /Ot /GL> ${CPP_WARNINGS})
else()
  target_link_libraries(innative-test innative innative-env stdc++fs)
  #target_compile_options(${target} PRIVATE -fno-exceptions -fno-rtti $<IF:$<CONFIG:Debug>,-g,-O3 -msse -msse2 -msse3 -mmmx -m3dnow -mcx16> ${CPP_WARNINGS})
  target_compile_options(innative-test PRIVATE -fno-exceptions -fno-rtti -pthread -DLIBICONV_PLUG $<IF:$<CONFIG:Debug>,-g,-O3 -msse -msse2 -msse3 -mmmx -m3dnow -mcx16> ${CPP_WARNINGS})
  target_compile_definitions(innative-test PUBLIC $<IF:$<CONFIG:Debug>,DEBUG,NDEBUG>)
endif()

if(WIN32)
  # Cmake does not support linking with a static library
  if(NOT BUILD_SHARED_LIBS)
    set_target_properties(innative-test PROPERTIES COMPILE_PDB_NAME "$(TargetName)") 
    set_target_properties(innative-test PROPERTIES OUTPUT_NAME "innative-test-s")
  endif()
  
  set_source_files_properties("innative-test.rc" PROPERTIES LANGUAGE RC)
else()
  target_link_libraries(innative-test ${CMAKE_THREAD_LIBS_INIT} ${LIB_RT})
endif()
