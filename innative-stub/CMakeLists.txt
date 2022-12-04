cmake_minimum_required(VERSION 3.13.4)
project(innative-stub LANGUAGES C VERSION ${PROJECT_VERSION})

set(CMAKE_POSITION_INDEPENDENT_CODE ON)

list(APPEND stub_SOURCES
    stub.c
)

if(MSVC)
  string(REGEX REPLACE "/EH[a-z]+" "" CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
else()
  string(REPLACE "-fexceptions" "" CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
endif()

# add library and include directories
add_library(innative-stub STATIC ${stub_SOURCES})
set_property(TARGET innative-stub PROPERTY C_STANDARD 17)
set_property(TARGET innative-stub PROPERTY VERBOSE_MAKEFILE TRUE)

if(MSVC)
  target_compile_options(innative-stub PRIVATE /Zc:preprocessor $<$<CONFIG:Release>:/Oi /Ot /GL> ${CPP_WARNINGS})
else()
  target_compile_options(innative-stub PRIVATE -fno-exceptions -fno-stack-protector $<IF:$<CONFIG:Debug>,-g,-O3 -msse -msse2 -msse3 -mmmx -m3dnow -mcx16> ${CPP_WARNINGS})
  target_compile_definitions(innative-stub PUBLIC $<IF:$<CONFIG:Debug>,DEBUG,NDEBUG>)
endif()

target_include_directories(innative-stub PUBLIC ${CMAKE_SOURCE_DIR}/include)
retarget_output(innative-stub)

set_target_properties(innative-stub PROPERTIES COMPILE_PDB_NAME "$(TargetName)")

if(WIN32)
  # Cmake does not support linking with a static library
  if(NOT BUILD_SHARED_LIBS)
    set_target_properties(innative-stub PROPERTIES OUTPUT_NAME "innative-stub-s")
  endif()
else()
  target_link_libraries(innative-stub ${CMAKE_DL_LIBS})
endif()