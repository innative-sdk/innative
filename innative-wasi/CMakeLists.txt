cmake_minimum_required(VERSION 3.13.4)
project(innative-wasi LANGUAGES C VERSION ${PROJECT_VERSION})

find_package(uvwasi CONFIG REQUIRED)
find_package(LibUV CONFIG REQUIRED)

list(APPEND wasi_SOURCES
    wasi.c
)

if(MSVC)
  string(REGEX REPLACE "/EH[a-z]+" "" CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
  string(REPLACE "/RTC1" "" CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
endif()

# add library and include directories
add_library(innative-wasi STATIC ${wasi_SOURCES})
add_library(innative-wasi-d STATIC ${wasi_SOURCES})

set_property(TARGET innative-wasi PROPERTY C_STANDARD 17)
set_property(TARGET innative-wasi-d PROPERTY C_STANDARD 17)
set_property(TARGET innative-wasi PROPERTY VERBOSE_MAKEFILE TRUE)
set_property(TARGET innative-wasi-d PROPERTY VERBOSE_MAKEFILE TRUE)

# Directly set target compilation settings to bypass Release/Debug modes
set(CMAKE_BUILD_TYPE "")

if(MSVC)
  target_compile_options(innative-wasi PRIVATE /WX- /GS-)
  target_compile_options(innative-wasi-d PRIVATE /Zi /Od /Ob0 /Gd /Gm- /GS-)
else()
  target_compile_options(innative-wasi PRIVATE -O3 -msse -msse2 -msse3 -mmmx -m3dnow -mcx16 -fno-stack-protector -Wno-unused-function ${C_WARNINGS})
  target_compile_options(innative-wasi-d PRIVATE -O3 -g3 -fno-stack-protector -Wno-unused-function ${C_WARNINGS})
endif()

target_compile_definitions(innative-wasi PUBLIC NDEBUG)
target_compile_definitions(innative-wasi-d PUBLIC DEBUG)
target_include_directories(innative-wasi PUBLIC ${CMAKE_SOURCE_DIR}/include)
target_include_directories(innative-wasi-d PUBLIC ${CMAKE_SOURCE_DIR}/include)
set_target_properties(innative-wasi PROPERTIES DEBUG_POSTFIX "" POSITION_INDEPENDENT_CODE ON)
set_target_properties(innative-wasi-d PROPERTIES DEBUG_POSTFIX "" POSITION_INDEPENDENT_CODE ON)
retarget_lib(innative-wasi TRUE)
retarget_lib(innative-wasi-d TRUE)

#uvwasi only builds statically right now
target_link_libraries(innative-wasi PRIVATE uvwasi::uvwasi_a)
target_link_libraries(innative-wasi-d PRIVATE uvwasi::uvwasi_a)

if(WIN32)  
  get_property(UVWASI_LOCATION TARGET uvwasi::uvwasi_a PROPERTY LOCATION)
  set(RLIST psapi.lib shell32.lib ole32.lib uuid.lib advapi32.lib delayimp.lib kernel32.lib user32.lib gdi32.lib winspool.lib shell32.lib ole32.lib oleaut32.lib uuid.lib comdlg32.lib advapi32.lib
iphlpapi.lib userenv.lib ws2_32.lib $<TARGET_FILE:uvwasi::uvwasi_a> $<TARGET_FILE:uv_a>)
  set(RLIST_DEBUG psapi.lib shell32.lib ole32.lib uuid.lib advapi32.lib delayimp.lib kernel32.lib user32.lib gdi32.lib winspool.lib shell32.lib ole32.lib oleaut32.lib uuid.lib comdlg32.lib advapi32.lib
iphlpapi.lib userenv.lib ws2_32.lib $<TARGET_FILE:uvwasi::uvwasi_a> $<TARGET_FILE:uv_a>)
  list(JOIN RLIST "$<SEMICOLON>" FIXED_RLIST)
  list(JOIN RLIST_DEBUG "$<SEMICOLON>" FIXED_RLIST_DEBUG)
  # libucrtd.lib libvcruntimed.lib
  # Cmake does not support linking with a static library
  set_target_properties(innative-wasi PROPERTIES COMPILE_PDB_NAME "$(TargetName)" STATIC_LIBRARY_OPTIONS ${FIXED_RLIST})
  set_target_properties(innative-wasi-d PROPERTIES COMPILE_PDB_NAME "$(TargetName)" STATIC_LIBRARY_OPTIONS ${FIXED_RLIST_DEBUG})
endif()
