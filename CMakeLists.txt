cmake_minimum_required(VERSION 3.15)
project(innative_sdk VERSION 0.2.0)
option(BUILD_SHARED_LIBS "Build all libraries as shared libraries instead of static" OFF)
option(USE_MIMALLOC "Replace the default allocator with mimalloc" OFF)
option(USE_DEFAULT_FOLDERS "Don't override the cmake output folders with a unified /bin/ folder. Also disables debug postfix." OFF)

if(MSVC)
  # This ensures that we default to static but let vcpkg configure things the way it wants
  set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>$<$<STREQUAL:${VCPKG_CRT_LINKAGE},dynamic>:DLL>" CACHE STRING "")
else()
  set(CPP_WARNINGS -Wall -Wno-attributes -Wno-unknown-pragmas -Wno-implicit-fallthrough -Wno-missing-braces -Wno-unused-function -Wno-comment -Wno-char-subscripts -Wno-sign-compare -Wno-unused-variable -Wno-switch -Wno-parentheses -Wno-reorder)
  set(C_WARNINGS -Wall -Wno-attributes -Wno-unknown-pragmas -Wno-implicit-fallthrough -Wno-unused-variable -Wno-switch -Wno-parentheses)
endif()

if (MSVC)
  if("${MSVC_C_ARCHITECTURE_ID}" STREQUAL "X86")
    set(TARGET_ARCH "i686")
  elseif("${MSVC_C_ARCHITECTURE_ID}" STREQUAL "x64")
    set(TARGET_ARCH "x86_64")
  elseif("${MSVC_C_ARCHITECTURE_ID}" STREQUAL "ARM")
    set(TARGET_ARCH "arm")
  else()
    message(FATAL_ERROR "Failed to determine the MSVC target architecture: ${MSVC_C_ARCHITECTURE_ID}")
  endif()
else()
  execute_process(
    COMMAND ${CMAKE_C_COMPILER} -dumpmachine
    RESULT_VARIABLE RESULT
    OUTPUT_VARIABLE TARGET_ARCH
    ERROR_QUIET
  )
  if (RESULT)
    message(FATAL_ERROR "Failed to determine target architecture triplet: ${RESULT}")
  endif()
  string(REGEX MATCH "([^-]+).*" ARCH_MATCH ${TARGET_ARCH})
  if (NOT CMAKE_MATCH_1 OR NOT ARCH_MATCH)
    message(FATAL_ERROR "Failed to match the target architecture triplet: ${TARGET_ARCH}")
  endif()
  set(TARGET_ARCH ${CMAKE_MATCH_1})
endif()
message(STATUS "Target architecture - ${TARGET_ARCH}")

if(TARGET_ARCH MATCHES "i386")
  set(TARGET_ARCH "i386")
elseif(TARGET_ARCH MATCHES "i486")
  set(TARGET_ARCH "i386")
elseif(TARGET_ARCH MATCHES "i586")
  set(TARGET_ARCH "i386")
elseif(TARGET_ARCH MATCHES "i686")
  set(TARGET_ARCH "i386")
elseif(TARGET_ARCH MATCHES "amd64")
  set(TARGET_ARCH "amd64")
elseif(TARGET_ARCH MATCHES "x86_64")
  set(TARGET_ARCH "amd64")
elseif(TARGET_ARCH MATCHES "xscale")
  set(TARGET_ARCH "arm")
elseif(TARGET_ARCH MATCHES "ARM")
  set(TARGET_ARCH "arm")
elseif(TARGET_ARCH MATCHES "AARCH64")
  set(TARGET_ARCH "aarch64")
elseif(TARGET_ARCH MATCHES "powerpc")
  set(TARGET_ARCH "ppc32")
elseif(TARGET_ARCH MATCHES "powerpcspe")
  set(TARGET_ARCH "ppc32")
elseif(TARGET_ARCH MATCHES "powerpc64")
  set(TARGET_ARCH "ppc64")
elseif(TARGET_ARCH MATCHES "ppu")
  set(TARGET_ARCH "ppc64")
elseif(TARGET_ARCH MATCHES "risc-v")
  set(TARGET_ARCH "riscv32")
elseif(TARGET_ARCH MATCHES "riscv32")
  set(TARGET_ARCH "riscv32")
else()
  message(FATAL_ERROR, "Can't figure out what architecture to build for!")
endif()
  
set(LIB_DIR "lib")

if (CMAKE_SYSTEM_NAME MATCHES "Windows")
    set(TARGET_SYS "windows")
    set(LIB_DIR "bin")
elseif(CMAKE_SYSTEM_NAME MATCHES "Linux")
    set(TARGET_SYS "linux")
elseif(CMAKE_SYSTEM_NAME MATCHES "Darwin")
    set(TARGET_SYS "posix")
elseif(CMAKE_SYSTEM_NAME MATCHES "FreeBSD")
    set(TARGET_SYS "freebsd")
else()
  message(FATAL_ERROR, "Can't figure out what platform we're on!")
endif()

set(BIN_DIR "bin")

function(retarget_bin target)
  if(NOT USE_DEFAULT_FOLDERS)
    set_target_properties(${target}
        PROPERTIES
        VS_DEBUGGER_WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
        ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
        LIBRARY_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
        RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
        ARCHIVE_OUTPUT_DIRECTORY_DEBUG "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
        LIBRARY_OUTPUT_DIRECTORY_DEBUG "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
        RUNTIME_OUTPUT_DIRECTORY_DEBUG "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
        ARCHIVE_OUTPUT_DIRECTORY_RELEASE "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
        LIBRARY_OUTPUT_DIRECTORY_RELEASE "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
        RUNTIME_OUTPUT_DIRECTORY_RELEASE "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
        ARCHIVE_OUTPUT_DIRECTORY_MINSIZEREL "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
        LIBRARY_OUTPUT_DIRECTORY_MINSIZEREL "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
        RUNTIME_OUTPUT_DIRECTORY_MINSIZEREL "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
        ARCHIVE_OUTPUT_DIRECTORY_RELWITHDEBINFO "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
        LIBRARY_OUTPUT_DIRECTORY_RELWITHDEBINFO "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
        RUNTIME_OUTPUT_DIRECTORY_RELWITHDEBINFO "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
        DEBUG_POSTFIX "-d"
        PREFIX ""
    )
  endif()
endfunction()

function(retarget_lib target isembed)
  if(NOT USE_DEFAULT_FOLDERS)
    if(isembed)
      set(isembed "/${TARGET_SYS}-${TARGET_ARCH}")
      set_target_properties(${target} PROPERTIES DEBUG_POSTFIX "")
    else()
      set(isembed "")
      set_target_properties(${target} PROPERTIES DEBUG_POSTFIX "-d")
    endif()
    
    if (CMAKE_SYSTEM_NAME MATCHES "Windows")
      set_target_properties(${target} PROPERTIES PREFIX "")
    else()
      set_target_properties(${target} PROPERTIES PREFIX "lib")
    endif()
    
    set_target_properties(${target}
        PROPERTIES
        VS_DEBUGGER_WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}/${LIB_DIR}${isembed}"
        ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/${LIB_DIR}${isembed}"
        LIBRARY_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/${LIB_DIR}${isembed}"
        RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/${LIB_DIR}${isembed}"
        ARCHIVE_OUTPUT_DIRECTORY_DEBUG "${CMAKE_SOURCE_DIR}/${LIB_DIR}${isembed}"
        LIBRARY_OUTPUT_DIRECTORY_DEBUG "${CMAKE_SOURCE_DIR}/${LIB_DIR}${isembed}"
        RUNTIME_OUTPUT_DIRECTORY_DEBUG "${CMAKE_SOURCE_DIR}/${LIB_DIR}${isembed}"
        ARCHIVE_OUTPUT_DIRECTORY_RELEASE "${CMAKE_SOURCE_DIR}/${LIB_DIR}${isembed}"
        LIBRARY_OUTPUT_DIRECTORY_RELEASE "${CMAKE_SOURCE_DIR}/${LIB_DIR}${isembed}"
        RUNTIME_OUTPUT_DIRECTORY_RELEASE "${CMAKE_SOURCE_DIR}/${LIB_DIR}${isembed}"
        ARCHIVE_OUTPUT_DIRECTORY_MINSIZEREL "${CMAKE_SOURCE_DIR}/${LIB_DIR}${isembed}"
        LIBRARY_OUTPUT_DIRECTORY_MINSIZEREL "${CMAKE_SOURCE_DIR}/${LIB_DIR}${isembed}"
        RUNTIME_OUTPUT_DIRECTORY_MINSIZEREL "${CMAKE_SOURCE_DIR}/${LIB_DIR}${isembed}"
        ARCHIVE_OUTPUT_DIRECTORY_RELWITHDEBINFO "${CMAKE_SOURCE_DIR}/${LIB_DIR}${isembed}"
        LIBRARY_OUTPUT_DIRECTORY_RELWITHDEBINFO "${CMAKE_SOURCE_DIR}/${LIB_DIR}${isembed}"
        RUNTIME_OUTPUT_DIRECTORY_RELWITHDEBINFO "${CMAKE_SOURCE_DIR}/${LIB_DIR}${isembed}"
    )
  endif()
endfunction()

function(standard_target_flags target)
  set_property(TARGET ${target} PROPERTY C_STANDARD 17)
  set_property(TARGET ${target} PROPERTY CXX_STANDARD 17)
  set_property(TARGET ${target} PROPERTY CXX_EXTENSIONS OFF)
  set_property(TARGET ${target} PROPERTY CXX_STANDARD_REQUIRED ON)
  set_property(TARGET ${target} PROPERTY VERBOSE_MAKEFILE TRUE)

  if(MSVC)
    target_compile_options(${target} PRIVATE /Zc:preprocessor $<$<CONFIG:Release>:/Oi /Ot /GL> ${CPP_WARNINGS})
  else()
    target_compile_options(${target} PRIVATE -fno-exceptions -fno-rtti $<IF:$<CONFIG:Debug>,-g,-O3 -msse -msse2 -msse3 -mmmx -m3dnow -mcx16> ${CPP_WARNINGS})
    target_compile_definitions(${target} PUBLIC $<IF:$<CONFIG:Debug>,DEBUG,NDEBUG>)
  endif()
endfunction()


add_subdirectory(innative)
add_subdirectory(innative-env)
add_subdirectory(innative-wasi)
add_subdirectory(innative-cmd)
add_subdirectory(innative-stub)
#add_subdirectory(innative-loader)
add_subdirectory(innative-assemblyscript)
add_subdirectory(innative-test-embedding)
add_subdirectory(innative-test)

set(INSTALL_BIN_DIR "${CMAKE_INSTALL_PREFIX}/bin" CACHE PATH "Installation directory for executables")
set(INSTALL_LIB_DIR "${CMAKE_INSTALL_PREFIX}/lib" CACHE PATH "Installation directory for libraries")
set(INSTALL_INC_DIR "${CMAKE_INSTALL_PREFIX}/include" CACHE PATH "Installation directory for headers")

install(TARGETS innative 
        EXPORT innativeTargets
        RUNTIME DESTINATION ${INSTALL_BIN_DIR}
        ARCHIVE DESTINATION ${INSTALL_LIB_DIR}
        LIBRARY DESTINATION ${INSTALL_LIB_DIR}  
        INCLUDES DESTINATION ${INSTALL_INC_DIR})
        
install(TARGETS innative-cmd innative-stub innative-test
        RUNTIME DESTINATION ${INSTALL_BIN_DIR}
        ARCHIVE DESTINATION ${INSTALL_LIB_DIR}
        LIBRARY DESTINATION ${INSTALL_LIB_DIR})

install(TARGETS innative-env innative-assemblyscript
        EXPORT innativeTargetLibs
        ARCHIVE DESTINATION ${INSTALL_LIB_DIR}/${TARGET_SYS}-${TARGET_ARCH}
        LIBRARY DESTINATION ${INSTALL_LIB_DIR}/${TARGET_SYS}-${TARGET_ARCH}  )

install(DIRECTORY "${CMAKE_SOURCE_DIR}/scripts" DESTINATION ${CMAKE_INSTALL_PREFIX})
install(DIRECTORY "${CMAKE_SOURCE_DIR}/spec" DESTINATION ${CMAKE_INSTALL_PREFIX})
install(DIRECTORY "${CMAKE_SOURCE_DIR}/include/innative" DESTINATION "${INSTALL_INC_DIR}")

install(EXPORT innativeTargets
        FILE innativeTargets.cmake
        NAMESPACE innative::
        DESTINATION share/innative
)

install(EXPORT innativeTargetLibs
        FILE innativeTargetLibs.cmake
        NAMESPACE innative::
        DESTINATION share/innative
)

include(CMakePackageConfigHelpers)
configure_package_config_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/innativeConfig.cmake.in"
  "${CMAKE_CURRENT_BINARY_DIR}/innativeConfig.cmake"
  INSTALL_DESTINATION "share/innative"
)
install(
  FILES "${CMAKE_CURRENT_BINARY_DIR}/innativeConfig.cmake"
  DESTINATION "share/innative"
)