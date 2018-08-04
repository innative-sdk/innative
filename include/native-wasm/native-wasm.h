/* Native-WASM Runtime Compiler
Copyright ©2018 Black Sphere Studios

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

#ifndef __NATIVE_WASM_H__NW__
#define __NATIVE_WASM_H__NW__

#define NATIVE_WASM_VERSION_MAJOR 0
#define NATIVE_WASM_VERSION_MINOR 1
#define NATIVE_WASM_VERSION_REVISION 0

// CPU Architecture (possible pre-defined macros found on http://predef.sourceforge.net/prearch.html)
#if defined(_M_X64) || defined(__amd64__) || defined(__amd64) || defined(_AMD64_) || defined(__x86_64__) || defined(__x86_64) || defined(_LP64)
#define NW_CPU_x86_64  //x86-64 architecture
#define NW_64BIT
#elif defined(__ia64__) || defined(_IA64) || defined(__IA64__) || defined(__ia64) || defined(_M_IA64)
#define NW_CPU_IA_64 //Itanium (IA-64) architecture
#define NW_64BIT
#elif defined(_M_IX86) || defined(__i386) || defined(__i386__) || defined(__X86__) || defined(_X86_) || defined(__I86__) || defined(__THW_INTEL__) || defined(__INTEL__)
#define NW_CPU_x86  //x86 architecture
#define NW_32BIT
#elif defined(__arm__) || defined(__thumb__) || defined(__TARGET_ARCH_ARM) || defined(__TARGET_ARCH_THUMB) || defined(_ARM)
#define NW_CPU_ARM //ARM architecture
#define NW_32BIT
#elif defined(__mips__) || defined(mips) || defined(_MIPS_ISA) || defined(__mips) || defined(__MIPS__)
#define NW_CPU_MIPS
#define NW_64BIT
#elif defined(__powerpc) || defined(__powerpc__) || defined(__POWERPC__) || defined(__ppc__) || defined(_M_PPC) || defined(_ARCH_PPC)
#define NW_CPU_POWERPC
#define NW_32BIT
#else
#define NW_CPU_UNKNOWN //Unknown CPU architecture (should force architecture independent C implementations)
#endif

// Compiler detection and macro generation
#if defined(__clang__) // Clang (must be before GCC, because clang also pretends it's GCC)
#define NW_COMPILER_CLANG
#define NW_COMPILER_DLLEXPORT __attribute__((dllexport))
#define NW_COMPILER_DLLIMPORT __attribute__((dllimport))
#define NW_COMPILER_FASTCALL __attribute__((fastcall))
#define NW_FORCEINLINE __attribute__((always_inline)) inline
#define NW_RESTRICT __restrict__
#define NW_ALIGN(n) __attribute__((aligned(n)))
#define NW_ALIGNED(sn, n) sn NW_ALIGN(n)
#elif defined __GNUC__ // GCC
#define NW_COMPILER_GCC
#define NW_COMPILER_DLLEXPORT __attribute__((dllexport))
#define NW_COMPILER_DLLIMPORT __attribute__((dllimport))
#define NW_COMPILER_FASTCALL __attribute__((fastcall))
#define NW_FORCEINLINE __attribute__((always_inline)) inline
#define NW_RESTRICT __restrict__
#define NW_ALIGN(n) __attribute__((aligned(n)))
#define NW_ALIGNED(sn, n) sn NW_ALIGN(n)
#elif defined _MSC_VER // VC++
#define NW_COMPILER_MSC
#define NW_COMPILER_DLLEXPORT __declspec(dllexport)
#define NW_COMPILER_DLLIMPORT __declspec(dllimport)
#define NW_COMPILER_FASTCALL __fastcall
#define NW_FORCEINLINE __forceinline
#define NW_RESTRICT __restrict
#define NW_ALIGN(n) __declspec(align(n))
#define NW_ALIGNED(sn, n) NW_ALIGN(n) sn
#define NW_SSE_ENABLED
#define NW_ASSUME(x) __assume(x)
#define _HAS_EXCEPTIONS 0
#endif

// Platform detection
#if defined(WIN32) || defined(_WIN32) || defined(_WIN64) || defined(__TOS_WIN__) || defined(__WINDOWS__)
#define NW_PLATFORM_WIN32
#elif defined(_POSIX_VERSION) || defined(_XOPEN_VERSION) || defined(unix) || defined(__unix__) || defined(__unix)
#define NW_PLATFORM_POSIX
#endif

#ifdef _WIN32_WCE
#define NW_PLATFORM_WIN32_CE // Implies WIN32
#elif defined(__APPLE__) || defined(__MACH__) || defined(macintosh) || defined(Macintosh)
#define NW_PLATFORM_APPLE // Should also define POSIX, use only for Apple OS specific features
#elif defined(__CYGWIN__)
#define NW_PLATFORM_CYGWIN // Should also define POSIX, use only to deal with Cygwin weirdness
#elif defined(__ANDROID__) || defined(__ANDROID_API__) 
#define NW_PLATFORM_ANDROID // Should also define POSIX, use for Android specific features.
#elif defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__) || defined(__bsdi__) || defined(__DragonFly__) || defined(BSD) // Also defines POSIX
#define NW_PLATFORM_BSD // Should also define POSIX
#elif defined(sun) || defined(__sun) 
# define NW_PLATFORM_SOLARIS
# if !defined(__SVR4) && !defined(__svr4__)
#   define NW_PLATFORM_SUNOS
# endif
#endif

#if defined(__linux__) || defined(__linux)
#define NW_PLATFORM_LINUX // Should also define POSIX, use only for linux specific features
#endif

#if !(defined(NW_PLATFORM_WIN32) || defined(NW_PLATFORM_POSIX) || defined(NW_PLATFORM_WIN32_CE) || defined(NW_PLATFORM_APPLE))
#error "Unknown Platform"
#endif

// Endianness detection
#if defined(NW_PLATFORM_WIN32) || defined(NW_PLATFORM_WIN32_CE) || defined(NW_CPU_x86_64) || defined(NW_CPU_x86) || defined(NW_CPU_IA_64) // Windows, x86, x86_64 and itanium all only run in little-endian (except on HP-UX but we don't support that)
# define NW_ENDIAN_LITTLE
#elif defined(NW_CPU_ARM)
# ifdef NW_PLATFORM_LINUX
#   define NW_ENDIAN_LITTLE
# endif
#elif defined(NW_CPU_POWERPC)
# ifdef NW_PLATFORM_SOLARIS
#   define NW_ENDIAN_LITTLE
# elif defined(NW_PLATFORM_APPLE) || defined(NW_PLATFORM_BSD) || defined(NW_PLATFORM_LINUX)
#   define NW_ENDIAN_BIG
# endif
#elif defined(NW_CPU_MIPS) // MIPS is a bitch to detect endianness on
# ifdef NW_PLATFORM_LINUX
#   define NW_ENDIAN_BIG
# endif
#endif

#if !defined(NW_ENDIAN_LITTLE) && !defined(NW_ENDIAN_BIG)
#error "Unknown Endianness"
#endif

// Debug detection
#ifdef NW_COMPILER_GCC
#ifndef NDEBUG
#define NW_DEBUG
#endif
#else
#if defined(DEBUG) || defined(_DEBUG)
#define NW_DEBUG
#endif
#endif


#ifdef NW_COMPILER_MSC
#define FOPEN(f, path, mode) fopen_s((&f), (path), (mode))
#define ITOA(value, buffer, size, radix) _itoa_s(value, buffer, size, radix)
#define STRICMP(a, b) _stricmp(a, b)
#else
#define FOPEN(f, path, mode) f = fopen(path, mode)
#define ITOA(value, buffer, size, radix) itoa((value), (buffer), (radix))
#define STRICMP(a, b) stricmp(a, b)
#endif

#endif
