/* inNative Runtime Compiler
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

#ifndef __INNATIVE_H__IR__
#define __INNATIVE_H__IR__

#define INNATIVE_VERSION_MAJOR 0
#define INNATIVE_VERSION_MINOR 1
#define INNATIVE_VERSION_REVISION 0

// CPU Architecture (possible pre-defined macros found on http://predef.sourceforge.net/prearch.html)
#if defined(_M_X64) || defined(__amd64__) || defined(__amd64) || defined(_AMD64_) || defined(__x86_64__) || defined(__x86_64) || defined(_LP64)
#define IR_CPU_x86_64  //x86-64 architecture
#define IR_64BIT
#elif defined(__ia64__) || defined(_IA64) || defined(__IA64__) || defined(__ia64) || defined(_M_IA64)
#define IR_CPU_IA_64 //Itanium (IA-64) architecture
#define IR_64BIT
#elif defined(_M_IX86) || defined(__i386) || defined(__i386__) || defined(__X86__) || defined(_X86_) || defined(__I86__) || defined(__THW_INTEL__) || defined(__INTEL__)
#define IR_CPU_x86  //x86 architecture
#define IR_32BIT
#elif defined(__arm__) || defined(__thumb__) || defined(__TARGET_ARCH_ARM) || defined(__TARGET_ARCH_THUMB) || defined(_ARM)
#define IR_CPU_ARM //ARM architecture
#define IR_32BIT
#elif defined(__mips__) || defined(mips) || defined(_MIPS_ISA) || defined(__mips) || defined(__MIPS__)
#define IR_CPU_MIPS
#define IR_64BIT
#elif defined(__powerpc) || defined(__powerpc__) || defined(__POWERPC__) || defined(__ppc__) || defined(_M_PPC) || defined(_ARCH_PPC)
#define IR_CPU_POWERPC
#define IR_32BIT
#else
#define IR_CPU_UNKNOWN //Unknown CPU architecture (should force architecture independent C implementations)
#endif

// Compiler detection and macro generation
#if defined(__clang__) // Clang (must be before GCC, because clang also pretends it's GCC)
#define IR_COMPILER_CLANG
#define IR_COMPILER_DLLEXPORT __attribute__((dllexport))
#define IR_COMPILER_DLLIMPORT __attribute__((dllimport))
#define IR_COMPILER_FASTCALL __attribute__((fastcall))
#define IR_COMPILER_NAKED __attribute__((naked))
#define IR_FORCEINLINE __attribute__((always_inline)) inline
#define IR_RESTRICT __restrict__
#define IR_ALIGN(n) __attribute__((aligned(n)))
#define IR_ALIGNED(sn, n) sn IR_ALIGN(n)
#elif defined __GNUC__ // GCC
#define IR_COMPILER_GCC
#define IR_COMPILER_DLLEXPORT __attribute__((dllexport))
#define IR_COMPILER_DLLIMPORT __attribute__((dllimport))
#define IR_COMPILER_FASTCALL __attribute__((fastcall))
#define IR_COMPILER_NAKED __attribute__((naked))
#define IR_FORCEINLINE __attribute__((always_inline)) inline
#define IR_RESTRICT __restrict__
#define IR_ALIGN(n) __attribute__((aligned(n)))
#define IR_ALIGNED(sn, n) sn IR_ALIGN(n)
#elif defined _MSC_VER // VC++
#define IR_COMPILER_MSC
#define IR_COMPILER_DLLEXPORT __declspec(dllexport)
#define IR_COMPILER_DLLIMPORT __declspec(dllimport)
#define IR_COMPILER_FASTCALL __fastcall
#define IR_FORCEINLINE __forceinline
#define IR_RESTRICT __restrict
#define IR_ALIGN(n) __declspec(align(n))
#define IR_ALIGNED(sn, n) IR_ALIGN(n) sn
#define IR_SSE_ENABLED
#define IR_ASSUME(x) __assume(x)
#define _HAS_EXCEPTIONS 0
#endif

// Platform detection
#if defined(WIN32) || defined(_WIN32) || defined(_WIN64) || defined(__TOS_WIN__) || defined(__WINDOWS__)
#define IR_PLATFORM_WIN32
#elif defined(_POSIX_VERSION) || defined(_XOPEN_VERSION) || defined(unix) || defined(__unix__) || defined(__unix)
#define IR_PLATFORM_POSIX
#endif

#ifdef _WIN32_WCE
#define IR_PLATFORM_WIN32_CE // Implies WIN32
#elif defined(__APPLE__) || defined(__MACH__) || defined(macintosh) || defined(Macintosh)
#define IR_PLATFORM_APPLE // Should also define POSIX, use only for Apple OS specific features
#elif defined(__CYGWIN__)
#define IR_PLATFORM_CYGWIN // Should also define POSIX, use only to deal with Cygwin weirdness
#elif defined(__ANDROID__) || defined(__ANDROID_API__) 
#define IR_PLATFORM_ANDROID // Should also define POSIX, use for Android specific features.
#elif defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__) || defined(__bsdi__) || defined(__DragonFly__) || defined(BSD) // Also defines POSIX
#define IR_PLATFORM_BSD // Should also define POSIX
#elif defined(sun) || defined(__sun) 
# define IR_PLATFORM_SOLARIS
# if !defined(__SVR4) && !defined(__svr4__)
#   define IR_PLATFORM_SUNOS
# endif
#endif

#if defined(__linux__) || defined(__linux)
#define IR_PLATFORM_LINUX // Should also define POSIX, use only for linux specific features
#endif

#if !(defined(IR_PLATFORM_WIN32) || defined(IR_PLATFORM_POSIX) || defined(IR_PLATFORM_WIN32_CE) || defined(IR_PLATFORM_APPLE))
#error "Unknown Platform"
#endif

// Endianness detection
#if defined(IR_PLATFORM_WIN32) || defined(IR_PLATFORM_WIN32_CE) || defined(IR_CPU_x86_64) || defined(IR_CPU_x86) || defined(IR_CPU_IA_64) // Windows, x86, x86_64 and itanium all only run in little-endian (except on HP-UX but we don't support that)
# define IR_ENDIAN_LITTLE
#elif defined(IR_CPU_ARM)
# ifdef IR_PLATFORM_LINUX
#   define IR_ENDIAN_LITTLE
# endif
#elif defined(IR_CPU_POWERPC)
# ifdef IR_PLATFORM_SOLARIS
#   define IR_ENDIAN_LITTLE
# elif defined(IR_PLATFORM_APPLE) || defined(IR_PLATFORM_BSD) || defined(IR_PLATFORM_LINUX)
#   define IR_ENDIAN_BIG
# endif
#elif defined(IR_CPU_MIPS) // MIPS is a bitch to detect endianness on
# ifdef IR_PLATFORM_LINUX
#   define IR_ENDIAN_BIG
# endif
#endif

#if !defined(IR_ENDIAN_LITTLE) && !defined(IR_ENDIAN_BIG)
#error "Unknown Endianness"
#endif

// Debug detection
#ifdef IR_COMPILER_GCC
#ifndef NDEBUG
#define IR_DEBUG
#endif
#else
#if defined(DEBUG) || defined(_DEBUG)
#define IR_DEBUG
#endif
#endif

#ifdef IR_COMPILER_MSC
#define FOPEN(f, path, mode) fopen_s((&f), (path), (mode))
#define STRICMP(a, b) _stricmp(a, b)
#define STRTOK(str,delim,context) strtok_s(str,delim,context)
#else
#define FOPEN(f, path, mode) f = fopen(path, mode)
#define STRICMP(a, b) strcasecmp(a, b)
#define STRTOK(str,delim,context) strtok_r(str,delim,context)
#endif

#endif
