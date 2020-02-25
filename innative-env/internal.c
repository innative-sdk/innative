// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "internal.h"

#ifdef IN_PLATFORM_WIN32
  #include "../innative/win32.h"
#elif defined(IN_PLATFORM_POSIX)
  #include <unistd.h>
  #include <sys/mman.h>
#else
  #error unknown platform!
#endif

#ifdef IN_PLATFORM_WIN32
#elif defined(IN_PLATFORM_POSIX)
const int SYSCALL_WRITE  = 1;
const int SYSCALL_MMAP   = 9;
const int SYSCALL_MUNMAP = 11;
const int SYSCALL_MREMAP = 25;
const int SYSCALL_EXIT   = 60;
const int MREMAP_MAYMOVE = 1;

  #ifdef IN_CPU_x86_64
IN_COMPILER_DLLEXPORT extern IN_COMPILER_NAKED void* _innative_syscall(size_t syscall_number, const void* p1, size_t p2,
                                                                       size_t p3, size_t p4, size_t p5, size_t p6)
{
  __asm volatile("movq %rdi, %rax\n\t"
                 "movq %rsi, %rdi\n\t"
                 "movq %rdx, %rsi\n\t"
                 "movq %rcx, %rdx\n\t"
                 "movq %r8, %r10\n\t"
                 "movq %r9, %r8\n\t"
                 "movq 8(%rsp), %r9\n\t"
                 "syscall\n\t"
                 "ret");
  #elif defined(IN_CPU_x86)
__asm volatile(".cfi_startproc\n"
               "pushl %ebp\n\t"
               ".cfi_adjust_cfa_offset 4\n\t"
               ".cfi_rel_offset %ebp, 0\n\t"
               "pushl %edi\n\t"
               ".cfi_adjust_cfa_offset 4\n\t"
               ".cfi_rel_offset %edi, 0\n\t"
               "pushl %esi\n\t"
               ".cfi_adjust_cfa_offset 4\n\t"
               ".cfi_rel_offset %esi, 0\n\t"
               "pushl %ebx\n\t"
               ".cfi_adjust_cfa_offset 4\n\t"
               ".cfi_rel_offset %ebx, 0\n\t"
               "movl 44(%esp), %ebp\n\t"
               "movl 40(%esp), %edi\n\t"
               "movl 36(%esp), %esi\n\t"
               "movl 32(%esp), %edx\n\t"
               "movl 28(%esp), %ecx\n\t"
               "movl 24(%esp), %ebx\n\t"
               "movl 20(%esp), %eax\n\t"
               "int $0x80\n\t"
               "popl %ebx\n\t"
               ".cfi_adjust_cfa_offset -4\n\t"
               ".cfi_restore %ebx\n\t"
               "popl %esi\n\t"
               ".cfi_adjust_cfa_offset -4\n\t"
               ".cfi_restore %esi\n\t"
               "popl %edi\n\t"
               ".cfi_adjust_cfa_offset -4\n\t"
               ".cfi_restore %edi\n\t"
               "popl %ebp\n\t"
               ".cfi_adjust_cfa_offset -4\n\t"
               ".cfi_restore %ebp\n\t"
               "ret\n\t"
               ".cfi_endproc");
  #elif defined(IN_CPU_ARM)
__asm volatile("mov	ip, sp\n\t"
               "push{ r4, r5, r6, r7 }\n\t"
               "cfi_adjust_cfa_offset(16)\n\t"
               "cfi_rel_offset(r4, 0)\n\t"
               "cfi_rel_offset(r5, 4)\n\t"
               "cfi_rel_offset(r6, 8)\n\t"
               "cfi_rel_offset(r7, 12)\n\t"
               "mov	r7, r0\n\t"
               "mov	r0, r1\n\t"
               "mov	r1, r2\n\t"
               "mov	r2, r3\n\t"
               "ldmfd	ip, { r3, r4, r5, r6 }\n\t"
               "swi	0x0\n\t"
               "pop{ r4, r5, r6, r7 }\n\t"
               "cfi_adjust_cfa_offset(-16)\n\t"
               "cfi_restore(r4)\n\t"
               "cfi_restore(r5)\n\t"
               "cfi_restore(r6)\n\t"
               "cfi_restore(r7)\n\t"
               "cmn	r0, #4096\n\t"
               "it	cc\n\t"
    #ifdef ARCH_HAS_BX
               "bxcc lr\n\t"
    #else
               "movcc pc, lr\n\t"
    #endif
);
  #elif defined(IN_CPU_ARM64)
__asm volatile("uxtw        x8, w0\n\t"
               "mov        x0, x1\n\t"
               "mov        x1, x2\n\t"
               "mov        x2, x3\n\t"
               "mov        x3, x4\n\t"
               "mov        x4, x5\n\t"
               "mov        x5, x6\n\t"
               "mov        x6, x7\n\t"
               "svc        0x0\n\t"
               "cmn        x0, #4095\n\t"
               "RET");
  #elif defined(IN_CPU_POWERPC) || defined(IN_CPU_POWERPC64)
__asm volatile("mr   r0,r3\n\t"
               "mr   r3,r4\n\t"
               "mr   r4,r5\n\t"
               "mr   r5,r6\n\t"
               "mr   r6,r7\n\t"
               "mr   r7,r8\n\t"
               "mr   r8,r9\n\t"
               "sc\n\t"
               "bnslr+\n\t");
  #elif defined(IN_CPU_MIPS64)

    #if _MIPS_SIM == _ABI64 || _MIPS_SIM == _ABIN32
      #define SZREG 8
    #else
      #define SZREG 4
    #endif

    #if(_MIPS_SIM == _ABIO32 && _MIPS_SZPTR == 32)
      #define PTR_ADDIU addiu
    #endif
    #if _MIPS_SIM == _ABIN32
      #if !defined __mips_isa_rev || __mips_isa_rev < 6
        #define PTR_ADDIU addi /* no u */
      #else
        #define PTR_ADDIU addiu
      #endif
    #endif
    #if(_MIPS_SIM == _ABIO32 && _MIPS_SZPTR == 64 /* o64??? */) || _MIPS_SIM == _ABI64
      #define PTR_ADDIU daddiu
    #endif

    #if(SZREG == 4)
      #define REG_S sw
      #define REG_L lw
    #else
      #define REG_S sd
      #define REG_L ld
    #endif

__asm volatile(".mask 0x00010000, -SZREG"
               ".fmask 0x00000000, 0"
               "PTR_ADDIU sp, -SZREG"
               "cfi_adjust_cfa_offset (SZREG)"
               "REG_S s0, (sp)"
               "cfi_rel_offset (s0, 0)"
               "move s0, a0"
               "move a0, a1"
               "move a1, a2"
               "move a2, a3"
               "move a3, a4"
               "move a4, a5"
               "move a5, a6"
               "move v0, s0"
               "syscall"
               "REG_L s0, (sp)"
               "cfi_restore (s0)"
               "PTR_ADDIU sp, SZREG"
               "cfi_adjust_cfa_offset (-SZREG)"
               "ret");
  #else
    #error unsupported architecture!
  #endif
}
#endif

IN_COMPILER_DLLEXPORT extern void _innative_internal_abort()
{
#ifdef IN_COMPILER_MSC
  #if defined(IN_CPU_x86_64) || defined(IN_CPU_x86)
  __ud2();
  #elif defined(IN_CPU_ARM)
  __trap(-1);
  #elif defined(IN_CPU_ARM64)
  __break(-1);
  #endif
  __fastfail(FAST_FAIL_FATAL_APP_EXIT);
#else
  __builtin_trap();
#endif
}

// Writes a buffer to the standard output using system calls
IN_COMPILER_DLLEXPORT extern void _innative_internal_write_out(const void* buf, size_t num)
{
#ifdef IN_PLATFORM_WIN32
  DWORD out;
  WriteConsoleA(GetStdHandle(STD_OUTPUT_HANDLE), buf, (DWORD)num, &out, NULL);
#elif defined(IN_PLATFORM_POSIX)
  size_t cast = 1;
  _innative_syscall(SYSCALL_WRITE, (void*)cast, (size_t)buf, num, 0, 0, 0);
#else
  #error unknown platform!
#endif
}

static const char lookup[16] = "0123456789ABCDEF";

IN_COMPILER_DLLEXPORT extern void _innative_internal_env_print(uint64_t n)
{
  int i = 0;
  do
  {
    // This is inefficient, but avoids using the stack (since i gets optimized out), which can segfault if other functions
    // are misbehaving.
    _innative_internal_write_out(&lookup[(n >> 60) & 0xF], 1);
    i++;
    n <<= 4;
  } while(i < 16);
  _innative_internal_write_out("\n", 1);
}

IN_COMPILER_DLLEXPORT extern void _innative_internal_env_memdump(const unsigned char* mem, uint64_t sz)
{
  static const char prefix[] = "\n --- MEMORY DUMP ---\n\n";
  char buf[256];

  _innative_internal_write_out(prefix, sizeof(prefix));
  for(uint64_t i = 0; i < sz;)
  {
    uint64_t j;
    for(j = 0; j < (sizeof(buf) / 2) && i < sz; ++j, ++i)
    {
      buf[j * 2]     = lookup[(mem[i] & 0xF0) >> 4];
      buf[j * 2 + 1] = lookup[mem[i] & 0x0F];
    }
    _innative_internal_write_out(buf, (size_t)j * 2);
  }
  _innative_internal_write_out("\n", 1);
}

// Very simple memcpy implementation because we don't have access to the C library
IN_COMPILER_DLLEXPORT extern void _innative_internal_env_memcpy(char* dest, const char* src, uint64_t sz)
{
  // Align dest pointer
  while((size_t)dest % sizeof(uint64_t) && sz)
  {
    *dest = *src;
    dest += 1;
    src += 1;
    sz -= 1;
  }

  while(sz > sizeof(uint64_t))
  {
    *((uint64_t*)dest) = *((uint64_t*)src);
    dest += sizeof(uint64_t);
    src += sizeof(uint64_t);
    sz -= sizeof(uint64_t);
  }

  while(sz)
  {
    *dest = *src;
    dest += 1;
    src += 1;
    sz -= 1;
  }
}

// Platform-specific memory free, called by the exit function to clean up memory allocations
IN_COMPILER_DLLEXPORT extern void _innative_internal_env_free_memory(void* p, uint64_t size)
{
  if(p && size > 0)
  {
    char* start = (char*)p;

#ifdef IN_PLATFORM_WIN32
    char* end = start + size;

    // Resized allocations can potentially have been allocated in multiple chunks, and we must ensure we free them properly
    while(start < end)
    {
      MEMORY_BASIC_INFORMATION meminfo;
      VirtualQuery(start, &meminfo, sizeof(MEMORY_BASIC_INFORMATION));
      VirtualFree(meminfo.AllocationBase, 0, MEM_RELEASE);
      start = (char*)meminfo.BaseAddress + meminfo.RegionSize;
    }
#elif defined(IN_PLATFORM_POSIX)
    _innative_syscall(SYSCALL_MUNMAP, start, size, 0, 0, 0, 0);
#else
  #error unknown platform!
#endif
  }
}

// Platform-specific implementation of the mem.grow instruction, except it works in bytes
IN_COMPILER_DLLEXPORT extern void* _innative_internal_env_grow_memory(void* p, uint64_t i, uint64_t max, uint64_t* size)
{
  if(!size)
    return 0;
  if(!i)
    return !p ? (void*)~0 : p; // We must return a non-zero pointer even if it's a zero-length allocation.
  if(i + *size > 0xFFFFFFFF)   // Invalid for wasm32
    return 0;

  char* info = (char*)p;
  if(info != 0 && *size > 0)
  {
    if(max > 0 && (i + *size) > max)
      return 0;
#ifdef IN_PLATFORM_WIN32
    if(max > 0) // If a maximum was specified, the memory should've been reserved already
    {
      // It's fine if we aren't aligned on page bounderies because the function won't fail on already committed pages.
      if(!VirtualAlloc(info + *size, (size_t)i, MEM_COMMIT, PAGE_READWRITE))
        return 0;
      i += *size;
    }
    else
    {
      // Align size to the page boundary (we can't do this via VirtualQuery)
      SYSTEM_INFO sysinfo;
      GetSystemInfo(&sysinfo);
      size_t sz = *size % sysinfo.dwPageSize;
      sz        = (size_t)(!sz ? *size : *size + sysinfo.dwPageSize - sz);
      i -= sz - *size; // Modify i by the difference

      if(!VirtualAlloc(info + sz, (size_t)i, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE))
      {
        // If this fails, we ran into someone else's memory, so we need to move the entire allocation.
        void* mem = VirtualAlloc(0, (size_t)(i + sz), MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
        if(!mem)
          return 0;
        _innative_internal_env_memcpy(mem, info, *size);
        _innative_internal_env_free_memory(info, *size);
        info = mem;
      }

      i += sz;
    }
#elif defined(IN_PLATFORM_POSIX)
    i += *size;
    info = _innative_syscall(SYSCALL_MREMAP, info, *size, i, MREMAP_MAYMOVE, 0, 0);
    if((void*)info >= (void*)0xfffffffffffff001) // This is a syscall error from -4095 to -1
      return 0;
#else
  #error unknown platform!
#endif
  }
  else
  {
    if(max > 0 && i > max)
      return 0;
#ifdef IN_PLATFORM_WIN32
    info = VirtualAlloc(0, (size_t)(!max ? i : max), MEM_RESERVE, PAGE_READWRITE);
    if(!info)
      return 0;
    if(!VirtualAlloc(info, (size_t)i, MEM_COMMIT, PAGE_READWRITE))
      return 0;

#elif defined(IN_PLATFORM_POSIX)
    info = _innative_syscall(SYSCALL_MMAP, NULL, i, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if((void*)info >= (void*)0xfffffffffffff001) // This is a syscall error from -4095 to -1
      return 0;
#else
  #error unknown platform!
#endif
  }

  if((size_t)info % sizeof(uint64_t))
    _innative_internal_abort();

  if(!info)
    return 0;

  *size = i;
  return info;
}

// You cannot return from the entry point of a program, you must instead call a platform-specific syscall to terminate it.
IN_COMPILER_DLLEXPORT extern void _innative_internal_env_exit(int status)
{
#ifdef IN_PLATFORM_WIN32
  ExitProcess(status);
#elif defined(IN_PLATFORM_POSIX)
  size_t cast = status;
  _innative_syscall(SYSCALL_EXIT, (void*)cast, 0, 0, 0, 0, 0);
#endif
}

// This function exists only to test the _WASM_ C export code path
IN_COMPILER_DLLEXPORT extern void _innative_internal_WASM_print(int32_t a) { _innative_internal_env_print(a); }
