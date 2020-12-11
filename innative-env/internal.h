
#include "innative/export.h"

IN_COMPILER_DLLEXPORT extern void _innative_internal_env_print(uint64_t n);
IN_COMPILER_DLLEXPORT extern void _innative_internal_write_out(const void* buf, size_t num);
IN_COMPILER_DLLEXPORT extern void _innative_internal_abort();

#ifdef IN_PLATFORM_POSIX
IN_COMPILER_DLLEXPORT extern void* _innative_syscall(size_t syscall_number, const void* p1, size_t p2, size_t p3, size_t p4,
                                                     size_t p5, size_t p6);

static const int SYSCALL_WRITE  = 1;
static const int SYSCALL_MMAP  = 9;
static const int SYSCALL_MUNMAP = 11;
static const int SYSCALL_MREMAP = 25;
static const int SYSCALL_EXIT   = 60;
static const int MREMAP_MAYMOVE = 1;
#endif