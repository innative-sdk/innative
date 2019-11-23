
#include "innative/export.h"

IN_COMPILER_DLLEXPORT extern void _innative_internal_write_out(const void* buf, size_t num);
IN_COMPILER_DLLEXPORT extern void _innative_internal_abort();

#ifdef IN_PLATFORM_POSIX
IN_COMPILER_DLLEXPORT extern void* _innative_syscall(size_t syscall_number, const void* p1, size_t p2, size_t p3, size_t p4,
                                                     size_t p5, size_t p6);
#endif