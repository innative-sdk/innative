// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#pragma once

#include "innative/export.h"

IN_COMPILER_DLLEXPORT extern int32_t _innative_internal_env_atomic_wait32(void* address, int32_t expected, int64_t timeoutns);
IN_COMPILER_DLLEXPORT extern int32_t _innative_internal_env_atomic_wait64(void* address, int64_t expected, int64_t timeoutns);
IN_COMPILER_DLLEXPORT extern uint32_t _innative_internal_env_atomic_notify(void* address, uint32_t count);

// Atomic helpers
int32_t _innative_internal_env_atomic_load32(int32_t* address);
int64_t _innative_internal_env_atomic_load64(int64_t* address);
