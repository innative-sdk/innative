// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#pragma once

#include "innative/export.h"

// -- Wait map --
typedef struct in_wait_map in_wait_map;

// Gets the wait list associated with a specified address. If create is 1, it will
// create a wait list when none is specified; otherwise, it will return null.
in_wait_list* _innative_internal_env_wait_map_get(in_wait_map* map, void* address, int create);

// Always call this function when you are done using a wait list. The list can be reused later
// for other addresses if there are no outstanding waiters.
void _innative_internal_env_wait_map_return(in_wait_map* map, void* address, in_wait_list* list);

// -- Wait list --
typedef in_wait_list in_wait_list;

void _innative_internal_env_wait_list_enter(in_wait_list* list);
in_wait_entry* _innative_internal_env_wait_list_push(in_wait_list* list);
void _innative_internal_env_wait_list_remove(in_wait_list* list, in_wait_entry* entry);
uint32_t _innative_internal_env_wait_list_notify(in_wait_list* list, uint32_t num);
void _innative_internal_env_wait_list_exit(in_wait_list* list);

// -- Wait entry --
typedef in_wait_entry in_wait_entry;

int32_t _innative_internal_env_wait_entry_wait(in_wait_list* list, in_wait_entry* entry, int64_t timeoutns);

// Global wait map. Look into breaking this up to be more efficient later.
extern in_wait_map global_wait_map;
