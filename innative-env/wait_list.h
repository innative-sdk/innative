// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__ENV_WAIT_LIST_H
#define IN__ENV_WAIT_LIST_H

#include "innative/export.h"

#ifdef IN_PLATFORM_WIN32
  #include "../innative/win32.h"
#elif defined(IN_PLATFORM_POSIX)
  #include <pthread.h>
  #include <errno.h>
#else
  #error unknown platform!
#endif

#ifdef __cplusplus
extern "C" {
#endif

typedef struct in_wait_map in_wait_map;
typedef struct in_wait_list in_wait_list;
typedef struct in_wait_entry in_wait_entry;

// -- Wait map --

// Gets the wait list associated with a specified address. If create is 1, it will
// create a wait list when none is specified; otherwise, it will return null.
in_wait_list* _innative_internal_env_wait_map_get(in_wait_map* map, void* address, int create);

// Always call this function when you are done using a wait list. The list can be reused later
// for other addresses if there are no outstanding waiters.
void _innative_internal_env_wait_map_return(in_wait_map* map, void* address, in_wait_list* list);

// Clean up any extra memory from this map
void _innative_internal_env_wait_map_cleanup(in_wait_map* map);

// -- Wait list --

void _innative_internal_env_wait_list_enter(in_wait_list* list);
void _innative_internal_env_wait_list_exit(in_wait_list* list);
in_wait_entry* _innative_internal_env_wait_list_push(in_wait_list* list);
void _innative_internal_env_wait_list_remove(in_wait_list* list, in_wait_entry* entry);
uint32_t _innative_internal_env_wait_list_notify(in_wait_list* list, uint32_t num);
void _innative_internal_env_wait_list_shrink(in_wait_list* list);

// -- Wait entry --

int32_t _innative_internal_env_wait_entry_wait(in_wait_list* list, in_wait_entry* entry, int64_t timeoutns);

// Global wait map. Look into breaking this up to be more efficient later.
extern in_wait_map _innative_internal_env_global_wait_map;

#ifdef IN_PLATFORM_WIN32
typedef SRWLOCK in_platform_mutex;
typedef SRWLOCK in_platform_rwlock;
typedef CONDITION_VARIABLE in_platform_condvar;
#elif defined(IN_PLATFORM_POSIX)
typedef pthread_mutex_t in_platform_mutex;
typedef pthread_rwlock_t in_platform_rwlock;
typedef pthread_cond_t in_platform_condvar;
#endif

struct in_wait_entry
{
  in_platform_condvar condvar;
  union
  {
    int32_t signaled;
    in_wait_entry* next_free_node;
  };
};

struct in_wait_list
{
  in_platform_mutex lock;

  in_wait_entry** entries;
  size_t cap;
  size_t len;

  size_t outstanding_signals;
  size_t refs;

  // Reuse the nodes
  in_wait_entry* free_list;
  in_wait_list* next_free_list;
};

struct in_wait_map
{
  in_platform_rwlock lock;
  size_t len, cap;
  struct in_wait_map_entry* entries;

  in_wait_list* free_lists;
  size_t free_count;
};

struct in_wait_map_entry
{
  uint64_t hash;
  void* key;
  in_wait_list* value;
};

#ifdef __cplusplus
}
#endif

#endif
