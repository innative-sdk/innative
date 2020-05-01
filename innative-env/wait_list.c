// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "wait_list.h"
#include "internal.h"

#ifdef IN_PLATFORM_WIN32
  #include "../innative/win32.h"
#elif defined(IN_PLATFORM_POSIX)
// TODO: POSIX
#else
  #error unknown platform!
#endif

#ifdef IN_PLATFORM_WIN32
typedef SRWLOCK in_platform_mutex;
typedef CONDITION_VARIABLE in_platform_condvar;
#elif defined(IN_PLATFORM_POSIX)
typedef pthread_mutex_t in_platform_mutex;
// TODO
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
  union
  {
    size_t len;
    in_wait_list* next_free_list;
  };

  size_t outstanding_signals;

  // Reuse the nodes
  in_wait_entry* free_list;
};

struct in_wait_map
{
  in_platform_mutex lock;
  size_t len, cap;
  struct in_wait_map_entry* entries;

  in_wait_list* free_lists;
};

struct in_wait_map_entry
{
  uint64_t hash;
  void* key;
  in_wait_list* value;
};

static uint64_t in_addr_hash(void* address);

static void in_platform_mutex_init(in_platform_mutex* mutex);
static void in_platform_mutex_lock(in_platform_mutex* mutex);
static void in_platform_mutex_unlock(in_platform_mutex* mutex);
static void in_platform_mutex_free(in_platform_mutex* mutex);

static void in_platform_condvar_init(in_platform_condvar* condvar);
static int in_platform_condvar_wait(in_platform_condvar* condvar, in_platform_mutex* mutex, int64_t timeoutns);
static void in_platform_condvar_notify(in_platform_condvar* condvar);
static void in_platform_condvar_free(in_platform_condvar* condvar);

// Memory stuff
static void* grow_array(void* array, size_t new_size);
static void free_array(void* array);

// Wait map
#define TOMB_MASK                      (1ULL << 63)
#define DESIRED_POS(map, hash)         (((hash) & ((~0ULL) ^ TOMB_MASK)) % (map)->cap)
#define PROBE_DISTANCE(map, hash, pos) ((pos + (map)->cap - DESIRED_POS((map), (hash))) % (map)->cap)
#define IS_DELETED(hash)               (((hash)&TOMB_MASK) == TOMB_MASK)
#define IS_ALIVE(hash)                 ((hash) != 0 && !IS_DELETED(hash))

static size_t in_wait_map_lookup(in_wait_map* map, void* key)
{
  if(map->len == 0)
  {
    return 0;
  }

  uint64_t hash = in_addr_hash(key);
  size_t pos    = DESIRED_POS(map, hash);
  size_t dist   = 0;

  for(;;)
  {
    struct in_wait_map_entry* entry = &map->entries[pos];

    if(entry->hash == 0 || dist > PROBE_DISTANCE(map, entry->hash, pos))
    {
      return map->cap;
    }
    else if(entry->hash == hash && entry->key == key)
    {
      return pos;
    }
    else
    {
      pos = (pos + 1) % map->cap;
      dist++;
    }
  }
}

static size_t in_wait_map_insert_helper(in_wait_map* map, void* key, in_wait_list* list)
{
  struct in_wait_map_entry* data = map->entries;
  uint64_t hash                  = in_addr_hash(key);
  size_t pos                     = DESIRED_POS(map, hash);
  size_t dist                    = 0;
  struct in_wait_map_entry entry = { hash, key, list };

  for(;;)
  {
    struct in_wait_map_entry curr = data[pos];

    if(curr.hash == 0)
    {
      data[pos] = entry;
      return pos;
    }

    size_t curr_probe_dist = PROBE_DISTANCE(map, curr.hash, pos);
    if(curr_probe_dist < dist)
    {
      if(IS_DELETED(curr.hash))
      {
        data[pos] = entry;
        return pos;
      }

      data[pos] = entry;
      entry     = curr;
      dist      = curr_probe_dist;
    }

    pos = (pos + 1) % map->cap;
    dist++;
  }
}

static void in_wait_map_grow(in_wait_map* map)
{
  struct in_wait_map_entry* old = map->entries;
  size_t old_cap                = map->cap;
  map->cap                      = max(map->cap * 2, 32);
  map->entries                  = grow_array(NULL, map->cap * sizeof(*old)); // Unfortunately it can't be reused

  for(size_t i = 0; i < old_cap; ++i)
  {
    if(IS_ALIVE(old[i].hash))
    {
      in_wait_map_insert_helper(map, old[i].key, old[i].value);
    }
  }

  free_array(old);
}

static size_t in_wait_map_insert(in_wait_map* map, void* key)
{
  // 95% storage limit to keep it fast
  if(map->len >= map->cap * 0.95)
  {
    in_wait_map_grow(map);
  }

  in_wait_list* list;
  if(map->free_lists)
  {
    list            = map->free_lists;
    map->free_lists = list->next_free_list;
    list->len       = 0;
  }
  else
  {
    list = grow_array(NULL, sizeof(*list));
    in_platform_mutex_init(&list->lock);
  }

  map->len++;
  return in_wait_map_insert_helper(map, key, list);
}

static in_wait_list* in_wait_map_get_inner(in_wait_map* map, void* address, int create)
{
  size_t idx = in_wait_map_lookup(map, address);
  if(idx == map->cap)
  {
    if(create)
      idx = in_wait_map_insert(map, address);
    else
      return 0;
  }

  return map->entries[idx].value;
}

in_wait_list* _innative_internal_env_wait_map_get(in_wait_map* map, void* address, int create)
{
  in_platform_mutex_lock(&map->lock);
  in_wait_list* result = in_wait_map_get_inner(map, address, create);
  in_platform_mutex_unlock(&map->lock);
  return result;
}

void _innative_internal_env_wait_map_return(in_wait_map* map, void* address, in_wait_list* list)
{
  in_platform_mutex_lock(&map->lock);

  // Put this entry on the free list if it's no longer being used
  if(list->len == 0 && list->outstanding_signals == 0)
  {
    size_t idx = in_wait_map_lookup(map, address);
    map->entries[idx].hash |= TOMB_MASK;
    map->len--;

    list->next_free_list = map->free_lists;
    map->free_lists      = list;
  }

  in_platform_mutex_unlock(&map->lock);
}

// Wait list
void _innative_internal_env_wait_list_enter(in_wait_list* list) { in_platform_mutex_lock(&list->lock); }

in_wait_entry* _innative_internal_env_wait_list_push(in_wait_list* list)
{
  if(list->len == list->cap)
  {
    list->cap     = max(list->cap * 2, 1);
    list->entries = grow_array(list->entries, list->cap * sizeof(void*));
  }

  in_wait_entry* entry;
  if(list->free_list)
  {
    entry           = list->free_list;
    list->free_list = entry->next_free_node;
    entry->signaled = 0;
  }
  else
  {
    entry = grow_array(NULL, sizeof(*entry));
    in_platform_condvar_init(&entry->condvar);
  }

  list->entries[list->len++] = entry;

  return entry;
}

void _innative_internal_env_wait_list_remove(in_wait_list* list, in_wait_entry* entry)
{
  if(entry->signaled)
  {
    list->outstanding_signals--;
    entry->next_free_node = list->free_list;
    list->free_list       = entry;
  }
  else
  {
    int found = 0;

    if(list->entries[list->len - 1] == entry)
      found = 1;
    else
    {
      for(size_t i = 0; i < list->len - 1; ++i)
      {
        if(found || (found = list->entries[i] == entry))
        {
          // shift the whole list once we've found it
          list->entries[i] = list->entries[i + 1];
        }
      }
    }

    if(found)
    {
      list->len--;
      entry->next_free_node = list->free_list;
      list->free_list       = entry;
    }
  }
}

uint32_t _innative_internal_env_wait_list_notify(in_wait_list* list, uint32_t num)
{
  size_t count = min(num, list->len);
  for(size_t i = 0; i < count; ++i)
  {
    // Doesn't need to be atomic because of the mutex
    list->entries[i]->signaled = 1;
    list->outstanding_signals++;
    in_platform_condvar_notify(&list->entries[i]->condvar);
  }
  size_t remainder = list->len - count;
  for(size_t i = 0; i < remainder; ++i)
  {
    list->entries[i] = list->entries[count + i];
  }
  list->len = remainder;
  return (uint32_t)count; // can't be bigger than `num` so cast is fine
}

void _innative_internal_env_wait_list_exit(in_wait_list* list) { in_platform_mutex_unlock(&list->lock); }

// Wait entry
int32_t _innative_internal_env_wait_entry_wait(in_wait_list* list, in_wait_entry* entry, int64_t timeoutns)
{
  while(entry->signaled == 0)
  {
    if(in_platform_condvar_wait(&entry->condvar, &list->lock, timeoutns))
    {
      return 2; // timeout
    }
  }
  return 0;
}

// Basic fnv1a hash
static uint64_t in_addr_hash(void* address)
{
  uint64_t value = (uint64_t)address;
  uint64_t hash  = 14695981039346656037ULL;
  for(int i = 0; i < 8; i++)
  {
    hash = hash ^ (value & 0xFF);
    hash = hash * 1099511628211ULL;
    value >>= 8;
  }
  return max(hash, 1); // 0 is reserved. I feel very sorry for the value that has a hash of 0 anyways lmao
}

#ifdef IN_PLATFORM_WIN32

in_wait_map global_wait_map = { SRWLOCK_INIT };

static void in_platform_mutex_init(in_platform_mutex* mutex) { InitializeSRWLock(mutex); }
static void in_platform_mutex_lock(in_platform_mutex* mutex) { AcquireSRWLockExclusive(mutex); }
static void in_platform_mutex_unlock(in_platform_mutex* mutex) { ReleaseSRWLockExclusive(mutex); }
static void in_platform_mutex_free(in_platform_mutex* mutex) {}

static void in_platform_condvar_init(in_platform_condvar* condvar) { InitializeConditionVariable(condvar); }

static int in_platform_condvar_wait(in_platform_condvar* condvar, in_platform_mutex* mutex, int64_t timeoutns)
{
  DWORD timeout;
  if(timeoutns < 0)
    timeout = INFINITE;
  else
    timeout = min((DWORD)(timeoutns / 1'000'000), INFINITE - 1);

  BOOL result = SleepConditionVariableSRW(condvar, mutex, timeout, 0);
  return !result;
}

static void in_platform_condvar_notify(in_platform_condvar* condvar) { WakeConditionVariable(condvar); }
static void in_platform_condvar_free(in_platform_condvar* condvar) {}

// Memory stuff
static void* grow_array(void* array, size_t new_size)
{
  if(array == 0)
  {
    return HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, new_size);
  }
  else
  {
    return HeapReAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, array, new_size);
  }
}

static void free_array(void* array) { HeapFree(GetProcessHeap(), 0, array); }

#elif defined(IN_PLATFORM_POSIX)

in_wait_map global_wait_map = { PTHREAD_MUTEX_INITIALIZER };

// TODO
static void in_platform_mutex_init(in_platform_mutex* mutex) {}
static void in_platform_mutex_lock(in_platform_mutex* mutex) {}
static void in_platform_mutex_unlock(in_platform_mutex* mutex) {}
static void in_platform_mutex_free(in_platform_mutex* mutex) {}

static void in_platform_condvar_init(in_platform_condvar* condvar) {}
static void in_platform_condvar_wait(in_platform_condvar* condvar, in_platform_mutex* mutex) {}
static void in_platform_condvar_notify(in_platform_condvar* condvar, in_platform_mutex* mutex) {}
static void in_platform_condvar_free(in_platform_condvar* condvar) {}

#endif
