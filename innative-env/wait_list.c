// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "wait_list.h"
#include "internal.h"

#define IN_MAX(a, b) ((a) > (b) ? (a) : (b))
#define IN_MIN(a, b) ((a) < (b) ? (a) : (b))

static void in_wait_map_entries_cleanup(in_wait_map* map);

static uint64_t in_addr_hash(void* address);

static void in_platform_mutex_init(in_platform_mutex* mutex);
static void in_platform_mutex_lock(in_platform_mutex* mutex);
static void in_platform_mutex_unlock(in_platform_mutex* mutex);
static void in_platform_mutex_free(in_platform_mutex* mutex);

static void in_platform_rwlock_init(in_platform_rwlock* mutex);
static void in_platform_rwlock_lock(in_platform_rwlock* mutex);
static void in_platform_rwlock_unlock(in_platform_rwlock* mutex);
static void in_platform_rwlock_shared_lock(in_platform_rwlock* mutex);
static void in_platform_rwlock_shared_unlock(in_platform_rwlock* mutex);
static void in_platform_rwlock_free(in_platform_rwlock* mutex);

static void in_platform_condvar_init(in_platform_condvar* condvar);
static int in_platform_condvar_wait(in_platform_condvar* condvar, in_platform_mutex* mutex, int64_t timeoutns);
static void in_platform_condvar_notify(in_platform_condvar* condvar);
static void in_platform_condvar_free(in_platform_condvar* condvar);

// Memory stuff
static void* grow_array(void* array, size_t elem_size, size_t old_count, size_t new_count);
static void free_array(void* array, size_t elem_size, size_t old_count);

static size_t in_atomic_incr(size_t* value);
static size_t in_atomic_decr(size_t* value);

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
    return map->cap;
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
  map->cap                      = IN_MAX(map->cap * 2, 32);
  map->entries                  = grow_array(NULL, sizeof(*old), 0, map->cap); // Unfortunately it can't be reused

  for(size_t i = 0; i < old_cap; ++i)
  {
    if(IS_ALIVE(old[i].hash))
    {
      in_wait_map_insert_helper(map, old[i].key, old[i].value);
    }
  }

  free_array(old, sizeof(*old), old_cap);
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
    map->free_count--;
  }
  else
  {
    list = grow_array(NULL, sizeof(*list), 0, 1);
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

  in_wait_list* value = map->entries[idx].value;
  in_atomic_incr(&value->refs);
  return value;
}

in_wait_list* _innative_internal_env_wait_map_get(in_wait_map* map, void* address, int create)
{
  // First try to extract the list with just a read lock
  in_platform_rwlock_shared_lock(&map->lock);
  in_wait_list* result = in_wait_map_get_inner(map, address, 0);
  in_platform_rwlock_shared_unlock(&map->lock);

  // If we don't get one and we need to create a new one, now get an exclusive lock
  if(!result && create)
  {
    in_platform_rwlock_lock(&map->lock);
    result = in_wait_map_get_inner(map, address, 1);
    in_platform_rwlock_unlock(&map->lock);
  }

  return result;
}

void _innative_internal_env_wait_map_return(in_wait_map* map, void* address, in_wait_list* list)
{
  // Put this entry on the free list if it's no longer being used
  if(list->len == 0 && list->outstanding_signals == 0)
  {
    in_platform_rwlock_lock(&map->lock);

    if(in_atomic_decr(&list->refs) == 0)
    {
      size_t idx = in_wait_map_lookup(map, address);
      map->entries[idx].hash |= TOMB_MASK;
      map->len--;

      if(map->free_count < 64)
      {
        list->next_free_list = map->free_lists;
        map->free_lists      = list;
        map->free_count++;
      }
      else
      {
        _innative_internal_env_wait_list_shrink(list);
        free_array(list, sizeof(*list), 1);
      }

      in_wait_map_entries_cleanup(map);
    }

    in_platform_rwlock_unlock(&map->lock);
  }
  else
  {
    in_atomic_decr(&list->refs);
  }
}

static void in_wait_map_free_list_cleanup(in_wait_map* map)
{
  in_wait_list* temp;
  while((temp = map->free_lists) != NULL)
  {
    _innative_internal_env_wait_list_shrink(temp);
    map->free_lists = temp->next_free_list;
    free_array(temp, sizeof(*temp), 1);
  }
}

static void in_wait_map_entries_cleanup(in_wait_map* map)
{
  if(map->len > 0)
  {
    if(map->cap > 64 && map->cap > map->len * 2)
    {
      struct in_wait_map_entry* old = map->entries;
      size_t old_cap                = map->cap;

      map->cap     = IN_MAX(map->len * 2, 32);
      map->entries = grow_array(NULL, sizeof(*old), 0, map->cap);

      for(size_t i = 0; i < old_cap; ++i)
      {
        if(IS_ALIVE(old[i].hash))
        {
          in_wait_map_insert_helper(map, old[i].key, old[i].value);
        }
      }

      free_array(old, sizeof(*old), old_cap);
    }
  }
  else
  {
    free_array(map->entries, sizeof(*map->entries), map->cap);
    map->cap     = 0;
    map->entries = 0;
  }
}

void _innative_internal_env_wait_map_cleanup(in_wait_map* map)
{
  in_platform_rwlock_lock(&map->lock);

  in_wait_map_free_list_cleanup(map);
  in_wait_map_entries_cleanup(map);

  in_platform_rwlock_unlock(&map->lock);
}

// Wait list
void _innative_internal_env_wait_list_enter(in_wait_list* list) { in_platform_mutex_lock(&list->lock); }

void _innative_internal_env_wait_list_exit(in_wait_list* list) { in_platform_mutex_unlock(&list->lock); }

in_wait_entry* _innative_internal_env_wait_list_push(in_wait_list* list)
{
  if(list->len == list->cap)
  {
    size_t old      = list->cap;
    list->cap     = IN_MAX(list->cap * 2, 1);
    list->entries = grow_array(list->entries, sizeof(void*), old, list->cap);
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
    entry = grow_array(NULL, sizeof(*entry), 0, 1);
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
  size_t count = IN_MIN(num, list->len);
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

void _innative_internal_env_wait_list_shrink(in_wait_list* list)
{
  in_wait_entry* temp;

  size_t old    = list->cap;
  list->cap     = list->len;
  list->entries = grow_array(list->entries, sizeof(void*), old, list->cap);
  while((temp = list->free_list) != NULL)
  {
    list->free_list = temp->next_free_node;
    free_array(temp, sizeof(void*), list->cap);
  }
}

// Wait entry
int32_t _innative_internal_env_wait_entry_wait(in_wait_list* list, in_wait_entry* entry, int64_t timeoutns)
{
  while(entry->signaled == 0)
  {
    if(in_platform_condvar_wait(&entry->condvar, &list->lock, timeoutns))
    {
      break; // timeout
    }
  }
  return entry->signaled ? 0 : 2;
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
  return IN_MAX(hash, 1) & ~TOMB_MASK; // 0 is reserved. I feel very sorry for the value that has a hash of 0 anyways lmao
}

#ifdef IN_PLATFORM_WIN32

in_wait_map _innative_internal_env_global_wait_map = { SRWLOCK_INIT };

static void in_platform_mutex_init(in_platform_mutex* mutex) { InitializeSRWLock(mutex); }
static void in_platform_mutex_lock(in_platform_mutex* mutex) { AcquireSRWLockExclusive(mutex); }
static void in_platform_mutex_unlock(in_platform_mutex* mutex) { ReleaseSRWLockExclusive(mutex); }
static void in_platform_mutex_free(in_platform_mutex* mutex) {}

static void in_platform_rwlock_init(in_platform_rwlock* rwlock) { InitializeSRWLock(rwlock); }
static void in_platform_rwlock_lock(in_platform_rwlock* rwlock) { AcquireSRWLockExclusive(rwlock); }
static void in_platform_rwlock_unlock(in_platform_rwlock* rwlock) { ReleaseSRWLockExclusive(rwlock); }
static void in_platform_rwlock_shared_lock(in_platform_rwlock* rwlock) { AcquireSRWLockShared(rwlock); }
static void in_platform_rwlock_shared_unlock(in_platform_rwlock* rwlock) { ReleaseSRWLockShared(rwlock); }
static void in_platform_rwlock_free(in_platform_rwlock* rwlock) {}

static void in_platform_condvar_init(in_platform_condvar* condvar) { InitializeConditionVariable(condvar); }

static int in_platform_condvar_wait(in_platform_condvar* condvar, in_platform_mutex* mutex, int64_t timeoutns)
{
  DWORD timeout;
  if(timeoutns < 0)
    timeout = INFINITE;
  else if(timeoutns < 1'000'000)
    timeout = 1; // timeout 0 would be worse than timeout 1ms if any timeout was requested
  else
    timeout = IN_MIN((DWORD)(timeoutns / 1'000'000), INFINITE - 1);

  BOOL result = SleepConditionVariableSRW(condvar, mutex, timeout, 0);
  return !result;
}

static void in_platform_condvar_notify(in_platform_condvar* condvar) { WakeConditionVariable(condvar); }
static void in_platform_condvar_free(in_platform_condvar* condvar) {}

// Memory stuff
static void* grow_array(void* array, size_t elem_size, size_t old_count, size_t new_count)
{
  if(new_count == 0)
  {
    if(array)
      free_array(array, elem_size, old_count);
    return 0;
  }

  if(array == 0)
  {
    return HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, elem_size * new_count);
  }
  else
  {
    return HeapReAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, array, elem_size * new_count);
  }
}

static void free_array(void* array, size_t elem_size, size_t old_count) { HeapFree(GetProcessHeap(), 0, array); }

  #ifdef _WIN64
static size_t in_atomic_incr(size_t* value) { return InterlockedExchangeAdd64(value, 1) + 1; }
static size_t in_atomic_decr(size_t* value) { return InterlockedExchangeAdd64(value, -1) - 1; }
  #else
static size_t in_atomic_incr(size_t* value) { return InterlockedExchangeAdd(value, 1) + 1; }
static size_t in_atomic_decr(size_t* value) { return InterlockedExchangeAdd(value, -1) - 1; }
  #endif

#elif defined(IN_PLATFORM_POSIX)

in_wait_map _innative_internal_env_global_wait_map = { PTHREAD_RWLOCK_INITIALIZER };

static void in_platform_mutex_init(in_platform_mutex* mutex) { pthread_mutex_init(mutex, NULL); }
static void in_platform_mutex_lock(in_platform_mutex* mutex) { pthread_mutex_lock(mutex); }
static void in_platform_mutex_unlock(in_platform_mutex* mutex) { pthread_mutex_unlock(mutex); }
static void in_platform_mutex_free(in_platform_mutex* mutex) { pthread_mutex_destroy(mutex); }

static void in_platform_rwlock_init(in_platform_rwlock* rwlock) { pthread_rwlock_init(rwlock, NULL); }
static void in_platform_rwlock_lock(in_platform_rwlock* rwlock) { pthread_rwlock_wrlock(rwlock); }
static void in_platform_rwlock_unlock(in_platform_rwlock* rwlock) { pthread_rwlock_unlock(rwlock); }
static void in_platform_rwlock_shared_lock(in_platform_rwlock* rwlock) { pthread_rwlock_rdlock(rwlock); }
static void in_platform_rwlock_shared_unlock(in_platform_rwlock* rwlock) { pthread_rwlock_unlock(rwlock); }
static void in_platform_rwlock_free(in_platform_rwlock* rwlock) { pthread_rwlock_destroy(rwlock); }

static void in_platform_condvar_init(in_platform_condvar* condvar) { pthread_cond_init(condvar, NULL); }
static int in_platform_condvar_wait(in_platform_condvar* condvar, in_platform_mutex* mutex, int64_t timeoutns)
{
  if(timeoutns < 0)
  {
    pthread_cond_wait(condvar, mutex);
    return 0;
  }
  else
  {
    struct timespec abstime = { timeoutns / 1000000000, timeoutns % 1000000000 };
    return pthread_cond_timedwait(condvar, mutex, &abstime) == ETIMEDOUT;
  }
}
static void in_platform_condvar_notify(in_platform_condvar* condvar) { pthread_cond_signal(condvar); }
static void in_platform_condvar_free(in_platform_condvar* condvar) { pthread_cond_destroy(condvar); }

// Memory stuff
static void* grow_array(void* array, size_t elem_size, size_t old_count, size_t new_count)
{
  if(new_count == 0)
  {
    if(array)
      free_array(array, elem_size, old_count);
    return 0;
  }

  if(array == 0)
    array = _innative_syscall(SYSCALL_MMAP, NULL, new_count * elem_size, PROT_READ | PROT_WRITE,
                              MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  else
    array = _innative_syscall(SYSCALL_MREMAP, array, old_count * elem_size, new_count * elem_size, MREMAP_MAYMOVE, 0, 0);

  if(array >= (void*)0xfffffffffffff001) // This is a syscall error from -4095 to -1
    return 0;
  return array;
}

static void free_array(void* array, size_t elem_size, size_t old_count)
{
  _innative_syscall(SYSCALL_MUNMAP, array, elem_size * old_count, 0, 0, 0, 0);
}

static size_t in_atomic_incr(size_t* value) { return __atomic_fetch_add(value, 1, __ATOMIC_RELAXED) + 1; }
static size_t in_atomic_decr(size_t* value) { return __atomic_fetch_add(value, -1, __ATOMIC_RELAXED) - 1; }

#endif
