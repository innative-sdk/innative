// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in native-wasm.h

#ifndef __PATH_H__NW__
#define __PATH_H__NW__

#include "native-wasm/native-wasm.h"
#include <string>

// Represents an operating system path
class NWPath
{
public:
  NWPath() {}
  NWPath(const NWPath& copy) : _path(copy._path) {}
  NWPath(NWPath&& mov) : _path(std::move(mov._path)) {}
  explicit NWPath(const char* path) : _path(path) { _canonize(); }
  explicit NWPath(const std::string& path) : _path(path) { _canonize(); }
  explicit NWPath(std::string&& path) : _path(std::move(path)) { _canonize(); }
  inline bool IsAbsolute() {
    const char* start = _path.c_str();
    const char* pos = strchr(start, '/');
    return pos != nullptr && ((pos == start) || (pos[-1] == ':'));
  }

  // Sets the path and canonizes it
  NW_FORCEINLINE void Set(const NWPath& path) { Set(path._path); }
  inline void Set(const std::string& path) { _path = path; _canonize(); }
  inline void Set(const char* path) { _path = path; _canonize(); }

  NW_FORCEINLINE void Append(const NWPath& path) { Append(path._path.c_str()); }
  NW_FORCEINLINE void Append(const std::string& path) { Append(path.c_str()); }
  inline void Append(const char* path)
  {
    if(!path || !path[0])
      return;
    if(path[0] != '/' && path[0] != '\\' && path[0] != '.')
      _path += '/';
    _path += path;
    _canonize();
  }
  
  // If there is a file extension, removes it
  inline NWPath RemoveExtension()
  {
    NWPath r;
    size_t pos = _path.find_last_of('.');
    r._path = (pos != std::string::npos) ? _path.substr(0, pos) : _path;
    return r;
  }

  // Either removes the file, if there is one, or the last directory in the path
  inline NWPath BaseDir()
  {
    NWPath r;
    size_t pos = _path.find_last_of('/');
    r._path = (pos != std::string::npos) ? _path.substr(0, pos) : _path;
    return r;
  }

  inline std::string Get()
  {
    std::string r(_path);
    // Returns the path canonized to the specific operating system
#ifdef NW_PLATFORM_WIN32
    for(size_t i = 0; i < r.size(); ++i)
    {
      if(r[i] == '/')
        r[i] = '\\';
    }
#elif NW_PLATFORM_POSIX
    for(size_t i = 0; i < r.size(); ++i)
    {
      if(r[i] == '\\')
        r[i] = '/';
    }
#else
#error "unknown platform"
#endif
    return r;
  }

  inline NWPath operator+(const NWPath& right) const { NWPath r(*this); r += right; return r; }
  inline NWPath operator+(const std::string& right) const { NWPath r(*this); r += right; return r; }
  inline NWPath operator+(const char* right) const { NWPath r(*this); r += right; return r; }
  inline NWPath& operator+=(const NWPath& right) { Append(right); return *this; }
  inline NWPath& operator+=(const std::string& right) { Append(right); return *this; }
  inline NWPath& operator+=(const char* right) { Append(right); return *this; }
  inline NWPath& operator=(const NWPath& copy) { _path = copy._path; return *this; }
  inline NWPath& operator=(NWPath&& mov) { _path = std::move(mov._path); return *this; }

protected:
  inline void _canonize()
  {
    for(size_t i = 0; i < _path.size(); ++i)
    {
      if(_path[i] == '\\')
        _path[i] = '/';
    }
    if(_path.back() == '/')
      _path.pop_back();
  }

  std::string _path;
};

#endif
