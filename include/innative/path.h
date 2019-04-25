// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef __PATH_H__IN__
#define __PATH_H__IN__

#include "innative/innative.h"
#include <string>
#include <string.h>

namespace innative {
  // Represents an operating system path for pre-C++17 compilers
  class Path : std::string
  {
  public:
#ifdef IN_PLATFORM_WIN32
    static const char SEPERATOR = '\\';
    static const char OTHER = '/';
#elif defined(IN_PLATFORM_POSIX)
    static const char SEPERATOR = '/';
    static const char OTHER = '\\';
#else
#error "unknown platform"
#endif

    Path() {}
    Path(const Path& copy) : _path(copy._path) {}
    Path(Path&& mov) : _path(std::move(mov._path)) {}
    explicit Path(const char* path) : _path(path) { _canonize(); }
    explicit Path(const std::string& path) : _path(path) { _canonize(); }
    explicit Path(std::string&& path) : _path(std::move(path)) { _canonize(); }
    inline bool IsAbsolute() const
    {
      const char* start = _path.c_str();
      const char* pos = strchr(start, SEPERATOR);
#ifdef IN_PLATFORM_WIN32
      return pos != nullptr && pos == start + 2 && pos[-1] == ':';
#elif defined(IN_PLATFORM_POSIX)
      return pos != nullptr && pos == start;
#endif
    }

    // Sets the path and canonizes it
    inline void Set(const Path& path) { _path = path._path; }
    inline void Set(const std::string& path) { _path = path; _canonize(); }
    inline void Set(const char* path) { _path = path; _canonize(); }

    IN_FORCEINLINE void Append(const Path& path) { Append(path._path.c_str()); }
    IN_FORCEINLINE void Append(const std::string& path) { Append(path.c_str()); }
    inline void Append(const char* path)
    {
      if(!path || !path[0])
        return;
      if(_path.size() > 0)
      {
        if(_path.back() == SEPERATOR)
          _path.pop_back();
        if(path[0] != SEPERATOR && path[0] != OTHER)
          _path += SEPERATOR;
      }
      _path += path;
      _canonize();
    }

    // If there is a file extension, removes it
    inline Path RemoveExtension() const
    {
      Path r;
      size_t pos = _path.find_last_of('.');
      r._path = (pos != std::string::npos) ? _path.substr(0, pos) : _path;
      return r;
    }

    inline std::string Extension() const
    {
      size_t pos = _path.find_last_of('.');
      return (pos != std::string::npos) ? _path.substr(pos + 1) : std::string();
    }

    // Either removes the file, if there is one, or the last directory in the path
    inline Path BaseDir() const
    {
      Path r;
      if(!_path.size())
        return r;
      size_t pos = _path.find_last_of(SEPERATOR, _path.size() - 1 - (_path.back() == SEPERATOR));
      r._path = (pos != std::string::npos) ? _path.substr(0, pos + 1) : std::string();
      return r;
    }

    inline Path File() const
    {
      Path r;
      size_t pos = _path.find_last_of(SEPERATOR);
      r._path = (pos != std::string::npos) ? _path.substr(pos + 1) : _path;
      return r;
    }

    inline const std::string& Get() const { return _path; }
    inline const char* c_str() const { return _path.c_str(); }
    inline Path operator+(const Path& right) const { Path r(*this); r += right; return r; }
    inline Path operator+(const std::string& right) const { Path r(*this); r += right; return r; }
    inline Path operator+(const char* right) const { Path r(*this); r += right; return r; }
    inline Path& operator+=(const Path& right) { Append(right); return *this; }
    inline Path& operator+=(const std::string& right) { Append(right); return *this; }
    inline Path& operator+=(const char* right) { Append(right); return *this; }
    inline Path& operator=(const char* copy) { Set(copy); return *this; }
    inline Path& operator=(const std::string& copy) { Set(copy); return *this; }
    inline Path& operator=(const Path& copy) { Set(copy); return *this; }
    inline Path& operator=(Path&& mov) { _path = std::move(mov._path); return *this; }

  protected:
    inline void _canonize()
    {
      for(size_t i = 0; i < _path.size(); ++i)
      {
        if(_path[i] == OTHER)
          _path[i] = SEPERATOR;
      }
    }

    std::string _path;
  };
}

#endif
