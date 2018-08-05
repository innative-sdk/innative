// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef __PATH_H__IR__
#define __PATH_H__IR__

#include "innative/innative.h"
#include <string>

namespace innative {
  // Represents an operating system path
  class Path
  {
  public:
    Path() {}
    Path(const Path& copy) : _path(copy._path) {}
    Path(Path&& mov) : _path(std::move(mov._path)) {}
    explicit Path(const char* path) : _path(path) { _canonize(); }
    explicit Path(const std::string& path) : _path(path) { _canonize(); }
    explicit Path(std::string&& path) : _path(std::move(path)) { _canonize(); }
    inline bool IsAbsolute()
    {
      const char* start = _path.c_str();
      const char* pos = strchr(start, '/');
      return pos != nullptr && ((pos == start) || (pos[-1] == ':'));
    }

    // Sets the path and canonizes it
    IR_FORCEINLINE void Set(const Path& path) { Set(path._path); }
    inline void Set(const std::string& path) { _path = path; _canonize(); }
    inline void Set(const char* path) { _path = path; _canonize(); }

    IR_FORCEINLINE void Append(const Path& path) { Append(path._path.c_str()); }
    IR_FORCEINLINE void Append(const std::string& path) { Append(path.c_str()); }
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
    inline Path RemoveExtension()
    {
      Path r;
      size_t pos = _path.find_last_of('.');
      r._path = (pos != std::string::npos) ? _path.substr(0, pos) : _path;
      return r;
    }

    // Either removes the file, if there is one, or the last directory in the path
    inline Path BaseDir()
    {
      Path r;
      size_t pos = _path.find_last_of('/');
      r._path = (pos != std::string::npos) ? _path.substr(0, pos) : _path;
      return r;
    }

    inline std::string Get()
    {
      std::string r(_path);
      // Returns the path canonized to the specific operating system
#ifdef IR_PLATFORM_WIN32
      for(size_t i = 0; i < r.size(); ++i)
      {
        if(r[i] == '/')
          r[i] = '\\';
      }
#elif IR_PLATFORM_POSIX
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

    inline Path operator+(const Path& right) const { Path r(*this); r += right; return r; }
    inline Path operator+(const std::string& right) const { Path r(*this); r += right; return r; }
    inline Path operator+(const char* right) const { Path r(*this); r += right; return r; }
    inline Path& operator+=(const Path& right) { Append(right); return *this; }
    inline Path& operator+=(const std::string& right) { Append(right); return *this; }
    inline Path& operator+=(const char* right) { Append(right); return *this; }
    inline Path& operator=(const Path& copy) { _path = copy._path; return *this; }
    inline Path& operator=(Path&& mov) { _path = std::move(mov._path); return *this; }

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
}

#endif
