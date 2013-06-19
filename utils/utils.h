#ifndef _UTILS_H_
#define _UTILS_H_

#include <functional>

// Reverse the ordering of a map to base on counts rather than keys
namespace utils
{
  
  template<class C1, class C2>
    std::map<C1,C2>
    invert_map(std::map<C2,C1> const& m)
    {
      std::map<C1,C2> rm;
      for (auto p : m) rm[p.second]=p.first;
      return rm;
    }

    template<class C1, class C2>
      std::map<C1,C2,std::greater<C1>>
      invert_map_descending(std::map<C2,C1> const& m)
    {
      std::map<C1,C2,std::greater<C1>> rm;
      for (auto p : m) rm[p.second]=p.first;
      return rm;
    }

}

#endif
