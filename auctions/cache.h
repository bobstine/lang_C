#ifndef _CACHE_H_
#define _CACHE_H_

/*
  Cache works fine with a template, but problem is that you have to
  know and define in the .cc file the types of cache that you will be
  needing.  So its not quite automagic, but the linker will tell you
  what you need.

  15 Apr 04 ... Template version
   8 Apr 04 ... Visitor policy for call-back.
  30 Mar 04 ... Created to factor feature properties out of the features themselves.
*/

#include <map>
#include <iostream>

template<class T, class R>
class Cache: public std::unary_function<T*, R>
{
  static std::map<T*, R> sCache;
  R (T::*mMemFun)();

 public:
  explicit Cache (R (T::*f)()) : mMemFun(f) { }

  R operator()(T* obj) const
  {
    if (sCache.count(obj))
      return sCache[obj];
    else
    { std::cout << "CACH: Filling cache.\n";
      R result((obj->*mMemFun)());
      sCache[obj] = result;
      return result;
    }
  }
};

template<class T, class R>
Cache<T,R>
cache_member_function (R (T::*f)() )
{ 
  return Cache<T,R>(f); 
}


  
/*

  Should this be a map, or have a map?
  -- Has one.  See below.

  How do we find the collection of properties of features?
  -- Its an operator class with a constructor.  With some sort of
     static map that holds its accumulated knowledge.
  
  How will features be identified?
  -- Need a better notion of the operator== function.

  Why lots of maps, rather than a more global map that gives a variety
  of properties of a feature rather than a whole new tree for each?
  -- Easy, you might have the mean easily, but not some other property.
     Think of its map as a cache.
*/

#endif
