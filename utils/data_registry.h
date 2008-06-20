// $Id: data_registry.h,v 1.2 2008/01/30 22:39:58 bob Exp $

/*
  
  A data registry is a map that lets you store pointers for use in
  several objects without having to suffer the slings of duplicate
  allocations or smart pointers.  Its a cheap way to implement smart
  points via tags.

  23 Aug 04 ... Implemented to simplify spline operators.

*/


/*
 Though not needed for spline application, could generalize this
 to manage any sort of dynamic space by adding a generic parameter
 that controls the creation and deletion of objects.
 */

#include <map>
#include <iostream>

template <class Data>
class DataRegistry
{
  int mLastKey;
  std::map<int, Data*> mMap;
  std::map<int, int>   mKeyCount;
  std::map<int, bool>  mReleaseFlag;
  
public:
  typedef int   key_type;
  typedef Data  data_type;

  ~DataRegistry()
  { for (int i=1; i<=mLastKey; ++i)
    if (mKeyCount[i]>0 && mReleaseFlag[i]) delete(mMap[i]);
  }

  DataRegistry()
    : mLastKey(0), mMap(), mKeyCount(), mReleaseFlag() {}
  
  key_type insert_data (Data* x, bool releaseWhenNoMoreReferences)
  {
    ++mLastKey;
    mKeyCount[mLastKey] = 1;
    mMap[mLastKey] = x;
    mReleaseFlag[mLastKey] = releaseWhenNoMoreReferences;
    return mLastKey;
  }
  
  Data* data(key_type key)
  {
    if (mKeyCount[key] == 0)
    { std::cerr << "DREG: Attempt to access a key that has been deleted or does not exist.\n";
      return 0;
    }
    else
      return mMap[key];
  }
  
  void add_reference (key_type key)
  { 
    if (mKeyCount[key] == 0)
      std::cerr << "DREG: Attempt to reference a key that has been deleted.\n";
    else
      ++mKeyCount[key];
  }
  
  void remove_reference (key_type key)
  {
    if (mKeyCount[key] == 0)
      std::cerr << "DREG: Attempt to release a key that had no external references.\n";
    else -- mKeyCount[key];
    if ((mKeyCount[key] == 0) && mReleaseFlag[key])
      delete mMap[key];
  }
  
  void print_to(std::ostream& os) const
  { os << "      Key  Refs  Pointer \n";
    std::map<key_type, int>::const_iterator   keyIter  (mKeyCount.begin());
    typename std::map<key_type, Data*>::const_iterator dataIter (mMap.begin());
    for (int i=1; i<=mLastKey; ++i)
    {	os << "      " << i << "     " << keyIter->second;
      if (keyIter->second > 0) os << "   @ " << dataIter->second;
      os << std::endl;
      ++keyIter;
      ++dataIter;
    }
  }
  
};

template<class Data>
inline
std::ostream&
operator<<(std::ostream& os, DataRegistry<Data> const& d)
{
  d.print_to(os);
  return (os);
}
