/*
  Reference counted pointers to an nonvolatile indexed array of floats, doubles or whatever.
*/

#include <iostream>
#include <assert.h>

template <class T>
class DynamicArray;


template <class T>
class DynamicArrayBase
{
  friend class DynamicArray<T>;
  
  int mRefCount;
  const int mLoIndex;
  const int mHiIndex;
  T* mPtr;

 public:
  
  ~DynamicArrayBase() { if (mPtr) delete[] mPtr; }
  
 DynamicArrayBase()
   : mRefCount(0), mLoIndex(0), mHiIndex(0), mPtr (NULL) { }
  
 DynamicArrayBase(int lo, int hi)
   : mRefCount(0), mLoIndex(lo), mHiIndex(hi), mPtr (new T[hi-lo+1]) { assert(hi-lo > 0); increment_ref_count(); }

  
  int size()                       const   { return mHiIndex-mLoIndex+1; }
  
  void assign (int k, T x)                 { assert((mLoIndex <= k) && (k <= mHiIndex)); mPtr[k-mLoIndex] = x; }
  
  T    operator[](int k)           const   { assert((mLoIndex <= k) && (k <= mHiIndex)); return mPtr[k-mLoIndex]; }

  void print_to (std::ostream& os) const   { os << "DynArray[" << size() << ",r" << mRefCount << "] "; for (int i=0; i<size(); ++i) os << mPtr[i] << " "; }

 private:
  void increment_ref_count()               { ++mRefCount; } // std::cout << " Increment " << mPtr << " ref to " << mRefCount << std::endl; }
  void decrement_ref_count()               { --mRefCount; } // std::cout << " Decrement " << mPtr << " ref to " << mRefCount << std::endl; }
};



template <class T>
inline
std::ostream&
operator<< (std::ostream& os, DynamicArrayBase<T> const& dab)
{
  dab.print_to(os);
  return (os);
}



template <class T>
class DynamicArray
{
  DynamicArrayBase<T> *mDAB;

 public:
  ~DynamicArray()     { mDAB->decrement_ref_count(); if (mDAB->mRefCount <= 0) delete mDAB; }
  
  DynamicArray(int lo, int hi)
    : mDAB(new DynamicArrayBase<T>(lo,hi)) { }

  DynamicArray(DynamicArray const& da)
    : mDAB(da.mDAB) { mDAB->increment_ref_count(); }

  int min_index ()                 const { return mDAB->mLoIndex; }
  int max_index ()                 const { return mDAB->mHiIndex; }

  void assign (int k, T x)               { mDAB->assign(k,x); }
   
  T    operator[](int k)           const { return (*mDAB)[k]; }

  void print_to (std::ostream& os) const { mDAB->print_to(os); }

  DynamicArray& operator= (DynamicArray const& da)
  {
    mDAB->decrement_ref_count();
    if( (mDAB->mRefCount <= 0) && (mDAB != da.mDAB) ) delete mDAB;
    mDAB = da.mDAB;
    mDAB->increment_ref_count();
    return *this;
  }

};


template <class T>
inline
std::ostream&
operator<< (std::ostream& os, DynamicArray<T> const& da)
{
  da.print_to(os);
  return (os);
}

