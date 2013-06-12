#ifndef _iterators_h_
#define _iterators_h_

#include <assert.h>
#include <vector>
#include <iterator>

#include <iostream>  // debugging

/*

  11 Jun 13 ... Add first/second iterators; seem redundant, but convenient.
  18 Jan 10 ... Add concat iterator.
   6 Dec 05 ... Add fixed-point iterator.
  26 Jan 04 ... Add the cyclic from wavelets.

  Note:  Many of these are pretty old and linked directly to vectors.
  
  Cyclic
  Only supports ++ and * operators, so you can only use these methods
  to walk a double vector in a forward direction.

  Concat
  Only supports forward direction as an efficiency hack: the only test
  comes at operator== which inserts iterator from second block. Resembles
  the car/cdr format of lisp pairs.

*/

template<class VT>
class AnyIteratorABC: public std::iterator<std::forward_iterator_tag, VT>
{
  
 public:
  virtual ~AnyIteratorABC() {};
  
  virtual AnyIteratorABC&  operator++()      = 0;
  virtual VT               operator*() const = 0;

};

template<class Iter>
class AnyIterator: public AnyIteratorABC< typename std::iterator_traits<Iter>::value_type >
{
  Iter mIter;
  
 public:
  typedef          AnyIterator<Iter>                      type_of_this;
  typedef typename std::iterator_traits<Iter>::value_type value_type;
  
 AnyIterator(Iter it) : mIter(it) { }

  AnyIterator& operator++()                                  { ++mIter; return *this; }
  value_type   operator*()                             const { return *mIter; }

  bool         operator==(AnyIterator<Iter> const& it) const { return mIter == it.mIter; }
  bool         operator!=(AnyIterator<Iter> const& it) const { return mIter != it.mIter;}
};



template<typename Iter1, typename Iter2>
  class JoinIterator: public std::iterator<std::forward_iterator_tag, typename std::iterator_traits<Iter1>::value_type>
{
 public:
  typedef typename std::iterator_traits<Iter1>::value_type value_type;

 private:
          AnyIterator<Iter1>  mIter1;
  mutable AnyIterator<Iter2>  mIter2;
  mutable bool                       mJumped;
  mutable AnyIteratorABC<value_type> *mIt;
  
 public:
  ~JoinIterator() { }
    
  JoinIterator(Iter1 const& iter1, Iter2 const& iter2)
    : mIter1(iter1),  mIter2(iter2), mJumped(false), mIt(&mIter1) { }

  JoinIterator(JoinIterator<Iter1, Iter2> const& ji)
    : mIter1(ji.mIter1), mIter2(ji.mIter2), mJumped(ji.mJumped) { mIt = &mIter1; }
  
  JoinIterator& operator++()       { ++(*mIt); return *this; }
  value_type    operator*() const  { return **mIt; }

  bool          operator==(JoinIterator<Iter1,Iter2> const& end) const
  { 
    if (mJumped)                           // check end condition on second iter
      return mIter2 == end.mIter2;
    if (mIter1 == end.mIter1)              // swap iterators when reach end of first
    { mJumped = true;                      // modify mutables
      mIt = &mIter2;
    }
    return false;
  }

  bool          operator!=(JoinIterator<Iter1,Iter2> const& end) const
  { 
    if (mJumped)                           // check end condition on second iter
      return mIter2 != end.mIter2;
    if (mIter1 == end.mIter1)              // swap iterators when reach end of first
    { mJumped = true;                      // modify mutables
      mIt = &mIter2;
    }
    return true;
  }
};

//  Pairs     Pairs     Pairs     Pairs     Pairs     Pairs     Pairs     Pairs     Pairs     

// class M must be an iterator itself that returns a pair of values

template<class Iter>
class FirstIterator: public std::iterator<std::forward_iterator_tag, typename std::iterator_traits<Iter>::value_type::first_type>
{
  typedef typename std::iterator_traits<Iter>::value_type::first_type value_type;
   
  Iter mIt;

public:
  FirstIterator (Iter it)  : mIt (it)                 { }
  
  bool             operator==(FirstIterator const& it) const  { return mIt == it.mIt; }
  bool             operator!=(FirstIterator const& it) const  { return mIt != it.mIt; }
  
  FirstIterator&   operator++()                                { ++mIt; return *this; }
  value_type       operator*()                          const  { return mIt->first; }
};

template<class Iter>
FirstIterator<Iter>
make_first_iterator( Iter it )
{
  return FirstIterator<Iter>(it);
}


template<class Iter>
class SecondIterator: public std::iterator<std::forward_iterator_tag, typename std::iterator_traits<Iter>::value_type::second_type>
{
  typedef typename std::iterator_traits<Iter>::value_type::second_type value_type;
   
  Iter mIt;

public:
  SecondIterator (Iter it)  : mIt (it)                 { }
  
  bool             operator==(SecondIterator const& it) const  { return mIt == it.mIt; }
  bool             operator!=(SecondIterator const& it) const  { return mIt != it.mIt; }
  
  SecondIterator&  operator++()                                { ++mIt; return *this; }
  value_type       operator*()                          const  { return mIt->second; }
};

template<class Iter>
SecondIterator<Iter>
make_second_iterator( Iter it )
{
  return SecondIterator<Iter>(it);
}



template<class Iter, class F>
  class FunctionIterator: public std::iterator<std::forward_iterator_tag, typename F::result_type>
{
  typedef typename F::result_type  value_type;
   
  Iter mIt;
  F    mF;

public:
  FunctionIterator (Iter it, F f)  : mIt (it), mF(f)                 { }
  
  bool             operator==(FunctionIterator const& it) const  { return mIt == it.mIt; }
  bool             operator!=(FunctionIterator const& it) const  { return mIt != it.mIt; }
  
  FunctionIterator&   operator++()                               { ++mIt; return *this; }
  value_type         operator*()                          const  { return mF(*mIt); }
};

template<class Iter, class F>
  FunctionIterator<Iter, F>
  make_function_iterator( Iter it, F f )
{
  return FunctionIterator<Iter,F>(it,f);
}


//  Cyclic   Cyclic   Cyclic   Cyclic   Cyclic   Cyclic   Cyclic   Cyclic   Cyclic   Cyclic   

template <class T>
class cyclic_iterator: public std::iterator<std::forward_iterator_tag, T>
{
 public:
  typedef typename std::vector<T>::const_iterator Iterator;

  cyclic_iterator(Iterator begin,Iterator end): mBegin(begin), mEnd(end), mCurrent(begin) { }
  
  cyclic_iterator& operator++();
  T                operator*()   const;

 private:
  Iterator mBegin, mEnd, mCurrent;
};

template <class T>
cyclic_iterator<T>
make_cyclic_iterator(std::vector<T> const& v)
{
  return cyclic_iterator<T>(v.begin(), v.end());
}

// Permutation   Permutation   Permutation   Permutation   Permutation   Permutation   Permutation   


// Question... how would you assign to a position?
template <class Data, class Permutation>  // containers
class permutation_iterator: public std::iterator<std::forward_iterator_tag, int>
{
  Data        mData;
  Permutation mPermutation;
  int         mPosition;
public:
  permutation_iterator ()
    : mData(), mPermutation(), mPosition(0) { }
  
  permutation_iterator (Data data, Permutation permute)
    : mData(data), mPermutation(permute), mPosition(0)         { }
  
  permutation_iterator& operator++()         { ++mPosition; return *this; }
  // kludge... need container traits to know what's in data
  double                operator*()  const   { return mData[mPermutation[mPosition]]; }
};



//  Periodic   Periodic   Periodic   Periodic   Periodic   Periodic   Periodic   Periodic   Periodic

class periodic_iterator: public std::iterator<std::forward_iterator_tag, int>
{
  int mBegin;
  int mEnd;
  int mAt;
public:
  periodic_iterator ()
    : mBegin(0), mEnd(0), mAt(0)           { }
  
  periodic_iterator(int end)
    : mBegin(0), mEnd(end), mAt(0)         { }

  periodic_iterator(int begin, int end)
    : mBegin(begin), mEnd(end), mAt(begin) { }

  periodic_iterator& operator++()          { if (mAt==mEnd) mAt = mBegin; else ++mAt; return *this; }
  int operator*()                 const    { return mAt; }
};


//  Lag     Lag     Lag     Lag     Lag     Lag     Lag     Lag     Lag     Lag     Lag     Lag     


template<class Iter>
class lag_iterator: public std::iterator<std::random_access_iterator_tag, typename std::iterator_traits<Iter>::value_type>
{

 private:
  Iter        mIter;
  int         mPosition;
  const Iter  mBegin;
  const int   mLag;
  
public:
 lag_iterator(Iter const& it, int lag)
   : mIter(it-lag), mPosition(-lag), mBegin(it), mLag(lag)          { }
 lag_iterator(Iter const& it, int lag, int blockSize)
   : mIter(it-lag*blockSize), mPosition(-lag*blockSize), mBegin(it), mLag(lag)          { }

  lag_iterator& operator++()             { ++mPosition ;  ++mIter; return *this; }
  lag_iterator& operator--()             { --mPosition ;  --mIter; return *this; }
  lag_iterator& operator+=(int j)        { mPosition+=j; mIter+=j; return *this; }
  lag_iterator& operator-=(int j)        { mPosition-=j; mIter-=j; return *this; }
  
  double operator*()            const    { if (mPosition<0) return *mBegin; else return *mIter; }

  typename std::iterator_traits<Iter>::difference_type
    operator-(lag_iterator const& it) const { return mIter - it.mIter; }
  
  bool operator==(lag_iterator const& it) const { return  mIter == it.mIter; }
  bool operator!=(lag_iterator const& it) const { return  mIter != it.mIter; }

  bool operator<(lag_iterator const& it) const { return mIter < it.mIter; }
  bool operator>(lag_iterator const& it) const { return mIter > it.mIter; }
};


template<class Iter>
lag_iterator<Iter>
make_lag_iterator(Iter const& it, int maxLag, int blockSize=1)
{
  return lag_iterator<Iter>(it, maxLag, blockSize);
}


//  Indexed     Indexed     Indexed     Indexed     Indexed     Indexed     Indexed     Indexed     Indexed     Indexed     

template<class ValueIter, class IndexIter>    // value[index]
class indexed_iterator: public std::iterator<std::random_access_iterator_tag, typename std::iterator_traits<ValueIter>::value_type>
{

 private:
  const ValueIter  mBase;
  IndexIter        mIndex;
  
public:
 indexed_iterator(ValueIter const& base, IndexIter const& it)  : mBase(base),mIndex(it)          { }
  
  indexed_iterator& operator++()             {  ++mIndex; return *this; }
  indexed_iterator& operator--()             {  --mIndex; return *this; }
  indexed_iterator& operator+=(int j)        { mIndex+=j; return *this; }
  indexed_iterator& operator-=(int j)        { mIndex-=j; return *this; }
  
  double operator*()            const    { return *(mBase+*mIndex); }
  int    index()                const    { return *mIndex; }         // also here for type checking

  typename std::iterator_traits<IndexIter>::difference_type
    operator-(indexed_iterator const& it) const { return mIndex - it.mIndex; }
  
  bool operator==(indexed_iterator const& it) const { return  mIndex == it.mIndex && mBase == it.mBase; }
  bool operator!=(indexed_iterator const& it) const { return  mIndex != it.mIndex || mBase != it.mBase; }

  bool operator<(indexed_iterator const& it) const { return mIndex < it.mIndex; }
  bool operator>(indexed_iterator const& it) const { return mIndex > it.mIndex; }
};


template<class ValueIter,class IndexIter>
  indexed_iterator<ValueIter,IndexIter>
  make_indexed_iterator(ValueIter const& base, IndexIter const& it)
{
  return indexed_iterator<ValueIter,IndexIter>(base, it);
}



//  Constant   Constant   Constant   Constant   Constant   Constant   Constant   Constant   Constant   

template <class T>
class constant_iterator: public std::iterator<std::forward_iterator_tag, T>
{
  T mValue;
public:
  constant_iterator ()
  : mValue()                 { }
  
  constant_iterator(T value)
  : mValue(value)            { }
  
  constant_iterator<T>& operator++()        { return *this; }
  T operator*()                      const  { return mValue; }    // double& to allow an assigment
};

#include "iterators.Template.h"

#endif
