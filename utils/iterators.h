#ifndef _iterators_h_
#define _iterators_h_

#include <assert.h>
#include <vector>
#include <iterator>

#include <iostream>  // debugging

/*

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

  
//  Cyclic   Cyclic   Cyclic   Cyclic   Cyclic   Cyclic   Cyclic   Cyclic   Cyclic   Cyclic   

class cyclic_iterator: public std::iterator<std::forward_iterator_tag, double>
{
public:
  cyclic_iterator(std::vector<double>::iterator begin,
		  std::vector<double>::const_iterator end):
    mBegin (begin),
    mEnd (end),
    mCurrent(begin)
    {
    }
  
    cyclic_iterator& operator++();
    double&          operator*()   const;
  
private:
  std::vector<double>::iterator mBegin;
  std::vector<double>::const_iterator mEnd;
  std::vector<double>::iterator mCurrent;
};


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
  T operator*()                      const  { return mValue; }
};


#endif
