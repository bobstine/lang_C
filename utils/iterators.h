#ifndef _iterators_h_
#define _iterators_h_

#include <assert.h>
#include <vector>
#include <iterator>


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
  typename std::iterator<std::forward_iterator_tag, VT> *mIter;
  
 public:
  virtual ~AnyIteratorABC() {};
  
  virtual AnyIteratorABC&  operator++()      = 0;
  virtual VT               operator*() const = 0;
  virtual bool             operator==(AnyIteratorABC const*) = 0;
};

template<class Iter>
class AnyIterator: public AnyIteratorABC< typename std::iterator_traits<Iter>::value_type >
{
  Iter mIter;
  
 public:
  typedef          AnyIterator<Iter>                      type_of_this;
  typedef typename std::iterator_traits<Iter>::value_type value_type;
  
 AnyIterator(Iter it) : mIter(it) { }

  AnyIterator& operator++()          { ++mIter; return *this; }
  value_type   operator*()     const { return *mIter; }
  bool         operator==(AnyIteratorABC<value_type> const* it) { return mIter == dynamic_cast<Iter>(it->mIter); }
};


template<typename FromIter, typename ToIter>
  class JumpIterator: public std::iterator<std::forward_iterator_tag, typename std::iterator_traits<FromIter>::value_type>
{
 public:
  typedef typename std::iterator_traits<FromIter>::value_type value_type;

 private:
  bool     mJumped;
  FromIter mFromIter;
  ToIter   mToIter;
  AnyIteratorABC<value_type> *mIt;
  
 public:

  ~JumpIterator() { if (mIt) delete(mIt);}
    
  JumpIterator(FromIter const& from, ToIter const& to)
    : mJumped(false), mFromIter(from), mToIter(to) { mIt = new AnyIterator<FromIter>(from); }
  
  JumpIterator& operator++()       { ++(*mIt); return *this; }
  value_type    operator*() const  { return *(*mIt); }

  bool          operator!=(JumpIterator<FromIter, ToIter> const& end)
  {
    if (mJumped)  // check end condition on second iter
      return (mIt->mIter) != end.mToIter; 
    // swap iterators if reach end of first
    if (mIt->mIter == end.mFromIter)
    { assert(mIt);
      delete mIt;
      mIt = new AnyIterator<ToIter>(mToIter);
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
