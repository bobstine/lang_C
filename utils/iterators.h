#ifndef _iterators_h_
#define _iterators_h_

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

template<typename Type>
class JumpIteratorABC: public std::iterator<std::forward_iterator, Type>
{
 public:
  typedef Type value_type;

  virtual void ~JumpIteratorABC() {};
  
  virtual JumpIteratorABC& operator++() = 0;
  virtual value_type       operator*() const = 0;
};


template<typename FromIter, typename ToIter>
  class JumpIterator: public JumpIteratorABC<std::iterator_traits<Iter>::value_type>
{
  FromIter mIter;
  ToIter   mToIter;
  
 public:

 JumpIterator(FromIter const& from, ToIter const& to): mIter(from), mToIter(to) { };

  JumpIterator& operator++()       { ++mIter; }
  value_type    operator*() const  { return *mIter; }
  
};
  

  

template <typename I1, typename I2>  // better have the same value type
  class concat_iterator: public std::iterator<std::forward_iterator_tag, std::iterator_traits<I1>::value_type>
{
  typedef typename std::iterator_traits<I1>::value_type  value_type;
 private:
  hidden_iterator_ABC *mAt;
  
  I1 const mBegin1;
  I1 const mEnd1;
  I2 const mBegin2;
  I2 const mEnd2;
 public:

 concat_iterator(concat_iterator const& i)
   : mBegin1(i.mBegin1), mEnd1(i.mEnd1), mBegin2(i.mBegin2), mEnd2(i.mEnd2) { }
  
 concat_iterator(I1 const& begin1, I1 const& end1, I2 const& begin2, I2 const& end2)
   : mBegin1(begin1), mEnd1(end1), mBegin2(begin2), mEnd2(end2)     { }
  
  concat_iterator& operator++()          
    value_type       operator*()   const { return *(*mAt); }

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
