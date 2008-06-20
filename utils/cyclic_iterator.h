// $Id: cyclic_iterator.h,v 1.4 2005/12/09 20:42:31 bob Exp $-*- c++ -*-

#ifndef _cyclic_iterator_h_
#define _cyclic_iterator_h_

#include <vector>
#include <iterator>


/*
 
   6 Dec 05 ... Add fixed-point iterator.
  26 Jan 04 ... Add the cyclic from wavelets.

  Only supports ++ and * operators, so you can only use these methods
  to walk a double vector in a forward direction.

*/

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
