// $Id: range_ops.h,v 1.2 2002/06/17 14:35:09 bob Exp $ -*- c++ -*-

/*

  15 Mar 02 ... Created to hold the un-localized functions of ranges.

*/

#ifndef _RANGE_OPS_H_
#define _RANGE_OPS_H_

#ifdef __APPLE__
#include "patch_iterator.h"
#endif

#include "range_traits.h"
#include <iostream>

//////////////////////////////  Range versions of algorithms  //////////////////////

template <class Range>
inline typename range_traits<Range>::iterator
min_element(const Range& range)
{
  return min_element(begin(range), end(range));
}

template <class Range>
inline typename range_traits<Range>::iterator
max_element(const Range& range)
{
  return max_element(begin(range), end(range));
}

template <class Range, class I>
inline I copy (const Range& range, I it)
{
  return copy(begin(range), end(range), it);
}

template <class Range, class F>
inline F for_each (const Range& range, F f)
{
  return for_each(begin(range), end(range), f);
}


namespace{
  template <class Range,class Tag>
  inline typename range_traits<Range>::value_type
  accumulate_unrolled(Range range, typename range_traits<Range>::value_type init,Tag)
  {
    // the following is 6 times slower than our own loop!
    // For some reason, its a lot faster to do it here that call the STL.
    //    return accumulate(begin(range), end(range), init);
    for(typename range_traits<Range>::iterator i = begin(range); i != end(range); ++i)
      init = init + *i;
    return init;
  }
  

  // In spite of the "natural version" of the following running blazingly fast, using it here is is amazing slow!
  //    int length = end(range) - begin(range);
  //    for(int i = 0;i < length;++i)
  //      init = init + *(beg + i);
  // RANGE: Time for length 200 was 23.46
  // RANGE: Time for length 2000 was 36.48
  // RANGE: Time for length 20000 was 36.47


  // generic unrolled 6 times code:
  // RANGE: Time for length 200 was 3.26
  // RANGE: Time for length 2000 was 9.55
  // RANGE: Time for length 20000 was 9.45

  // generic not-unrolled code:
  // RANGE: Time for length 200 was 6.35
  // RANGE: Time for length 2000 was 11.06
  // RANGE: Time for length 20000 was 10.96
  
  template <class Range>
  inline typename range_traits<Range>::value_type
  accumulate_unrolled(Range range, typename range_traits<Range>::value_type init,std::random_access_iterator_tag)
  {
    typename range_traits<Range>::iterator i = begin(range);
    if(end(range) - begin(range) > 6)
      {
	typename range_traits<Range>::iterator last = end(range) - 5;
	while(i < last)
	  {
	    init = init + *i; ++i;
	    init = init + *i; ++i;
	    init = init + *i; ++i;
	    init = init + *i; ++i;
	    init = init + *i; ++i;
	  }
      }
    for(;i != end(range);++i)
      init = init + *i;

    return init;
  }
}

/*
using end(range)

RANGE_IP: Time for length 200 was 6.43 giving avg 49.9387
RANGE_IP: Time for length 2000 was 7.18 giving avg 500.405
RANGE_IP: Time for length 20000 was 6.61 giving avg 5000.24
3.0:
RANGE_IP: Time for length 200 was 5.18 giving avg 49.9387
RANGE_IP: Time for length 2000 was 6.26 giving avg 500.405
RANGE_IP: Time for length 20000 was 5.37 giving avg 5000.24

A slight/big improvement if end(Range) uses a const reference

RANGE_IP: Time for length 200 was 4.76 giving avg 49.9387
RANGE_IP: Time for length 2000 was 5.59 giving avg 500.405
RANGE_IP: Time for length 20000 was 4.94 giving avg 5000.24
3.0:
RANGE_IP: Time for length 200 was 2.68 giving avg 49.9387
RANGE_IP: Time for length 2000 was 2.93 giving avg 500.405
RANGE_IP: Time for length 20000 was 2.84 giving avg 5000.24

A big improvement by using "ending" instead of end(range):

RANGE_IP: Time for length 200 was 2.01 giving avg 49.9387
RANGE_IP: Time for length 2000 was 2.12 giving avg 500.405
RANGE_IP: Time for length 20000 was 2.96 giving avg 5000.24
3.0:
RANGE_IP: Time for length 200 was 2.36 giving avg 49.9387
RANGE_IP: Time for length 2000 was 2.56 giving avg 500.405
RANGE_IP: Time for length 20000 was 2.76 giving avg 5000.24

Also a big/slight improvement by using range.second:

RANGE_IP: Time for length 200 was 2.22 giving avg 49.9387
RANGE_IP: Time for length 2000 was 2.03 giving avg 500.405
RANGE_IP: Time for length 20000 was 2.76 giving avg 5000.24
3.0:
RANGE_IP: Time for length 200 was 2.12 giving avg 49.9387
RANGE_IP: Time for length 2000 was 2.56 giving avg 500.405
RANGE_IP: Time for length 20000 was 2.76 giving avg 5000.24

removing the return from operator++() doesn't help:

RANGE_IP: Time for length 200 was 2.04 giving avg 49.9387
RANGE_IP: Time for length 2000 was 2.22 giving avg 500.405
RANGE_IP: Time for length 20000 was 2.84 giving avg 5000.24


TARGET:
        STL_IP: Time for length 200 was 1.45 giving avg 49.8428
	STL_IP: Time for length 2000 was 1.57 giving avg 500.043
	STL_IP: Time for length 20000 was 1.69 giving avg 5002.44
	3.0:
	STL_IP: Time for length 200 was 1.35 giving avg 49.8428
	STL_IP: Time for length 2000 was 1.44 giving avg 500.043
	STL_IP: Time for length 20000 was 1.49 giving avg 5002.44


*/

template <class Range>
inline typename range_traits<Range>::value_type
simple_accumulate (const Range& range, typename range_traits<Range>::value_type init)
{
  //  for(typename range_traits<Range>::iterator i = begin(range); i != end(range); ++i) // very slow

  for(typename range_traits<Range>::iterator i = begin(range); i != range.second; ++i)  // runs fast, but ugly

    //  typename range_traits<Range>::iterator ending = end(range);
    //  for(typename range_traits<Range>::iterator i = begin(range); i != ending; ++i)  // runs fast
      init = init + *i;
    return init;
}

// Unrolled version
template <class Range>
inline typename range_traits<Range>::value_type
accumulate (const Range& range, typename range_traits<Range>::value_type init)
{
  return accumulate_unrolled(range,init,typename range_traits<Range>::iterator_category());
}



template <class R1, class R2, class C>
inline C inner_product (const R1& x, const R2& y, C initial)
{
  return inner_product (begin(x), end(x), begin(y), initial);
}

template <class I1, class I2, class I3, class C>
inline C weighted_inner_product (std::pair<I1,I1> x, std::pair<I2,I2> y, std::pair<I3,I3> wts, C initial)
{
  return inner_product (x, make_binary_range(std::multiplies<double>(),y,wts), initial);
}

template <class R1, class R2, class R3, class C>
inline C weighted_inner_product (const R1& x, const R2& y, const R3& wts, C initial)
{
  return inner_product (x, make_binary_range(std::multiplies<double>(),y,wts), initial);
}


template <class R1, class R2, class UnaryOp>
inline void
transform (const R1& x, const R2& y, UnaryOp f)
{
  transform(begin(x), end(x), begin(y), f);
}

template <class I1, class I2, class I3, class BinaryOp>
inline void
transform (std::pair<I1,I1> x, std::pair<I2,I2> y, std::pair<I3,I3> z,  BinaryOp f)
{
  transform(begin(x), end(x), begin(y), begin(z), f);
}


/////////////////////  Accumulate a table  //////////////////////////////

template <class Range1, class Range2>
inline Range2
accumulate_table(const Range1& table, const Range2& init)
{
  for(typename range_traits<Range1>::iterator i = begin(table); i != end(table); ++i)
    transform(*i, init, init, std::plus<double>());
  return init;
}


namespace range_ops
{

////////////////////////////////  Range output  ///////////////////////////////////

//  Would like for it to look like this...
// template <class Range>
//  inline ostream&
//  operator<<(ostream& os, Range r)
//  {
//    copy(begin(r), end(r), ostream_iterator<typename range_traits<Range>::value_type>(os, " "));
//    os << endl;
//    return os;
//  }

  template <class Iter>
  inline std::ostream&
  operator<<(std::ostream& os, std::pair<Iter,Iter> range)
  {
    copy(range.first, range.second, std::ostream_iterator<typename std::iterator_traits<Iter>::value_type>(os, " "));
    os << std::endl;
    return os;
  }

  ////////////////////////  Operator + - / * /////////////////////////////////////////////////////////


  // COMPRESS OPERATOR: as in APL (+/[1 2 3]) looks like (plus() | v)
  template <class Op,class Range>
  inline
  typename range_traits<Range>::value_type
  operator|(Op o,  const Range& range)
  {
    assert(begin(range) != end(range)); // we don't know what to do with an empty range
    typename range_traits<Range>::iterator start = begin(range);
    typename range_traits<Range>::iterator finish = end(range);
    typename range_traits<Range>::value_type init = *start;
    ++start;
    return std::accumulate(start,finish,init,o);
  }

  template <class Range1,class Range2>
  inline
  typename binary_range_traits<std::multiplies<typename range_traits<Range1>::value_type>,  Range1,  Range2>::range
  operator*(const Range1& range1,  const Range2& range2)
  {
    return make_binary_range(std::multiplies<typename range_traits<Range1>::value_type>(), range1, range2);
  }

  template <class Range1,class Range2>
  inline
  typename binary_range_traits<std::divides<typename range_traits<Range1>::value_type>,  Range1,  Range2>::range
  operator/(const Range1& range1,  const Range2& range2)
  {
    return make_binary_range(std::divides<typename range_traits<Range1>::value_type>(), range1, range2);
  }

  template <class Range1,class Range2>
  inline
  typename binary_range_traits<std::plus<typename range_traits<Range1>::value_type>,  Range1,  Range2>::range
  operator+(const Range1& range1,  const Range2& range2)
  {
    return make_binary_range(std::plus<typename range_traits<Range1>::value_type>(), range1, range2);
  }

  template <class Range1,class Range2>
  inline
  typename binary_range_traits<std::minus<typename range_traits<Range1>::value_type>,  Range1,  Range2>::range
  operator-(const Range1& range1,  const Range2& range2)
  {
    return make_binary_range(std::minus<typename range_traits<Range1>::value_type>(), range1, range2);
  }
}




#endif
