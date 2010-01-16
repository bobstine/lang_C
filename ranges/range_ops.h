// -*- c++ -*-
// $Id: range_ops.h,v 1.43 2004/04/29 16:54:20 bob Exp $ -*- c++ -*-

/*
  14 Mar 03 ... Add a length function so can specialize later for sparse ranges.
  15 Dec 02 ... Lots of cleaning up, as use with regression code.  Cleaned includes up.
  15 Mar 02 ... Created to hold the un-localized functions of ranges.
*/

#ifndef _RANGE_OPS_H_
#define _RANGE_OPS_H_

#include "range.h"
#include "range_traits.h"
#include "function_iterators.h"

#include <iterator>
#include <algorithm>
#include <numeric>         // accumulate, inner_product

#include <ostream>         // debugging

#ifndef SPARSE_ITERATOR_TAG
#define SPARSE_ITERATOR_TAG
struct sparse_iterator_tag: public std::random_access_iterator_tag {};
#endif

//////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//   Forward declairations:  (In other words, this summarizes what this header file acutally delivers):
//
// R,R1,R2 = range_or_container I = iterator
//
namespace range_ops
{ 
  template <class R, class Pred>   typename range_traits<R>::difference_type       count_if (R range, Pred pred);
  template <class R>               typename range_traits<R>::const_iterator        min_element(const R& range);
  template <class R>               typename range_traits<R>::const_iterator        max_element(const R& range);

  template <class R, class I>      I                                               copy (const R& range, I it);
  template <class R, class F>      F                                               for_each (const R& range, F f);
  template <class R>               void                                            fill(R r, typename range_traits<R>::value_type value);
  
  template <class R>               typename range_traits<R>::value_type            simple_accumulate (const R&, typename range_traits<R>::value_type);
  template <class R, class Result>                           Result                accumulate (const R&, Result init);
  template <class R1, class R2, class C>                     C                     inner_product (const R1&, const R2&, C init);
  template <class R1, class R2, class R3, class C>           C                     weighted_inner_product (const R1&,const R2&,const R3& wts,C initial);

  template <class R, class I, class UnaryOp>                 void                  transform (R const&, I iter, UnaryOp f);
  template <class R1, class R2, class I, class BinaryOp>     void                  transform (R1 const& x, R2 const& y, I iter,  BinaryOp f);
}

template <class Iter>                                       std::ostream&          operator<<(std::ostream& os, const range<Iter>& r);


// Also defined (but with long return types, so they are left off):
//
// accumulate (const range<Iter>& , typename std::iterator_traits<Iter>::value_type);
//
// operator * (const range<Iter1> ,  const range<Iter2> )   // communtative
// operator * (double             ,  const range<Iter>& )
// operator + (const range<Iter1> ,  const range<Iter2> )
// operator + (double             ,  const range<Iter>& )
// operator / (const range<Iter1> ,  const range<Iter2> )   // non-commutative
// operator / (const range<Iter1> ,  double             )
// operator / (double             ,  const range<Iter1> )
// operator - (const range<Iter1> ,  const range<Iter2> )
// operator - (double             ,  const range<Iter2> )
// operator - (const range<Iter1> ,  double             )
//
///////////////////////////////////////////////////////////////////////////////////////////////////////////////

#include "range_ops.Template.h"

#endif
