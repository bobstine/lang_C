// $Id: apl.h,v 1.7 2004/09/06 01:04:03 foster Exp $ -*- c++ -*-

/*
  14 Mar 03 ... Add a length function so can specialize later for sparse ranges.
  15 Dec 02 ... Lots of cleaning up, as use with regression code.  Cleaned includes up.
  15 Mar 02 ... Created to hold the un-localized functions of ranges.
*/

#ifndef _APL_H_
#define _APL_H_

#include "function_iterators.h"

#include "range.h"
#include "range_traits.h"
#include "range_ops.h"

#include <iterator>
#include <algorithm>
#include <numeric>         // accumulate, inner_product
#include <iostream>


  
namespace apl
{  
  
  // COMPRESS OPERATOR: as in APL (+/[1 2 3]) looks like (plus() | v)

  template <class Op, class Range>
    inline
    typename range_traits<Range>::value_type
    operator|(Op op,  const Range& container_or_range)
    {
      std::cout << "\n---------------------------------- doing 1 begin" << std::endl;
      begin(container_or_range);
      std::cout << "\n----------------------------------" << std::endl;
      typename range_traits<Range>::range rng = make_range(container_or_range);
      assert(begin(rng) != end(rng)); // we don't know what to do with an empty range
      
      typename range_traits<Range>::const_iterator start = begin(rng);
      typename range_traits<Range>::const_iterator finish = end(rng);
      typename range_traits<Range>::value_type init = *start;
      ++start;
      std::cout << "\n---------------------------------- calling accumulate" << std::endl;
      typename range_traits<Range>::value_type result =  std::accumulate(start,finish,init,op);
      std::cout << "---------------------------------- return from accumulate" << std::endl;
      return result;
    }

  
  // +-*/  +-*/  +-*/  +-*/  +-*/  +-*/  +-*/  +-*/  +-*/  +-*/  +-*/  +-*/  +-*/  +-*/  +-*/  +-*/  +-*/  
  
  template <class Range1, class Range2>
    inline
    typename binary_range_traits<std::multiplies<typename range_traits<Range1>::value_type>,  Range1,  Range2>::range
    operator*(const Range1& range1,  const Range2& range2)
    {
      return make_range(range1) * make_range(range2);
    }

  template <class Range1,class Range2>
    inline
    typename binary_range_traits<std::divides<typename range_traits<Range1>::value_type>,  Range1,  Range2>::range
    operator/(const Range1& range1,  const Range2& range2)
    {
      return make_range(range1) / make_range(range2);
    }
  
  template <class Range1,class Range2>
    inline
    typename binary_range_traits<std::plus<typename range_traits<Range1>::value_type>,  Range1,  Range2>::range
    operator+(const Range1& range1,  const Range2& range2)
    {
      return make_range(range1) + make_range(range2);
    }

  template <class Range1,class Range2>
    inline
    typename binary_range_traits<std::minus<typename range_traits<Range1>::value_type>,  Range1,  Range2>::range
    operator-(const Range1& range1,  const Range2& range2)
    {
      return make_range(range1) - make_range(range2);
    }

}   // namespace APL

namespace range_ops {

  template <class Range, class WeightRange, class Result>
  inline
  Result
  range_ops::weighted_accumulate (const Range& containerOrRange, const WeightRange& wts, Result init)
  {
    using namespace apl;
    typename range_traits<Range>::range r = make_range(containerOrRange);
    return accumulate(
		      r * wts,    // cannot use std::multiplies since may not be doubles
		      init
		      );
  }
}

#endif
