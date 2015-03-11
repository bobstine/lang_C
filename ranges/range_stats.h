/*  $Id: range_stats.h,v 1.21 2008/01/30 22:39:58 bob Exp $  -*- c++ -*-

   These functions compute a scalar statistic from a range without
   reference to more elaborate objects such as variables or data sets.

   7 Jan 03 ... Renamed and moved from prior statistic_base code.
  13 Dec 02 ... Revise for newer range code.
  26 Jan 02 ... Roll in the newer range code with tables.     
  16 Jan 02 ... Created as ranges get more powerful.

*/

#ifndef _RANGE_STATS_H_
#define _RANGE_STATS_H_

#include "range.h"
#include "range_traits.h"
#include "range_ops.h"

#include "function_utils.h"

// template< class A, class B >  double test_function (A a, B b);

namespace Ranges
{
  typedef float Scalar;

  //  R = range or container     I = iterator
  template <class I>            Scalar        average                 (Ranges::range<I> range, Scalar divisor);
  template <class I, class Iw>  Scalar        weighted_average        (Ranges::range<I> r, Scalar divisor, Ranges::range<Iw> wts);

  template <class I>            Scalar        sum_of_squares          (Ranges::range<I> range);
  template <class I>            Scalar        sum_of_squares          (Ranges::range<I> range, Scalar center);
  template <class I, class Iw>  Scalar        weighted_sum_of_squares (Ranges::range<I> r, Ranges::range<Iw> wts);
  template <class I, class Iw>  Scalar        weighted_sum_of_squares (Ranges::range<I> r, Scalar center, Ranges::range<Iw> wts);
  
  template <class I>            Scalar        variance                (Ranges::range<I> range, Scalar yBar, Scalar df);
  template <class I>            Scalar        standard_deviation      (Ranges::range<I> range, Scalar yBar, Scalar df);
  
  template <class I, class Iw>  Scalar        weighted_variance       (Ranges::range<I> r, Scalar yBar, Scalar df, Ranges::range<Iw>wts);
  template <class I, class Iw>  Scalar        weighted_standard_deviation (Ranges::range<I> r, Scalar yBar, Scalar df, Ranges::range<Iw> wts);
  
  template <class I1, class I2> Scalar        covariance              (Ranges::range<I1> y, Scalar yBar, Ranges::range<I2> x, Scalar xBar, Scalar df);
  template <class I1, class I2> Scalar        covariance              (Ranges::range<I1> y, Ranges::range<I2> x, Scalar df);
  template <class I1, class I2, class Iw>  Scalar weighted_covariance (Ranges::range<I1> y, Scalar yBar, Ranges::range<I2> x, Scalar xBar, Scalar df, Ranges::range<Iw> wts);
  template <class I1, class I2, class Iw>  Scalar weighted_covariance (Ranges::range<I1> y, Ranges::range<I2> x, Scalar df, Ranges::range<Iw> wts);
  
  template <class I1, class I2> Scalar        cross_product           (Ranges::range<I1> y, Scalar yBar, Ranges::range<I2> x, Scalar xBar, Scalar n);
  
  template <class R1, class R2, class R3, class I> void
  fill_cross_product_vector (R1 const& y, Scalar yBar, R2 const& x, R3 const& xBar, I cp);
}


#include "range_stats.Template.h"

#endif
