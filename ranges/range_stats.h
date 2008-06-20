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

namespace range_stats
{
  //  R = range or container     I = iterator
  template <class I>            double        average                 (range<I> range, double divisor);
  template <class I, class Iw>  double        weighted_average        (range<I> r, double divisor, range<Iw> wts);

  template <class I>            double        sum_of_squares          (range<I> range);
  template <class I>            double        sum_of_squares          (range<I> range, double center);
  template <class I, class Iw>  double        weighted_sum_of_squares (range<I> r, range<Iw> wts);
  template <class I, class Iw>  double        weighted_sum_of_squares (range<I> r, double center, range<Iw> wts);
  
  template <class I>            double        variance                (range<I> range, double yBar, double df);
  template <class I>            double        standard_deviation      (range<I> range, double yBar, double df);
  
  template <class I, class Iw>  double        weighted_variance       (range<I> r, double yBar, double df, range<Iw>wts);
  template <class I, class Iw>  double        weighted_standard_deviation (range<I> r, double yBar, double df, range<Iw> wts);
  
  template <class I1, class I2> double        covariance              (range<I1> y, double yBar, range<I2> x, double xBar, double df);
  template <class I1, class I2> double        covariance              (range<I1> y, range<I2> x, double df);
  template <class I1, class I2, class Iw>  double weighted_covariance (range<I1> y, double yBar, range<I2> x, double xBar, double df, range<Iw> wts);
  template <class I1, class I2, class Iw>  double weighted_covariance (range<I1> y, range<I2> x, double df, range<Iw> wts);
  
  template <class I1, class I2> double        cross_product           (range<I1> y, double yBar, range<I2> x, double xBar, double n);
  
  template <class R1, class R2, class R3, class I> void
  fill_cross_product_vector (R1 const& y, double yBar, R2 const& x, R3 const& xBar, I cp);
}


#include "range_stats.Template.h"

#endif
