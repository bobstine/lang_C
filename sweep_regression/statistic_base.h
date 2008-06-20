/*  $Id: statistic_base.h,v 1.1 2005/06/13 20:47:51 bob Exp $

  Statistic_Base ... These functions work directly on ranges
                     without reference to variables or data sets.

  26 Jan 02 ... Roll in the newer range code with tables.     
  16 Jan 02 ... Created as ranges get more powerful.

*/

#ifndef _STATISTIC_BASE_H_
#define _STATISTIC_BASE_H_


#include "range.h"
#include "functions.h"

#include <math.h>


////  Averages:  use weighted ranges to obtain weighted sums

template <class I>
inline double
average (pair<I,I> range, double divisor)
{
  return accumulate(range,0.0)/divisor;
}

template <class I, class Iw>
inline vector<double>
average_vector (pair<I,I> range, double divisor)
{
  double sum(range.first.size,0.0);
  return accumulate_range(range,make_range(sum));
  transform (sum.begin(), sum.end(), bind2nd(divides<double>(),divisor), sum.begin());
}


////  Variances:  get weighted variances by defining center variables and using
//                versions that do not center at this level of code

template <class I>
inline double
sum_of_squares (pair<I,I> range)
{
  return accumulate(make_unary_range(Function::Square(),range), 0.0);
}

template <class I>
inline double
sum_of_squares (pair<I,I> range, double center)
{
  return accumulate(make_unary_range(Function::CenterSquare(center),range), 0.0);
}

template <class I, class Iw>
inline double
weighted_sum_of_squares (pair<I,I> range, pair<Iw,Iw> wts)
{
  return accumulate(make_weighted_unary_range(Function::Square(),range,wts), 0.0);
}

template <class I, class Iw>
inline double
weighted_sum_of_squares (pair<I,I> range, double center, pair<Iw,Iw> wts)
{
  return accumulate(make_weighted_unary_range(Function::CenterSquare(center),range,wts), 0.0);
}

template <class I>
inline double
variance (pair<I,I> range, double yBar, double df)
{
  return sum_of_squares(range,yBar) / df;
}

template <class I, class Iw>
inline double
weighted_variance (pair<I,I> range, double yBar, double df, pair<Iw,Iw>wts)
{
  return weighted_sum_of_squares(range,yBar,wts) / df;
}

template <class I>
inline double
standard_deviation (pair<I,I> range, double yBar, int df)
{
  return sqrt(variance(range, yBar, df));
}

template <class I, class Iw>
inline double
weighted_standard_deviation (pair<I,I> range, double yBar, int df, pair<Iw,Iw> wts)
{
  return sqrt(variance(range, yBar, df, wts));
}


//// Covariance:  use weighted range for *one* to get a weighted covariance
//                also, do your centering elsewhere.


template <class I1, class I2>
inline double
covariance(pair<I1,I1> y, const double yBar, pair<I2,I2> x, const double xBar, const double df)
{
  return covariance(make_column(Function::Center(yBar),y),
		    make_column(Function::Center(xBar),x),
		    df);
}


template <class I1, class I2>
inline double
covariance(pair<I1,I1> y, pair<I2,I2> x, const double df)
{ 
  return inner_product (y.first, y.second, x.first, 0.0) / df;
}

#endif
