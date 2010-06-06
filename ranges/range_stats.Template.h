// $Id: range_stats.Template.h,v 1.6 2004/04/29 16:54:21 bob Exp $ -*- c++ -*-

////  Averages

template <class I>
inline double
range_stats::average (Ranges::range<I> range, double divisor)
{
  return range_ops::accumulate(range,0.0)/divisor;
}

template <class Ix, class Iw>
inline double
range_stats::weighted_average (Ranges::range<Ix> r, double divisor, Ranges::range<Iw> wts)
{
  return range_ops::inner_product(r,wts,0.0)/divisor;
}

////  Variances and SD
//    No automatic centering so that we can use sparse ranges.

template <class I>
inline double
range_stats::sum_of_squares (Ranges::range<I> range)
{
  return range_ops::accumulate(make_unary_range(Function_Utils::Square(),range), 0.0);
}


template <class I>
inline double
range_stats::sum_of_squares (Ranges::range<I> range, double center)
{
  return range_ops::accumulate(make_unary_range(Function_Utils::CenteredSquare(center),range), 0.0);
}

template<class I, class Iw>
double
range_stats::weighted_sum_of_squares (Ranges::range<I> x, Ranges::range<Iw> wts)
{
  double wss (0.0);
  typename range_traits< Ranges::range<Iw> >::const_iterator wPtr (begin(wts));
  for(typename range_traits< Ranges::range<I> >::const_iterator xPtr = begin(x); xPtr != end(x); ++xPtr, ++wPtr)
    wss += (*xPtr) * (*xPtr) * (*wPtr);
  return wss;
}

/*  Seems to kill GCC 3.3 on OS-x in logistic regression scoring code
template <class I, class Iw>
inline double
range_stats::weighted_sum_of_squares (Ranges::range<I> r, Ranges::range<Iw> wts)
{
  return range_ops::accumulate(
		    make_binary_range(
				      std::multiplies<double>(),
				      make_unary_range(Function_Utils::Square(),r),
				      wts),
		    0.0);      
}
*/

template <class I, class Iw>
inline double
range_stats::weighted_sum_of_squares (Ranges::range<I> r, double center, Ranges::range<Iw> wts)
{
  return range_ops::accumulate(
		    make_binary_range(
				      std::multiplies<double>(),
				      make_unary_range(Function_Utils::CenteredSquare(center),r),
				      wts),
		    0.0);      
}

// Note:  these are numerically 'desirable' but must be fixed for sparse ranges

template <class I>
inline double
range_stats::variance (Ranges::range<I> range, double yBar, double df)
{
  return sum_of_squares(range, yBar) / df;
}

template <class I, class Iw>
inline double
range_stats::weighted_variance (Ranges::range<I> r, double yBar, double df, Ranges::range<Iw>wts)
{
  return weighted_sum_of_squares(r,yBar,wts) / df;
}

template <class I>
inline double
range_stats::standard_deviation (Ranges::range<I> range, double yBar, double df)
{
  return sqrt(variance(range, yBar, df));
}

template <class I, class Iw>
inline double
range_stats::weighted_standard_deviation (Ranges::range<I> r, double yBar, double df, Ranges::range<Iw> wts)
{
  return sqrt(weighted_variance(r, yBar, df, wts));
}


//// Covariance:  do your centering elsewhere.


template <class I1, class I2>
inline double
range_stats::covariance(Ranges::range<I1> y, double yBar, Ranges::range<I2> x, double xBar, double df)
{
  return covariance(make_unary_range(Function_Utils::Center(yBar),y),
		    make_unary_range(Function_Utils::Center(xBar),x),
		    df);
}


template <class I1, class I2>
inline double
range_stats::covariance(Ranges::range<I1> y, Ranges::range<I2> x, double df)
{ 
  return range_ops::accumulate(make_binary_range(std::multiplies<double>(),y,x), 0.0) / df;
}


template <class I1, class I2, class Iw>
inline double
range_stats::weighted_covariance(Ranges::range<I1> y, double yBar, Ranges::range<I2> x,  double xBar, double df, Ranges::range<Iw> wts)
{
  return weighted_covariance(make_unary_range(Function_Utils::Center(yBar),y),
			     make_unary_range(Function_Utils::Center(xBar),x),
			     df,
			     wts);
}

template <class I1, class I2, class Iw>
inline double
range_stats::weighted_covariance(Ranges::range<I1> y, Ranges::range<I2> x, double df, Ranges::range<Iw> wts)
{
  return range_ops::inner_product(make_binary_range(std::multiplies<double>(),y,x), wts, 0.0) / df;
}


//  Cross-products are not so numerically sound, but avoid issues for sparse ranges

template <class I1, class I2>
inline double
range_stats::cross_product (Ranges::range<I1> y, double yBar, Ranges::range<I2> x, double xBar, double n)
{
  return
    range_ops::accumulate(make_binary_range(std::multiplies<double>(), x, y),
			  0.0)
    - n * yBar * xBar;
}



template <class R1, class R2, class R3, class Iter>
void
range_stats::fill_cross_product_vector (R1 const& y, double yBar,         //  iterates over scalars
			   R2 const& x, R3 const& xBar,  //  iterates over vectors
			   Iter cp)
{
  typename range_traits<R1>::range  yRange (Ranges::make_range(y));

  typedef typename range_traits<R2>::range  MatrixRange;
  MatrixRange mat(Ranges::make_range(x));
  typename range_traits<MatrixRange>::const_iterator matColIter (begin(mat));
  
  typedef typename range_traits<R3>::range  AvgRange;
  AvgRange avgRange (Ranges::make_range(xBar));
  typename range_traits<AvgRange>::const_iterator avgIter (begin(avgRange));

  int k ((int) (end(mat)-begin(mat)));
  int n ((int) (Ranges::end(y)-Ranges::begin(y)));
  for (int i=0; i < k; ++i)
  { 
    *cp = cross_product(yRange, yBar, Ranges::make_range(*matColIter), *avgIter, n);
    ++cp;
    ++avgIter;
    ++matColIter;
  }
}
