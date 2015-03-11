// $Id: range_stats.Template.h,v 1.6 2004/04/29 16:54:21 bob Exp $ -*- c++ -*-

////  Averages

template <class I>
inline Ranges::Scalar
Ranges::average (Ranges::range<I> range, Ranges::Scalar divisor)
{
  return range_ops::accumulate(range,(Ranges::Scalar)0.0)/divisor;
}

template <class Ix, class Iw>
inline Ranges::Scalar
Ranges::weighted_average (Ranges::range<Ix> r, Ranges::Scalar divisor, Ranges::range<Iw> wts)
{
  return range_ops::inner_product(r,wts,0.0)/divisor;
}

////  Variances and SD
//    No automatic centering so that we can use sparse ranges.

template <class I>
inline Ranges::Scalar
Ranges::sum_of_squares (Ranges::range<I> range)
{
  return range_ops::accumulate(make_unary_range(Function_Utils::Square(),range), 0.0);
}


template <class I>
inline Ranges::Scalar
Ranges::sum_of_squares (Ranges::range<I> range, Ranges::Scalar center)
{
  return range_ops::accumulate(make_unary_range(Function_Utils::CenteredSquare(center),range), (Ranges::Scalar)0.0);
}

template<class I, class Iw>
Ranges::Scalar
Ranges::weighted_sum_of_squares (Ranges::range<I> x, Ranges::range<Iw> wts)
{
  Ranges::Scalar wss (0.0);
  typename range_traits< Ranges::range<Iw> >::const_iterator wPtr (begin(wts));
  for(typename range_traits< Ranges::range<I> >::const_iterator xPtr = begin(x); xPtr != end(x); ++xPtr, ++wPtr)
    wss += (*xPtr) * (*xPtr) * (*wPtr);
  return wss;
}

/*  Seems to kill GCC 3.3 on OS-x in logistic regression scoring code
template <class I, class Iw>
inline Ranges::Scalar
Ranges::weighted_sum_of_squares (Ranges::range<I> r, Ranges::range<Iw> wts)
{
  return range_ops::accumulate(
		    make_binary_range(
				      std::multiplies<Ranges::Scalar>(),
				      make_unary_range(Function_Utils::Square(),r),
				      wts),
		    0.0);      
}
*/

template <class I, class Iw>
inline Ranges::Scalar
Ranges::weighted_sum_of_squares (Ranges::range<I> r, Ranges::Scalar center, Ranges::range<Iw> wts)
{
  return range_ops::accumulate(
		    make_binary_range(
				      std::multiplies<Ranges::Scalar>(),
				      make_unary_range(Function_Utils::CenteredSquare(center),r),
				      wts),
		    0.0);      
}

// Note:  these are numerically 'desirable' but must be fixed for sparse ranges

template <class I>
inline Ranges::Scalar
Ranges::variance (Ranges::range<I> range, Ranges::Scalar yBar, Ranges::Scalar df)
{
  return sum_of_squares(range, yBar) / df;
}

template <class I, class Iw>
inline Ranges::Scalar
Ranges::weighted_variance (Ranges::range<I> r, Ranges::Scalar yBar, Ranges::Scalar df, Ranges::range<Iw>wts)
{
  return weighted_sum_of_squares(r,yBar,wts) / df;
}

template <class I>
inline Ranges::Scalar
Ranges::standard_deviation (Ranges::range<I> range, Ranges::Scalar yBar, Ranges::Scalar df)
{
  return (Ranges::Scalar)sqrt(variance(range, yBar, df));
}

template <class I, class Iw>
inline Ranges::Scalar
Ranges::weighted_standard_deviation (Ranges::range<I> r, Ranges::Scalar yBar, Ranges::Scalar df, Ranges::range<Iw> wts)
{
  return (Ranges::Scalar)sqrt(weighted_variance(r, yBar, df, wts));
}


//// Covariance:  do your centering elsewhere.


template <class I1, class I2>
inline Ranges::Scalar
Ranges::covariance(Ranges::range<I1> y, Ranges::Scalar yBar, Ranges::range<I2> x, Ranges::Scalar xBar, Ranges::Scalar df)
{
  return covariance(make_unary_range(Function_Utils::Center(yBar),y),
		    make_unary_range(Function_Utils::Center(xBar),x),
		    df);
}


template <class I1, class I2>
inline Ranges::Scalar
Ranges::covariance(Ranges::range<I1> y, Ranges::range<I2> x, Ranges::Scalar df)
{ 
  return range_ops::accumulate(make_binary_range(std::multiplies<Ranges::Scalar>(),y,x), 0.0) / df;
}


template <class I1, class I2, class Iw>
inline Ranges::Scalar
Ranges::weighted_covariance(Ranges::range<I1> y, Ranges::Scalar yBar, Ranges::range<I2> x,  Ranges::Scalar xBar, Ranges::Scalar df, Ranges::range<Iw> wts)
{
  return weighted_covariance(make_unary_range(Function_Utils::Center(yBar),y),
			     make_unary_range(Function_Utils::Center(xBar),x),
			     df,
			     wts);
}

template <class I1, class I2, class Iw>
inline Ranges::Scalar
Ranges::weighted_covariance(Ranges::range<I1> y, Ranges::range<I2> x, Ranges::Scalar df, Ranges::range<Iw> wts)
{
  return range_ops::inner_product(make_binary_range(std::multiplies<Ranges::Scalar>(),y,x), wts, 0.0) / df;
}


//  Cross-products are not so numerically sound, but avoid issues for sparse ranges

template <class I1, class I2>
inline Ranges::Scalar
Ranges::cross_product (Ranges::range<I1> y, Ranges::Scalar yBar, Ranges::range<I2> x, Ranges::Scalar xBar, Ranges::Scalar n)
{
  return
    range_ops::accumulate(make_binary_range(std::multiplies<Ranges::Scalar>(), x, y),
			  0.0)
    - n * yBar * xBar;
}



template <class R1, class R2, class R3, class Iter>
void
Ranges::fill_cross_product_vector (R1 const& y, Ranges::Scalar yBar,         //  iterates over scalars
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
