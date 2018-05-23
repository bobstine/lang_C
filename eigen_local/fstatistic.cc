//  -*- c++ -*-
#include "fstatistic.h"

#include <boost/math/distributions/fisher_f.hpp>

using boost::math::fisher_f_distribution;

using boost::math::cdf;
using boost::math::quantile;
using boost::math::complement;


/*
  other choices include

  #include <boost/math/distributions/normal.hpp>
  using boost::math::normal;

  #include <boost/math/distributions/students_t.hpp>
  using boost::math::students_t;

  normal norm(0,1);
  students_t tdist(6);
  std::cout << quantile(complement(norm, .05)) << "    " << quantile(complement(tdist,0.05)) ;
*/


void
FStatistic::calc_p_value()
{
  if ( (mF <= 0.0) || std::isnan(mF) || std::isinf(mF) )
    mPValue = 0.0;
  else
  { fisher_f_distribution<Scalar> fdist((Scalar)mNumDF, (Scalar)mDenDF);
    mPValue = cdf(complement(fdist, mF));
  }		  
}


FStatistic::Scalar
FStatistic::critical_value(Scalar p) const
{
  assert ((Scalar)0.0 <= p && p <= (Scalar)1.0);
  if ((Scalar)0 == p)
    return (Scalar)0.0;
  else if ((Scalar)1.0 == p)
    return INFINITY;
  else
  { fisher_f_distribution<Scalar> fdist((Scalar)mNumDF, (Scalar)mDenDF);
    return (Scalar)quantile(complement(fdist, p));
  }
}



