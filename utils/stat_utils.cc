#include "stat_utils.h"

#include <numeric>  // for accumulate
#include <math.h>   // for sqrt, nan
#include <assert.h>


// for distributions
#include <boost/math/distributions/students_t.hpp>
using boost::math::students_t;
#include <boost/math/distributions/fisher_f.hpp>
using boost::math::fisher_f;

using boost::math::cdf;
using boost::math::quantile;
using boost::math::complement;


namespace {
  
  class CenterSquare {
    double mCenter;
  public:
  CenterSquare(double center)
    : mCenter(center) { }
    double operator()(double ss, double x)
    { double err (x - mCenter);
      return ss + err*err;
    }
  };

}


std::pair<double,double>
Stat_Utils::mands (const std::vector<double>& x)
{
  assert (x.size() > 1);
  double xBar = accumulate(x.begin(), x.end(), 0.0)/(double)x.size();
  return std::make_pair(xBar, standard_deviation(x, xBar));
}

double
Stat_Utils::standard_deviation (std::vector<double> const& x, double avg)
{
  double ss = std::accumulate(x.begin(), x.end(), 0.0, CenterSquare(avg));
  assert (ss >= 0);
  return sqrt(ss/(double)(x.size()-1));
}

////  Tests and p-values

std::pair<double,double>
Stat_Utils::t_test(double tStat, int df)
{
  if (std::isnan(tStat) || std::isinf(tStat))
    return std::make_pair(0.0,1.001);
  else
  { students_t tdist(df);
    return std::make_pair(tStat, cdf(complement(tdist, tStat)));
  }    
}

std::pair<double,double> 
Stat_Utils::f_test(double f, int numDF, int denDF)
{
  if ( std::isnan(f) || std::isinf(f) || (f <= 0) )
    return std::make_pair(0.0,1.01);
  else
  { fisher_f fdist(numDF, denDF);
    return std::make_pair(f, cdf(complement(fdist, f)));
  }
}

std::pair<double,double> 
Stat_Utils::f_test(double numSS, int numDF, double denSS, int denDF)
{
  if ( ( std::isnan(numSS) || std::isnan(denSS) ) ||
       ( std::isinf(numSS) || std::isinf(denSS) ) ||
       ( (numSS <= 0.0) || (denSS <= 0.0)) )
    return std::make_pair(0.0,1.01);
  else
  { double fRatio ((numSS * denDF)/(denSS * numDF));
    fisher_f fdist(numDF, denDF);
    return std::make_pair(fRatio, cdf(complement(fdist,fRatio)));
  }
}
