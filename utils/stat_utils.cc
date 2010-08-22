#include "stat_utils.h"

#include <numeric>  // for accumulate
#include <math.h>   // for sqrt
#include <assert.h>


// for distributions

#ifndef NOGSL
#include <gsl/gsl_sf_erf.h>
#include <gsl/gsl_cdf.h>
#endif

using namespace Stat_Utils;

namespace {

  class CenterSquare {
    double mCenter;
  public:
    CenterSquare(double center)
      : mCenter(center) { }
    double operator()(double ss, double x)
    {
      double err (x - mCenter);
      return ss + err*err;
    }
  };
}

std::pair<double,double>
Stat_Utils::mands (const std::vector<double>& x)
{
  assert (x.size() > 1);
  double xBar = accumulate(x.begin(), x.end(), 0.0)/x.size();
  return std::make_pair(xBar, standard_deviation(x, xBar));
}

double
Stat_Utils::standard_deviation (std::vector<double> const& x, double avg)
{
  double ss = std::accumulate(x.begin(), x.end(), 0.0, CenterSquare(avg));
  assert (ss >= 0);
  return sqrt(ss/(x.size()-1));
}

////  Tests and p-values

std::pair<double,double>
Stat_Utils::t_test(double tStat, int df)
{
  if (isnan(tStat) || isinf(tStat))
    return std::make_pair(0.0,1.001);
#ifndef NOGSL
      double p = gsl_cdf_tdist_Q(tStat, df);    
#else
      double p = exp(-2*exp(2 * log(tStat)));
#endif
      return std::make_pair(tStat, p);
}

std::pair<double,double> 
Stat_Utils::f_test(double f, int numDF, int denDF)
{
  if ( isnan(f) || isinf(f) || (f <= 0) )
    return std::make_pair(0.0,1.01);
#ifndef NOGSL
      double p = gsl_cdf_fdist_Q(f, numDF, denDF);    
#else
      double p = exp(-2*f);  // wow!  THis is crappy
#endif
return std::make_pair(f, p);
}

std::pair<double,double> 
Stat_Utils::f_test(double numSS, int numDF, double denSS, int denDF)
{
  if ( ( isnan(numSS) || isnan(denSS) ) ||
       ( isinf(numSS) || isinf(denSS) ) ||
       ( (numSS <= 0.0) || (denSS <= 0.0)) )
    return std::make_pair(0.0,1.01);
  double fRatio ((numSS * denDF)/(denSS * numDF));                      
#ifndef NOGSL
      double p = gsl_cdf_fdist_Q(fRatio, numDF, denDF);    
#else
      double p = exp(-2*fRatio);  // wow!  THis is crappy
#endif
      return std::make_pair(fRatio, p);
}



