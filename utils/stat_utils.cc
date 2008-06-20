#include "stat_utils.h"

#include <numeric>  // for accumulate
#include <math.h>   // for sqrt
#include <assert.h>


// for distributions
#include <gsl/gsl_sf_erf.h>
#include <gsl/gsl_cdf.h>


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
mands (const std::vector<double>& x)
{
  assert (x.size() > 1);
  double xBar = accumulate(x.begin(), x.end(), 0.0)/x.size();
  return std::make_pair(xBar, standard_deviation(x, xBar));
}

double
standard_deviation (std::vector<double> const& x, double avg)
{
  double ss = std::accumulate(x.begin(), x.end(), 0.0, CenterSquare(avg));
  assert (ss >= 0);
  return sqrt(ss/(x.size()-1));
}

////  Tests and p-values


std::pair<double,double>
t_test(double tStat, int df)
{
  return std::make_pair(tStat, gsl_cdf_tdist_Q(tStat, df));    
}

std::pair<double,double> 
f_test(double numSS, int numDF, double denSS, int denDF)
{
  if((numSS <= 0.0) || (denSS <= 0.0)) return std::make_pair(0.0,1.0);
  
  double fRatio ((numSS * denDF)/(denSS * numDF));                      
  return std::make_pair(fRatio, gsl_cdf_fdist_Q(fRatio, numDF, denDF));    
}



