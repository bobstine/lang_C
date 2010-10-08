#ifndef _STAT_UTILS_H_
#define _STAT_UTILS_H_

#include <vector>
#include <math.h>

/////////////////////  Mean and SD summary  ////////////////

namespace Stat_Utils {

  double                    standard_deviation (std::vector<double> const& x, double avg);
  
  std::pair<double,double>                       mands (std::vector<double> const& x); 
  template<class Iter> std::pair<double,double>  mands (Iter b, Iter const& e, int n); 
  
  // p-values
  
  std::pair<double,double> t_test(double tStat, int df);

  std::pair<double,double> f_test(double f,     int numDF,               int denDF); 
  std::pair<double,double> f_test(double numSS, int numDF, double denSS, int denDF); 

}


////////////  Template.h //////////////

template<class Iter>
std::pair<double,double>
  Stat_Utils::mands (Iter b, Iter const& e, int n)
{ double xBar = accumulate(b, e, 0.0)/n;
  double ss (0.0);
  for (int i=0; i<n; ++i)
  { double dev = *b - xBar;
    ss += dev * dev;
  }
  return std::make_pair(xBar, sqrt(ss/(n-1)));
}

#endif
