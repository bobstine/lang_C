#ifndef _STAT_UTILS_H_
#define _STAT_UTILS_H_

#include <vector>

/////////////////////  Mean and SD summary  ////////////////

namespace Stat_Utils {

  double                    standard_deviation (std::vector<double> const& x, double avg);
  
  std::pair<double,double>  mands (std::vector<double> const& x); 
  
  // p-values
  
  std::pair<double,double> t_test(double tStat, int df);
  
  std::pair<double,double> f_test(double f,     int numDF,               int denDF); 
  std::pair<double,double> f_test(double numSS, int numDF, double denSS, int denDF); 
}

#endif
