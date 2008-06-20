/*  $Id: print_utils.h,v 1.8 2008/01/16 22:51:52 bob Exp $
    
  Utility functions

  23 Mar 02 ... Update to GCC 3 with std:: prefixes.
  10 Feb 02 ... Need to resolve how to print pairs vs print ranges.
  14 Dec 01 ... Created to implement read/write of vectors.
  
*/

#ifndef _PRINT_UTILS_H_
#define _PRINT_UTILS_H_

#include <iostream>
#include <iterator>
#include <vector>
#include <iomanip>

// p-value for tabular summary
#include <gsl/gsl_cdf.h>


////////////////////////  Stat Tabular Summary  //////////////////

template <class Iter>
void
print_stat_summary_table (int k, Iter est, Iter se, std::ostream &os)
{
  os << "      Estimates :  ";
  
  double y (*est);
  
  for (int i=0; i<k; ++i) 
  { double x (est.operator[](i));
    os << std::setw(10) << x << " ";
  }
  os << std::endl;
  // reduce precision for se, t values, p-values
  os.precision(3);
  // skip over first element if first se is 0
  int i0 (0);
  std::string padding ("");
  if (*se == 0) {
    i0 = 1; 
    padding = "           "; }
  // output SE
  os << "        SE      :  " << padding ;
  for (int i=i0; i<k; ++i) os << std::setw(10) << se[i]  << " ";
  os << std::endl;
  // output t stats
  os << "         t      :  " << padding;
  for (int i=i0; i<k; ++i) os << std::setw(10) << est[i]/se[i]  << " ";
  os << std::endl;
  // output 2-sided z-values
  os << "         p      :  " << padding;
  for (int i=i0; i<k; ++i) 
  { double z ((est[i] > 0.0) ? est[i]/se[i] : -est[i]/se[i]);
    os << std::setw(10) << (2.0*gsl_cdf_ugaussian_Q(z)) << " ";  
  }
  os << std::endl;
  os.precision(6);
}

////////////////////////  Line Printer  /////////////////////

template<class T>
class LinePrinter {
  std::ostream& mOutput;
 public:
  LinePrinter(std::ostream &output)
    : mOutput(output) { }
  void
    operator()(const T x) { mOutput << x << std::endl; }
};

template<class T>
class Printer {
  std::ostream& mOutput;
 public:
  Printer(std::ostream &output)
    : mOutput(output) { }
  void
    operator()(const T x) { mOutput << x << " "; }
};

////////////////////////  Pair output  //////////////////////

template<class T1, class T2>
inline std::ostream&
operator<<(std::ostream& output, const std::pair<T1,T2> p)
{
  output << " <" << p.first << "," << p.second << "> ";
  return output;
}



////////////////////////  Vector output  //////////////////////

template<class T>
inline std::ostream&
operator<<(std::ostream& ostrm, const std::vector<T>& vec)
{
  int n (vec.size());
  if (n == 0)
    ostrm << " [empty] " << std::endl;
  else
    {
      ostrm << "{";
      if (n > 4) ostrm << "[" << n << "]";
      std::copy(vec.begin(),vec.end(),std::ostream_iterator<T>(ostrm," "));
      ostrm << "}";
    }
  return ostrm;
}

template<class T1, class T2>
std::ostream&
print_pair(std::ostream& output, const std::pair<T1, T2> p)
{
  output << "<" << p.first << "," << p.second << "> ";
  return output;
}

#endif
