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


// First iterator gives the names of the elements, next two give the estimates and SEs

template <class SIter, class Iter>
void
  print_stat_summary_table (int k, SIter name, Iter est, Iter se, std::ostream &os)
{
  const unsigned int maxString (25);

  os << "\n - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n";
  os << "                         Summary of Estimates\n";
  os << "        Name                   Estimate            SE          t         p\n";
  // print the names in the first column, then est   se   t  p-val
  for (int i=0; i<k; ++i)
  { std::string aName (name[i]);
    if (aName.size() > maxString)
      aName = aName.substr(0,maxString);
    os << std::setw(25) << aName  << "   ";
    os << std::setw(15) << est[i] << " ";
    // reduce precision for se, t values, p-values
    os.precision(3);
    // skip over rest of elements if  se <= 0
    if (se[i] > 0)
    { os << std::setw(10) <<    se[i]      << " ";
      // round z to 2 decimals
      double z (est[i]/se[i]);
      int zInt (100 * z);
      os << std::setw(10) << double(zInt)/100.0  << " ";
      // output 2-sided z-values
      if (z < 0) z = -z;
      double p (2.0*gsl_cdf_ugaussian_Q(z));
      if (p < 0.00001) p = 0;
      os << std::setw(10) << p;
    }
    os << std::endl;
    os.precision(6);
  }
  os << " - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n\n";
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
