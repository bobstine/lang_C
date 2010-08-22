/*  $Id: print_utils.h,v 1.8 2008/01/16 22:51:52 bob Exp $
    
  Utility functions

  23 Mar 02 ... Update to GCC 3 with std:: prefixes.
  10 Feb 02 ... Need to resolve how to print pairs vs print ranges.
  14 Dec 01 ... Created to implement read/write of vectors.
  
*/

#ifndef _PRINT_UTILS_H_
#define _PRINT_UTILS_H_

#include <iostream>
#include <iomanip>
#include <iterator>
#include <vector>
 
// p-value for tabular summary
#ifndef NOGSL
#include <gsl/gsl_cdf.h>
#else
#include <math.h>
#endif

////////////////////////  Stat Tabular Summary  //////////////////


// First iterator gives the names of the elements, next two give the estimates and SEs

template <class SIter, class Iter>
void
  print_stat_summary_table (int k, SIter name, Iter est, Iter se, std::ostream &os)
{
  const unsigned int maxNameString (50);

  os << "\n - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n";
  os << "                                     Summary of Regression Coefficient Estimates\n";
  os << "                       Predictor Name                    Estimate        SE         t        p\n";
  // print the names in the first column, then est   se   t  p-val
  for (int i=0; i<k; ++i)
  { std::string aName (name[i]);
    if (aName.size() > maxNameString)
      aName = aName.substr(0,maxNameString);
    os << std::setw(maxNameString) << aName  << "   ";
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
      os << std::setiosflags(std::ios::left);
      if (z < 0) z = -z;
#ifndef NOGSL
      double p (2.0*gsl_cdf_ugaussian_Q(z));
#else
      double p = exp(-2*exp(2 * log(z)));
#endif
      if (p < 0.0000001) p = 0.0;
      os << std::setw(10)  << p;
      os << std::setiosflags(std::ios::right);
    }
    os << std::endl;
    os.precision(6);
  }
  os << " - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n\n";
}

template <class SIter, class Iter>
void
  print_stat_summary_table_in_html (int k, SIter name, Iter est, Iter se, std::ostream &os)
{
  const unsigned int maxNameLength (50);

  // find the length of the longest name; use that length unless bigger than max
  unsigned int maxLen = 0;
  for (int i=0; i<k; ++i)
  { if (name[i].size() > maxLen)
      maxLen = name[i].size();
  }
  if (maxLen > maxNameLength)
    maxLen = maxNameLength;
  
  os << "\n<TABLE BORDER CELLSPACING=10 CELLPADDING=10 WIDTH=\"80%\">\n";
  os << " <CAPTION> Summary of Statistical Estimates </CAPTION>\n";
  os << "<TR> <TH SCOPE=\"column\"> Name </TH> "
     << "<TH SCOPE=\"column\"> Estimate </TH>"
     << "<TH SCOPE=\"column\"> Std Error </TH>"
     << "<TH SCOPE=\"column\"> t-statistic </TH>"
     << "<TH SCOPE=\"column\"> p-value </TH>  </TR>\n";
    
  // print the names in the first column, then est   se   t  p-val
  for (int i=0; i<k; ++i, ++name,++est,++se)
  { // trim names to allowed size
    os << "<TR> ";
    std::string aName (*name);
    if (aName.size() > maxLen)
      aName = aName.substr(0,maxLen);
    os <<  "<TD>" << aName << "</TD> ";
    os <<  "<TD>" << *est  << "</TD> ";
    // reduce precision for se, t values, p-values
    os.precision(3);
    // skip over rest of elements if  se <= 0
    if (*se <= 0)
      os <<  "<TD> na </TD>   <TD> </TD>    <TD> </TD>";
    else
    { os <<  "<TD>" << *se  << "</TD> ";
      // round z to 2 decimals
      double z (*est / *se);
      int zInt (100 * z);
      os <<  "<TD>" << double(zInt)/100.  << "</TD> ";
      // output p-values
      if (z < 0) z = -z;
#ifndef NOGSL
      double p (2.0*gsl_cdf_ugaussian_Q(z));
#else
      double p = exp(-2*exp(2 * log(z)));
#endif
      if (p < 0.0000001) p = 0.0;
      os <<  "<TD>" << p  << "</TD>";
    }
    os << "</TR>\n";
    os.precision(6);
  }
  os << "</TABLE>\n";
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
  int n ((int) vec.size());
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
