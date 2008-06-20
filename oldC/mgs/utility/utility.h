/*  $Id: utility.h,v 1.2 2002/06/16 15:41:14 bob Exp $
    
  Utility functions

  23 Mar 02 ... Update to GCC 3 with std:: prefixes.
  10 Feb 02 ... Need to resolve how to print pairs vs print ranges.
  14 Dec 01 ... Created to implement read/write of vectors.
  
*/

#ifndef _UTILITY_H_
#define _UTILITY_H_

#include <iostream>
#include <vector>

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
  output << "<" << p.first << "," << p.second << "> ";
  return output;
}

////////////////////////  Vector output  //////////////////////

template<class T>
inline std::ostream&
operator<<(std::ostream& ostrm, const std::vector<T>& vec)
{
  if (vec.size() == 0)
    ostrm << " [empty] " << std::endl;
  else
    {
      ostrm << "[" << vec.size() << "] {";
      copy(vec.begin(),vec.end(),std::ostream_iterator<T>(ostrm," "));
      ostrm << "}" << endl;
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


std::istream&
operator>>(std::istream& input, std::vector<double>& vec);


/////////////////////  Mean and SD summary  ////////////////

std::pair<double,double>
mands (const std::vector<double>& x);

#endif
