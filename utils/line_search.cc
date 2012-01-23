#include "line_search.h"
#include "math.h"


#include <iostream>   // only for debugging


Line_Search::double_pair
Line_Search::GoldenSection::operator()(scalar_function const& f, Line_Search::double_pair const& interval) const
{
  std::clog << "GSRC: In operator search\n";
  return std::make_pair(0.0, 0.0);
}



Line_Search::double_pair
Line_Search::GoldenSection::operator()(double (*f)(double), Line_Search::double_pair const& interval) const
{
  std::clog << "GSRC: In function pointer search\n";

  typedef std::pair<double,double> Pair;

  const double eps = .0001;                // tolerance
  const int    maxIt = 100;                
  const double GR = 2.0/(1 + sqrt(5.0));   // 0.618
  const double gr = 1.0 - GR;              // 0.382
  double x;
  
  // build starting conditions
  Pair   xLo = std::make_pair(interval.first,  f(interval.first));
  Pair   xHi = std::make_pair(interval.second, f(interval.second));
  double len = interval.second-interval.first;

  x = interval.first + gr*len;
  Pair xA = std::make_pair(x, f(x));
  x = interval.first + GR*len;
  Pair xB = std::make_pair(x, f(x));
  
  // find new interval
  int i (0);
  while(++i < maxIt)
  { len = xB.first - xLo.first;       // 0.618 times prior length
    if (len < eps) break;
    std::clog << "GSRC:  <" << xLo.first << "," << xLo.second << "> "
	      <<        "<" <<  xA.first << "," << xA.second <<  "> "
	      <<        "<" <<  xB.first << "," << xB.second <<  "> "
	      <<        "<" << xHi.first << "," <<xHi.second <<  "> " << std::endl;
    if(xA.second < xB.second)
    { xHi = xB;
      xB=xA;
      x = xLo.first + gr*len;
      xA = std::make_pair(x, f(x));
    }
    else
    { xLo = xA;
      xA = xB;
      x = xLo.first + GR*len;
      xB = std::make_pair(x,f(x)); 
    }
  }
  x = 0.5 * (xLo.first + xHi.first);
  return(std::make_pair(x,f(x)));
}

