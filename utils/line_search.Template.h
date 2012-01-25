#include "line_search.h"
#include "math.h"
#include <iostream>


template< class Func, class Comp >
Line_Search::Pair
Line_Search::GoldenSection::optimize(Func const& f, Comp const& comp ) const
{
  const double GR = 2.0/(1 + sqrt(5.0));   // 0.618
  const double gr = 1.0 - GR;              // 0.382
  double x;
  
  // build starting conditions
  double len = mInterval.second-mInterval.first;
  Pair   xLo = std::make_pair(mInterval.first,  f(mInterval.first));
  Pair   xHi = std::make_pair(mInterval.second, f(mInterval.second));
  // init the internal positions
  x = mInterval.first + gr*len;
  Pair xA = std::make_pair(x, f(x));
  x = mInterval.first + GR*len;
  Pair xB = std::make_pair(x, f(x));
  // find new interval
  int i (0);
  while(++i < mMaxIterations)
  { len = xB.first - xLo.first;              // 0.618 times prior length
    if (len < mTolerance) break;
    /* use to trace optimization
      std::clog << "GSRC:  <" << xLo.first << "," << xLo.second << "> "
	      <<        "<" <<  xA.first << "," << xA.second <<  "> "
	      <<        "<" <<  xB.first << "," << xB.second <<  "> "
	      <<        "<" << xHi.first << "," <<xHi.second <<  "> " << std::endl;
    */
    if(comp(xA.second,xB.second)) // < for min
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
  if (mMaxIterations == i)
    std::cout << "GSRC:  Warning.  Golden section reached maximum number of iterations.\n";
  x = 0.5 * (xLo.first + xHi.first);
  return(std::make_pair(x,f(x)));
}


template< class Func >
Line_Search::Pair
Line_Search::GoldenSection::find_minimum(Func const& f) const
{
  return optimize(f, std::less<double> () );
}


template< class Func >
Line_Search::Pair
Line_Search::GoldenSection::find_maximum(Func const& f) const
{
  return optimize(f, std::greater<double>());
}
