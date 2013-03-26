#ifndef LINE_SEARCH_TEMPLATE_H
#define LINE_SEARCH_TEMPLATE_H

#include "line_search.h"
#include "math.h"
#include <iostream>


template <class Vec>
double
Line_Search::invert_monotone(double y, int i0, Vec const& yVec)
{
  if (yVec[0] < yVec[1])
    return Line_Search::invert_monotone(y,i0,yVec,std::greater<double>());
  else
    return Line_Search::invert_monotone(y,i0,yVec,std::less<double>());
}

template <class Vec, class Comp>
  double
  Line_Search::invert_monotone(double y, int i0, Vec const& yVec, Comp const& comp)
{
  int last (yVec.size()-1);
  if (comp(yVec[0], y) || comp(y, yVec[last]))
  { std::cerr << "LINE_SEARCH: " << " y=" << y << " out of bounds in vector "
	      << yVec[0] << " to " << yVec[last] << std::endl;
    return 0;
  }
  while (!comp(yVec[i0],y))  // has to be an i0 which is eventually less... should make go faster with bisection
    ++i0;
  double lo (yVec[i0-1]);
  double hi (yVec[i0  ]);
  return (i0-1)+(y-lo)/(hi-lo);
}


template <class F>
double
Line_Search::Bisection::operator()(F const& f) const
{
  // check signs first
  double left  (f(mInterval.first));
  double right (f(mInterval.second));
  if (left*right > 0)
  { std::cerr << "LINE_SEARCH: Error. Zero of input function not on this range; f("
	      << mInterval.first << ")=" << left << "   f(" << mInterval.second << ")=" << right << std::endl;
    return 0;
  }
  else if (left < right)               // monotone increasing
    return find_zero(f,std::greater<double>());
  else                                          // decreasing
    return find_zero(f,std::less<double>());
}


template <class F, class Comp>
double
Line_Search::Bisection::find_zero (F const& f, Comp const& comp) const   // comp is comparison func
{
  Line_Search::Pair interval  (mInterval);
  Line_Search::Pair fInterval (std::make_pair(f(mInterval.first),f(mInterval.second)));
  double            eps       (length(interval));
  double            mp;
  while (eps > mTolerance)
  { //std::cout << "BISECT: On ["<<interval.first<< ","<<interval.second<<"], f=["<<fInterval.first<< "," << fInterval.second << "]\n";
    mp = midpoint(interval);
    double fx (f(mp));
    if (comp(fx,0))
    { interval.second= mp; fInterval.second=fx; }
    else
    { interval.first = mp; fInterval.first =fx; }
    eps /= 2;
  }
  return midpoint(interval);
}
  

#define MAX(A,B) (((A) < (B))? (B) : (A))
#define MIN(A,B) (((A) < (B))? (A) : (B))

template< class Func, class Comp >
Line_Search::Pair
Line_Search::GoldenSection::optimize(Func const& f, Comp const& comp) const
{
  const double GR = 2.0/(1 + sqrt(5.0));   // 0.618
  const double gr = 1.0 - GR;              // 0.382
  
  // build starting conditions
  Pair   xLo = std::make_pair(mInterval.first,  f(mInterval.first));
  Pair   xHi = std::make_pair(mInterval.second, f(mInterval.second));
  // perform optional initial grid search
  if (mGridSize > 0)
  { int   xOpt = xLo.first;
    double opt = xLo.second;
    int n = ceil((mInterval.second - mInterval.first)/mGridSize)-1;
    double x=mInterval.first+mGridSize;
    for (int i=0; i<n; ++i, x += mGridSize)
    { x += mGridSize;
      double fx = f(x);
      if (comp(fx,opt)) { xOpt = x; opt = fx; }
    }
    // update range
    xLo.first  = MAX(xLo.first, xOpt - 2 * mGridSize);
    xLo.second = f(xLo.first);
    xHi.first  = MIN(xHi.first, xOpt + 2 * mGridSize);
    xHi.second = f(xHi.first);
  }
  // init the internal positions
  /* std::cout << "\n\nLINE: Searching interval  "
  	      << "<" << xLo.first << "," <<xLo.second <<  "> ... " 
  	      << "<" << xHi.first << "," <<xHi.second <<  "> " << std::endl; */

  double len = xHi.first-xLo.first;
  double x = xLo.first + gr*len;
  Pair xA = std::make_pair(x, f(x));
  x = xLo.first + GR*len;
  Pair xB = std::make_pair(x, f(x));
  // find new interval
  int i (0);
  while(++i < mMaxIterations)
  { len = xB.first - xLo.first;              // 0.618 times prior length
    if (len < mTolerance) break;
    // use to trace optimization
    /*    std::clog << "LINE:  <" << xLo.first << "," << xLo.second << "> "
	      <<        "<" <<  xA.first << "," << xA.second <<  "> "
	      <<        "<" <<  xB.first << "," << xB.second <<  "> "
	      <<        "<" << xHi.first << "," <<xHi.second <<  "> " << std::endl; */
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
    std::clog << "GSRC:  Warning.  Golden section reached maximum number of iterations.\n";
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

#endif
