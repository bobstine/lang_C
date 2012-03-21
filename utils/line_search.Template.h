#include "line_search.h"
#include "math.h"
#include <iostream>


template< class Func, class Comp >
Line_Search::Pair
Line_Search::GoldenSection::optimize(Func const& f, Comp const& comp) const
{
  const double GR = 2.0/(1 + sqrt(5.0));   // 0.618
  const double gr = 1.0 - GR;              // 0.382
  double x;
  
  // build starting conditions
  Pair   xLo = std::make_pair(mInterval.first,  f(mInterval.first));
  Pair   xHi = std::make_pair(mInterval.second, f(mInterval.second));
  // perform optional initial grid search
  if (mGridSize > 0)
  { int iOpt = 0;
    double opt = xLo.second;
    int n = ceil((mInterval.second - mInterval.first)/mGridSize)-1;
    double *pGrid  = new double[n];
    double *pF     = new double[n];
    x = mInterval.first;
    for (int i=0; i<n; ++i)
    { x += mGridSize;
      pGrid[i] = x;
      pF[i] = f(x);
      if (comp(pF[i],opt)) {iOpt = i; opt = pF[i]; }
    }
    if(comp(xHi.second,opt)) iOpt = n;
    if (0 == iOpt || 1 == iOpt)
    { xHi.first=pGrid[1]; xHi.second=pF[1]; }
    else if (n == iOpt || (n-1 == iOpt))
    { xLo.first=pGrid[n-1]; xLo.second=pF[n-1]; }
    else
    { xLo.first=pGrid[iOpt-1]; xLo.second=pF[iOpt-1];
      xHi.first=pGrid[iOpt+1]; xHi.second=pF[iOpt+1];
    }
    delete [] pGrid;
    delete [] pF;
  }
  // init the internal positions
  /* std::cout << "\n\n Searching interval  "
  	      << "<" << xLo.first << "," <<xLo.second <<  "> ... " 
  	      << "<" << xHi.first << "," <<xHi.second <<  "> " << std::endl;
  */
  double len = xHi.first-xLo.first;
  x = xLo.first + gr*len;
  Pair xA = std::make_pair(x, f(x));
  x = xLo.first + GR*len;
  Pair xB = std::make_pair(x, f(x));
  // find new interval
  int i (0);
  while(++i < mMaxIterations)
  { len = xB.first - xLo.first;              // 0.618 times prior length
    if (len < mTolerance) break;
    // use to trace optimization
    /*    std::clog << "GSRC:  <" << xLo.first << "," << xLo.second << "> "
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
