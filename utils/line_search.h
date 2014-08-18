/* -*-c++-*-

   20 Nov 12 ... Add invert_monotone to find x such that f[x] = y
   24 May 12 ... Add bisection for finding zero.
   23 Jan 12 ... Define.  First code in long time.

*/


#ifndef _LINE_SEARCH_H_
#define _LINE_SEARCH_H_

#include <functional>
#include <utility>


namespace Line_Search
{
  typedef std::pair<double,double>           Pair;
  typedef std::unary_function<double,double> scalar_function;
  
  /*    invert_monotone     invert_monotone     invert_monotone     invert_monotone

	find value x such that f[x]=w assuming f as given in vector is monotone
  */

  template <class Vec>
    double
    invert_monotone(double y, int i0, Vec const& yVec);
  
  template <class Vec, class Comp>
    double
    invert_monotone(double y, int i0, Vec const& yVec, Comp const& comp);
  

  //     bisection     bisection     bisection     bisection     bisection     bisection     bisection     bisection     
  class Bisection        // finds zero
  {
  private:
    const double mTolerance;
    const Pair   mInterval;
    
  public:
    Bisection (double tolerance, Pair const& interval)
      : mTolerance (tolerance), mInterval(interval) { }

    template <class F>
      double operator()(F const& f) const;

  private:
    double length  (Pair const& p) const { return p.second-p.first; }
    double midpoint(Pair const& p) const { return (p.second+p.first)/2; }
    template <class F, class Comp>
      double find_zero (F const& f, Comp const& comp) const;   // comp is comparison func
  };


  
  //     golden section     golden section     golden section     golden section     golden section     golden section
  /*
    The initial grid search may be particularly useful if the function
    is not globally unimodal.  Use the line search to find a starting
    unimodal domain.
  */
  
  class GoldenSection
  {
  private:
    const double  mTolerance;
    const double  mGridSpacing;      // do initial grid search at this spacing  (0 implies none)
    const Pair    mInterval;
    const int     mMaxIterations;
    
  public:

    GoldenSection ()
      : mTolerance(0), mGridSpacing(0), mInterval(std::make_pair(0,0)), mMaxIterations(0) { }
    
    GoldenSection (GoldenSection const& gs)
      : mTolerance(gs.mTolerance), mGridSpacing(gs.mGridSpacing), mInterval(gs.mInterval), mMaxIterations(gs.mMaxIterations) { }
    
    GoldenSection (double tolerance, Pair const& interval, double grid, int maxIterations)
      : mTolerance(tolerance), mGridSpacing (grid), mInterval(interval), mMaxIterations(maxIterations) { }

    GoldenSection (double tolerance, Pair const& interval)
      : mTolerance(tolerance), mGridSpacing (   0), mInterval(interval), mMaxIterations(   100       ) { }

    template< class Func >
      Pair find_minimum(Func const& f) const;

    template< class Func >
      Pair find_maximum(Func const& f) const;

  private:
    
    template< class Func, class Comp >
      Pair optimize(Func const& f, Comp const& c) const;   // comp is comparison function
    
  };

  
}  // namespace


#endif
