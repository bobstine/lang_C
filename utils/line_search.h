/*
  23 Jan 12 ... Define.  First code in long time.
*/


#ifndef _LINE_SEARCH_H_
#define _LINE_SEARCH_H_

#include <functional>

namespace Line_Search
{
  typedef std::pair<double,double>           Pair;
  typedef std::unary_function<double,double> scalar_function;

  
  class GoldenSection   // finds minimum
  {
  private:
    const double  mTolerance;
    const Pair    mInterval;
    const int     mMaxIterations;
    
  public:

    GoldenSection (double tolerance, Pair const& interval, int maxIterations=100)
      : mTolerance(tolerance), mInterval(interval), mMaxIterations(maxIterations) { }

    // returns x and min(f(x))
    //    Pair  operator()(double (*f)(double))      const;    use ptr_to_unary_function

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
