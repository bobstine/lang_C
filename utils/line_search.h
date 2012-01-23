
/*
  23 Jan 12 ... Define.  First code in long time.
*/


#ifndef _LINE_SEARCH_H_
#define _LINE_SEARCH_H_

#include <functional>

namespace Line_Search
{
  typedef std::pair<double,double>           double_pair;
  typedef std::unary_function<double,double> scalar_function;
  
  class GoldenSection   // finds minimum
    {
    public:
      double_pair  operator()(scalar_function const& f, double_pair const& interval) const;
      double_pair  operator()(double (*f)(double), double_pair const& interval) const;
    };
  
}



#endif
