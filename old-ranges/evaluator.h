// $Id: evaluator.h,v 1.2 2003/03/21 04:19:34 bob Exp $

/*
  The evaluator class implements the operator policy, but with
  operator() implemented in a 'reversed' order that applies the
  argument to the function.  That is, treat the argument x as an
  operator on f.

  As a result, the operator associated with a range can be a 'point' x
  and the range consist of functions f_1,...f_n. When the range is
  evaluated, one obtains a 'function range' f_1(x),...,f_n(x).

  It is parameterized (with a template) over the associated function
  class F in order to know the result type.
*/

#ifndef EVALUATOR_H
#define EVALUATOR_H

#include <functional>


template <class F>
class evaluator : public std::unary_function<F, typename F::result_type>
{
  typename F::argument_type mX;

public:
  evaluator(typename F::argument_type x)
    : mX(x) { }
  
  typename F::result_type
  operator()(const F& f)  { return f(mX); } // Should this be "const F&" or "F" ??
};

template <class F>
class make_evaluator : public std::unary_function<typename F::argument_type, evaluator<F> >
{
public:
  
  evaluator<F>
  operator()(const typename F::argument_type& x)
  {
    return evaluator<F>(x);
  }
};

#endif
