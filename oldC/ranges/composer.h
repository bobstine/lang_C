// $Id: composer.h,v 1.3 2003/04/15 21:11:12 bob Exp $

#include "compose.h"

#ifndef _COMPOSER_H_
#define _COMPOSER_H_

/*
  The composer class builds a function looking to be applied to
  another function.  Consider the composition of two functions,
  written as (f o g). Evaluation at x (in the domain of g) returns
  f(g(x)).  A composer object holds (f o _).  Its operator() function
  returns the composition (f o g).


  20 Mar 03 ... Cut from old mixed up range code.
  
*/

template<class F>
class Composer
{
  F mf;

public:
  typedef typename F::result_type result_type;
  
  Composer(const F& f) : mf(f) { }

  template<class G>
  boost::compose_f_gx_t<F,G>  operator()(const G& g) const
  { return boost::compose_f_gx_t<F,G>(mf,g); }
};

template<class F>
Composer<F> make_composer(const F& f) 
{ return Composer<F>(f); }


template<class F, class G>
class operator_traits {
  typedef typename F::result_type result_type;
};

template<class F, class G1, class G2>
class binary_operator_traits {
  typedef typename F::result_type result_type;
};

template<class F, class G>
class operator_traits <Composer<F>,G> {
  typedef typename boost::compose_f_gx_t<F,G> result_type;
};

#endif
