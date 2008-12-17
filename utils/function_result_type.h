/*
  Metafunction that digs out the return type from a function class.

  16 Dec 2008   Define to be able to use lambda functions in our range code.
  
*/
#ifndef FUNCTION_RESULT_TYPE_H
#define FUNCTION_RESULT_TYPE_H


#include <boost/lambda/lambda.hpp>


template <class F>
class function_result_type
{
 public:
  typedef typename F::result_type type;
};


template <class X, class Y>
  class function_result_type< std::unary_function<X,Y> >
{
 public:
  typedef Y type;
};


template <class X1, class X2, class Y>
  class function_result_type< std::binary_function<X1,X2,Y> >
{
 public:
  typedef Y type;
};


template <class Action, class Args>
  class function_result_type< boost::lambda::lambda_functor<boost::lambda::lambda_functor_base<Action,Args> > >
{
 public:
  typedef typename boost::lambda::lambda_functor_base<Action,Args>::template sig<Args>::type type;
};

#endif
