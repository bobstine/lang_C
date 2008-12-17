#include <boost/lambda/lambda.hpp>
#include <boost/function.hpp>                     // need to store the lambda function
#include <boost/type_traits/function_traits.hpp>

#include <algorithm>

#include <vector>
#include <iostream>
#include <functional>

////
using namespace boost::lambda;
////


////////////////////   Function Result Type

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
class function_result_type< lambda_functor<lambda_functor_base<Action,Args> > >
{
public:
  typedef typename lambda_functor_base<Action,Args>::template sig<Args>::type type;
};



//  This version digs directly into the lambda function 

template <class Act, class Args>
typename lambda_functor_base<Act,Args>::template sig<Args>::type
original_example_return(lambda_functor< lambda_functor_base<Act,Args> > f)
{
  return f(2.2,3.3);
}

//  This version uses the result type metafunction

template <class F>
typename function_result_type<F>::type
example_return(F f)
{
  return f(2.2,3.3);
}



class func : public std::binary_function<double,double,double>
{
public:
  double operator()(double x, double y) const { return x + y; }
};


int main()
{
  // simple use of lambda function with a collection
  std::vector<double> x(10);
  
  std::for_each(x.begin(), x.end(), _1 = 10);
  std::for_each(x.begin(), x.end(), std::cout << _1 << " ");
  std::cout << std::endl;
  
  // store and then apply a lambda function requires a function wrapper for convenience
  // when defining the appropriate type, but introduces inefficiency in indirection 
  boost::function<double(double, double)> f = _1 + _2;
  std::cout << "TEST: directly apply function: " << f(1.1,2.2) << std::endl;


  
  // now get traits from a lambda function ... 

  double result = example_return(ret<double>(_1+_2));
  
  std::cout << "TEST: indirect function call : " << result << std::endl;
  std::cout << "                               " << example_return(ret<double>(_1+_2)) << std::endl;
  std::cout << "                               " << (_1 + _2)(2.2,3.3) << std::endl;
  std::cout << "                               " << example_return(func()) << std::endl;

  
  
  std::cout << "\nDone.\n";
}
