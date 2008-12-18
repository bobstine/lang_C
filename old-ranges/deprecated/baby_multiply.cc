// $Id: baby_multiply.cc,v 1.1 2003/11/26 14:11:50 bob Exp $ -*- c++ -*-

// this compiles using: g++ baby_multiply.cc; a.out

#include <vector>
#include <iostream>
#include "range_traits.h"
#include "range_ops.h"
#include "compose.h"
#include "range.h"

template<class op, class T1, class T2>
class result_computer
{
};

template<class op>
class result_computer<op,double, double>
{
public:
  typedef double first_argument;
  typedef double second_argument;
  typedef op result;

  typename result::result_type
  operator()(first_argument x, second_argument y)
  {
    return op(x,y);
  }
  
};

template<class op, class Iter1, class Iter2>
class result_computer<op,range_class<Iter1>, range_class<Iter2> >
{
public:
  typedef range_class<Iter1> first_argument;
  typedef range_class<Iter2> second_argument;

  typedef typename std::iterator_traits<Iter1>::value_type first_value_type;
  typedef typename std::iterator_traits<Iter2>::value_type second_value_type;
  typedef typename result_computer<op, first_value_type, second_value_type>::result value;

  typedef typename binary_range_traits<value,first_argument,second_argument>::range result;

  result
  operator()(first_argument x, second_argument y)
  {
    return make_binary_range(value(), x, y);
  }
};


template<class op, class value, class Iter2>
class result_computer<op, value, range_class<Iter2> >
{
public:
  typedef value              first_argument;
  typedef range_class<Iter2> second_argument;

  typedef value                                            first_value_type;
  typedef typename std::iterator_traits<Iter2>::value_type second_value_type;
  typedef typename result_computer<op, first_value_type, second_value_type>::result value;

  typedef typename unary_range_traits<value,second_argument>::range result;

  result
  operator()(first_argument x, second_argument y)
  {
    std::cout << "op" << std::endl;
    return make_unary_range(value(), x, y);
  }
};


// the following don't restrict their domain to range_class's.  Hence
// they are more appropiate to put in the APL domain.


template <class LHS_type, class RHS_type>
inline
typename result_computer<std::multiplies<double>,LHS_type,RHS_type>::result
operator*(LHS_type x, RHS_type y)
{
  return result_computer<std::multiplies<double>,LHS_type,RHS_type>()(x,y);
};

template <class LHS_type, class RHS_type>
inline
typename result_computer<std::plus<double>,LHS_type,RHS_type>::result
operator+(LHS_type x, RHS_type y)
{
  return result_computer<std::plus<double>,LHS_type,RHS_type>()(x,y);
};


int
main()
{
  std::vector<double> x(5);
  std::vector<double> y(5,10);
  x[0] = 0;
  x[1] = 1;
  x[2] = 2;
  x[3] = 3;
  x[4] = 4;
  make_range(x);
  std::cout << make_range(x) * make_range(y) << std::endl; 
  std::cout << 3. * make_range(x)  << std::endl; 
  std::cout << 3. * make_range(x) + make_range(y) << std::endl; 
  //  std::cout << 2. + 3. * make_range(x) + make_range(y) << std::endl; 
}
