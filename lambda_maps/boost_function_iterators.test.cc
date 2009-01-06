// $Id: function_iterators.test.cc,v 1.11 2004/08/25 14:47:08 bob Exp $    

#include "function_utils.h"
#include "function_iterators.h"
#include "function_result_type.h"
#include "range_traits.h"
#include "range.h"

#include "composer.h"
#include "evaluator.h"

#include <boost/lambda/lambda.hpp>
#include <boost/lambda/bind.hpp>
#include <boost/tuple/tuple.hpp>

////
using namespace boost::lambda;
////

#include <iostream>
#include <vector>
#include <list>
#include <string>
#include <algorithm>
#include <utility>
#include <numeric>
#include <functional>


class Employee  {
  std::string mName;
  double mSalary;
public:
  Employee (std::string name, double salary)
    : mName(name), mSalary(salary) { }
  std::string name() const { return mName; }
  double salary() const { return mSalary; }
};

std::ostream&
operator<<(std::ostream& os, const Employee& emp) 
{
  os << "Employee " << emp.name() << " makes " << emp.salary() << std::endl;
  return os;
}


class Operator : public std::unary_function<double,double> 
{
  double mX;
public:
  Operator(double x)
    : mX(x) { }
  
  double operator()(double x) const
  { return x + mX; }
  
  // virtual
  Operator& operator=(const Operator& op)
  { this->mX = op.mX+1; return *this; }
};


class Simple_square : public std::unary_function<double,double>
{
 public:
   double operator()(double x){return x * x;}
};

class Shifter : public std::unary_function<double,double>
{
  double mShift;
  bool my_true;      // used in testing
  bool my_false;
  
public:
  ~Shifter();
  Shifter(double s);
  Shifter(const Shifter& s);
  double get_shift() const;
  double operator()(const double x) const;
  void operator=(const Shifter& s);  // RAS : made public so push_back would work
private:
  Shifter();
};

Shifter::~Shifter()
{
#ifndef NDEBUG
  std::cout << "\t\t\t > Destroying shifter " << this << " with " << mShift << std::endl;
#endif
}

Shifter::Shifter(double s)
  : mShift(s),
    my_true(true),
    my_false(false)
{
#ifndef NDEBUG
  std::cout << "\t\t\t < Init shifter " << this << " with " << s << std::endl;
#endif
}

Shifter::Shifter(const Shifter& s)
  : mShift(s.get_shift()),
     my_true(true),
     my_false(false)
   {
 #ifndef NDEBUG
     std::cout << "\t\t\t < Copy constructed " << this << " with " <<mShift << std::endl;
 #endif
 }

 double
 Shifter::get_shift() const
 {
   return mShift;
 }

 void
 Shifter::operator=(const Shifter& s)
 {
 #ifndef NDEBUG
   std::cout << "\t\t\tAssignment = changing shift from " << mShift << " to " << s.mShift << std::endl;
 #endif
   mShift = s.get_shift();
 }

 double
 Shifter::operator()(const double x) const
 {
   return x + mShift;
 };





template <class F>
class tuple_binary_function
{
  F mF;
  
 public:
  typedef          tuple_binary_function<F>             type;
  typedef typename function_result_type<F>::type result_type;
  
 tuple_binary_function(F f) : mF(f) {}

  // result_type operator()(typename boost::tuple<double,double> const& tup) const {    return mF(tup.get<0>(),tup.get<1>());    }
  // either works if not a template

  // double operator()(typename boost::tuple<double,double> const& tup) const {    return mF(tup.get<0>(),tup.get<1>());    }
  
  // result_type operator()(typename boost::tuple<double,double> const& tup) const {    return mF(tup.get<0>(),tup.get<1>());    }

  // these formulations die with the "expected primary expression" error

  //    template <class Tuple>
  //   result_type operator()(Tuple const& tup) const {    return mF(tup.get<0>(),tup.get<1>());    }

  /*  template<class  get_zero(T const &tup)
  {
    return T::get < 0 > (tup);
  }
  */
  template<class X, class Y>
  result_type operator()(
			 typename boost::tuple<X,Y> const& tup) const
  {
    // fails as member function
    //    X x=tup.get<0>();
    X x=boost::tuples::get<0>(tup);
    Y y=boost::tuples::get<1>(tup);
    // fails Koenig lookup
    // X x=get<0>(tup);
    // Y y=get<1>(tup);
    return mF(x,y);
  }


  
};



class Adder : public std::binary_function<double,double,double> {
  double mConstant;
public:
  Adder()                : mConstant(0.0) { }
  Adder(double constant) : mConstant(constant) { }
  double operator()(const double x, const double y) const { return x + y + mConstant; }
};



 template <class I, class Pred>
 int
 count_if (std::pair<I,I> range, Pred pred)
 {
   return count_if(range.first, range.second, pred);
 }




template<class range>
void
foo(const range& r)
{
  typename range_traits<range>::value_type v();
  std::cout << v << std::endl;
};

//  OUTPUT OUTPUT OUTPUT OUTPUT OUTPUT OUTPUT OUTPUT OUTPUT OUTPUT OUTPUT OUTPUT OUTPUT OUTPUT

template <class Iter>
inline
std::ostream&
operator<<(std::ostream& os, const range<Iter>& r)
{
  typedef typename std::iterator_traits<Iter>::value_type local_type;
  std::copy( begin(r), end(r),  std::ostream_iterator<local_type>(os, " "));
  os << std::endl;
  return os;
}


//  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  MAIN  

int main()
{

  std::vector<double> iz;
  for(int i=0; i<5; ++i)
    iz.push_back(10 * i);
  
  range<std::vector<double>::const_iterator> rng(iz.begin(),iz.end());
  std::cout << "\n Direct access to ends: " << *begin(rng) << " -- " << *(end(rng)-1) << std::endl;
  std::cout << "\n Length of range      : " << end(rng)-begin(rng) << std::endl;
  
  
  { // use unary ranges
    std::cout << "  shifted vector:           " << make_unary_range(Shifter(6.6),iz) << std::endl;
    std::cout << "  lambda function:          " << make_unary_range(ret<double>(_1 + 6.6), iz) << std::endl;
    std::cout << "  direct shifter:           " << make_unary_range(std::bind1st(std::plus<double>(),6.6),iz) << std::endl;
    std::cout << "  begin of shifted vector:  " << *begin(make_unary_range(Shifter(6.6), make_range(iz))) << std::endl;
    std::cout << std::endl;
  }

  { // Make the operator first
    Shifter s1(1.0);
    std::cout << "  shifted range is ... "
	      << make_unary_range(s1, iz) << std::endl;
  }
  
  {
    
    std::cout << "\nTest of binary iterators...  These must operate on the tuples made by boost\n";
    boost::tuple<double,double> tuplePair = boost::make_tuple(3.3,3.3);

    std::cout << "    First apply a function for a tuple to a tuple    " << TupleSum()(tuplePair) << std::endl;  // defined in function_iterators.h

    std::cout << "iz + iz         :           " << make_binary_range(TupleAdder(), make_range(iz),make_range(iz));

    //  Now use a binary function converted into a tuple function in some manner

    //  This fails since Adder is a binary function
    // std::cout << "iz + iz         :           " << make_binary_range(Adder(), make_range(iz),make_range(iz));     
    //  This fails since the function operators on tuples (ie, the pair is bundled into a zip)
    // std::cout << "iz + iz   lam   :           " << make_binary_range(ret<double>(_1+_2), make_range(iz),make_range(iz));

    std::cout << "    Then adapt a binary function to a paired tuple;       scalar sum =  ";
    std::cout << tuple_binary_function<Adder>(Adder())(tuplePair) << std::endl;
    std::cout << " iz+iz          :   " << make_binary_range(tuple_binary_function<Adder>(Adder()), make_range(iz), make_range(iz)) << std::endl;
    
    //  boost cannot find 'get'?
    //  std::cout << "iz+iz via bind  :           " << make_binary_range(ret<double>(boost::lambda::bind(boost::tuples::get<0>(),_1)), make_range(iz), make_range(iz));
    
    // this fails since lambda functions cannot use _1. and the like
    // std::cout << "iz+iz via tuple :           " << make_binary_range(ret<double>(_1.get<0>() + _1.get<1>()), make_range(iz), make_range(iz));
    // std::cout << "iz+iz via tuple :           " << make_binary_range(ret<double>(boost::lambda::bind(boost::tuples::get<0>,_1)), make_range(iz), make_range(iz));

    
  }


  { std::cout << "Speed test of the binary iterator versus use of addition\n" ;
  }
  
  /*   
  { 
    std::cout << std::endl << "Test of function ranges (and evaluator)" << std::endl;
    std::list< Shifter > f;
    f.push_back(Shifter(1.0));  f.push_back(Shifter(2.0)); 
    f.push_back(Shifter(3.0));  f.push_back(Shifter(4.0));

    double x (0.0);
    // here are two styles that reverse the order of evaulation in operator*
    std::cout << "Range of shifter functions evaluated at 0: " << make_function_range(x,f);
    std::cout << "                                         : " << make_unary_range(evaluator<Shifter>(x),f);
   } 
  */

  
  
  {
    std::cout << std::endl << "Test of composition functions" << std::endl;
    Shifter shift1(1.0);
    Function_Utils::Square s;
    Composer<Function_Utils::Square> comp(s);
    std::cout << "                                 (7+1)^2 = " << (comp(shift1))(7.0) << std::endl;
    std::cout << "                 Squares of 1+iz         : " << make_unary_range(make_composer(s)(shift1), iz);
  }

  /*
    {
    // std::cout << std::endl << "Test of table iterators" << std::endl;
    // std::cout << " " << make_unary_range( make_unary_range<Shifter>(iz), f);
    // std::cout << "--- and now transposed ---" << std::endl;
    // std::cout << " " << make_unary_range(make_function_range<double>(f), iz);
    }
    
    {
    std::list<Employee> staff;
    staff.push_back( Employee("Fred", 110) );
    staff.push_back( Employee("Barney", 90) );
    staff.push_back( Employee("Wilma", 125) );
    staff.push_back( Employee("Betty", 105) );
    staff.push_back( Employee("Dino", 5) );
    std::cout << "\n\nEmployee illustrations\n";
    
    std::cout << "Salaries are "
    << make_unary_range(std::mem_fun_ref(&Employee::salary), staff)
    << std::endl;
    }
  */
  std::cout << std::endl << "DONE." << std::endl;
  return 0;
}
