// $Id: function_iterators.test.cc,v 1.11 2004/08/25 14:47:08 bob Exp $    
  
#include "function_utils.h"
#include "function_iterators.h"
#include "range_traits.h"
#include "range.h"

#include "composer.h"
#include "evaluator.h"

#include "test_iterator.h"

#include <boost/lambda/lambda.hpp>

#include <iostream>
#include <vector>
#include <list>
#include <string>
#include <algorithm>
#include <utility>
#include <numeric>
#include <functional>

using namespace boost::lambda;

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


 class Adder : public std::binary_function<double,double,double> {
   double mConstant;
 public:
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
inline std::ostream&
operator<<(std::ostream& os, range<Iter> const& r)
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
  
  { // simple test printing a range of numbers
    std::cout << "\nTest with 5 doubles  :  " << rng;
  }
  
  { // use unary ranges
    std::cout << std::endl;
    std::cout << "  shifted vector:           "
	      << make_range(make_unary_iterator(Operator(6.6), iz.begin()), make_unary_iterator(Operator(6.6), iz.end()));
    std::cout << "  doubly shifted vector:    "
	      << make_unary_range(Shifter(6.6), make_unary_range(Shifter(6.6),iz) );
    std::cout << "  direct shifter:           "
	      << make_unary_range(std::bind1st(std::plus<double>(),6.6),iz) << std::endl;
    std::cout << "  lambda function:          "
	      << make_unary_range(ret<double>(_1+6.6),iz) << std::endl;
    std::cout << "  begin of shifted vector:  "
	      << *begin(make_unary_range(Shifter(6.6), make_range(iz))) << std::endl;
    std::cout << "  from a saved range        ";
    range< unary_iterator<Shifter,std::vector<double>::const_iterator,std::random_access_iterator_tag> > rng = make_unary_range(Shifter(7.7),iz);
    std::cout << rng << std::endl;

    std::cout << std::endl;
  }

  { // Make the operator first
    Shifter s1(1.0);
    std::cout << "  shifted range is ... "
	      << make_unary_range(s1, iz) << std::endl;
  }
  
  {
    std::cout << "\nTest of binary iterators...\n";
    std::cout << "  100 + iz + iz :           " << make_binary_range(Adder(100.0), make_range(iz),make_range(iz));
    // Not working for constants...
    //    std::cout << "  100 + range   :           " << make_binary_range(std::plus<double>(), 100.0, make_range(iz));
    //    std::cout << "  range + 200   :           " << make_binary_range(std::plus<double>(), make_range(iz), 200.);
  }

  std::list< Shifter > f;
  f.push_back(Shifter(1.0));  f.push_back(Shifter(2.0)); 
  f.push_back(Shifter(3.0));  f.push_back(Shifter(4.0));
  
  {
    std::cout << std::endl << "Test of function ranges (and evaluator)" << std::endl;
    double x (0.0);
    // here are two styles that reverse the order of evaulation in operator*
    std::cout << "Range of shifter functions evaluated at 0: " << make_function_range(x,f);
    // std::cout << "                                         : " << make_unary_range(evaluator<Shifter>(x),f);  // requires non-const operator to change
    
    
    // Checking basic iterators using ranges
    std::list<double> iz;
    for(int i=0; i<10; ++i)
      iz.push_back(10 * i);
    test_bidirectional_iterator(begin(make_unary_range(Shifter(6.6),iz)));
    test_bidirectional_iterator(begin(make_unary_range(std::bind1st(std::plus<double>(),6.6),iz)));
    test_bidirectional_iterator(begin(make_binary_range(Adder(100.0), make_range(iz),make_range(iz))));
    test_bidirectional_iterator(begin(make_function_range(x,f)));
    test_bidirectional_iterator(begin(make_unary_range(evaluator<Shifter>(x),f)));
  }

  {
    // Check basic iterations for random access iterators
    std::vector<double> iz;
    for(int i=0; i<10; ++i)
      iz.push_back(10 * i);
    test_random_access_iterator(begin(make_unary_range(Shifter(6.6),iz)));
    test_random_access_iterator(begin(make_unary_range(std::bind1st(std::plus<double>(),6.6),iz)));
    test_random_access_iterator(begin(make_binary_range(Adder(100.0), make_range(iz),make_range(iz))));
  }
  
  {
    std::cout << std::endl << "Test of composition functions" << std::endl;
    Shifter shift1(1.0);
    Function_Utils::Square s;
    Composer<Function_Utils::Square> comp(s);
    std::cout << "                                 (7+1)^2 = " << (comp(shift1))(7.0) << std::endl;
    std::cout << "                 Squares of 1+iz         : " << make_unary_range(make_composer(s)(shift1), iz);
  }

  {
    std::cout << std::endl << "Test of table iterators" << std::endl;
    std::cout << " " << make_unary_range( make_unary_range<Shifter>(iz), f);
    std::cout << "--- and now transposed ---" << std::endl;
    std::cout << " " << make_unary_range(make_function_range<double>(f), iz);
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

  std::cout << std::endl << "DONE." << std::endl;
  return 0;
}
