// $Id: range.test.cc,v 1.1.1.1 2002/06/16 14:54:12 bob Exp $    
  
#include "range_traits.h"
#include "range.h"
#include "utility.h"
#include "functions.h"

#include <iostream>
#include <vector>
#include <list>
#include <string>
#include <algorithm>
#include <utility>
#include <numeric>
#include <functional>

 
class Shifter : public std::unary_function<double,double>
{
  double mShift;
  bool my_true;
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

class Employee  {
  std::string mName;
  double mSalary;
public:
  Employee (char *name, double salary)
    : mName(name), mSalary(salary) { }
  std::string name() const { return mName; }
  double salary() const { return mSalary; }
};

//
// Note:
//   Include *only* this function from range_ops.h for printing ranges.
//
template <class Iter>
inline std::ostream&
operator<<(std::ostream& os, std::pair<Iter,Iter> range)
{
  copy(begin(range), end(range),
       std::ostream_iterator<typename std::iterator_traits<Iter>::value_type>(os, " "));
  os << std::endl;
  return os;
}

int main()
{
  std::vector<double> iz (10);
  iz[0]=0; iz[1]=10; iz[2]=20; iz[3]=30; iz[4]=40;
  iz[5]=50;iz[6]=60;iz[7]=70;iz[8]=80;iz[9]=90;

  { // simple test printing a range of numbers
    std::cout << "\nTest with 5 doubles:      " << make_range(iz) << std::endl;
  }
  { // use the unary iterators
    std::cout << std::endl;
    std::cout << "  begin of shifted vector:  " << *(begin(make_unary_range(Shifter(6.6), make_range(iz)))) << std::endl;
    std::cout << std::endl;
    std::cout << "  begin of shifted vector:  " << *begin(make_unary_range<Shifter>(make_range(iz))(Shifter(6.6))) << std::endl;
    std::cout << std::endl;
    std::cout << "  shifted vector:           " << make_unary_range(Shifter(6.6), iz);
    std::cout << std::endl;
    std::cout << "  shifted vector:           " << make_unary_range<Shifter>(make_range(iz))(Shifter(6.6));
    std::cout << std::endl;
    std::cout << "  shifted vector:           " << make_unary_range(Shifter(6.0), iz);
    std::cout << std::endl;
    std::cout << "  100 + iz + iz :           " << make_binary_range(Adder(100.0),
								     make_range(iz),make_range(iz));
    std::cout << std::endl;
  }

  std::vector< Shifter > f;
  f.push_back(Shifter(1.0));  f.push_back(Shifter(2.0)); 
  f.push_back(Shifter(3.0));  f.push_back(Shifter(4.0));

  { // Test of function iterators f_i(x)
    std::cout << std::endl;
    std::cout << "Shifter functions applied to 0: " << make_function_range(f, 0.0);
    std::cout << "Shifter functions applied to 0: " << make_function_range(make_range(f))(0.0);
    std::cout << "Shifter functions applied to 0: " << make_other_unary_range(make_range(f),0.0);
  }

  {  // test of table iterators
    std::cout << std::endl;
    std::cout <<" "<< make_unary_range(make_unary_range<Shifter>(make_range(iz)),make_range(f));  
    std::cout << "----------" << std::endl;
    std::cout << " " << make_unary_range(make_function_range(make_range(f)), make_range(iz));

    // NOTE: the above crashes if the make_range's are left off.  I don't know why.

    //	std::vector<double> initial(4, 0.0);
    //	std::cout << "Sum of table columns: \n"
    //	<< accumulate_table(make_row_major_table(make_range(f), make_range(iz)), make_range(initial));
  }
    
  { // test of composing functions, squareing the shifter functions
    // application here works fine, with one-term <F> template
    std::cout << std::endl;
      
    Shifter shift1(1.0);
    Function::Square s; Composer<Function::Square> comp(s);
    std::cout << "(7+1)^2 = " <<  (comp(shift1))(7.0) << std::endl;
    std::cout << "Squares, again at once    "
	      << make_function_range(make_unary_range(make_composer(s),make_range(f)),0.0);
  }
  std::cout << std::endl << "DONE." << std::endl; 
  return 0;
}




