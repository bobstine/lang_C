// $Id: range_ops.test.cc,v 1.1.1.1 2002/06/16 14:54:14 bob Exp $ 
  
#include "range.h"
#include "range_ops.h"
#include "utility.h"
#include "functions.h"

#include <vector>
#include <list>
#include <iostream>
#include <string>
#include <algorithm>
#include <utility>
#include <numeric>
#include <functional>

using namespace std;

class Shifter : public unary_function<double,double> {
  double mShift;
public:
  Shifter() { }
  Shifter(double s) : mShift(s) { }
  double operator()(const double x) const { return x + mShift; }
};

template <class Range, class Pred>
typename range_traits<Range>::difference_type
count_if (Range range, Pred pred)
{
  return count_if(begin(range), end(range), pred);
}

class Employee  {
  string mName;
  double mSalary;
public:
  Employee (char *name, double salary)
    : mName(name), mSalary(salary) { }
  string name() const { return mName; }
  double salary() const { return mSalary; }
};


int main()
{
  using namespace range_ops;

  vector< Shifter > f;
  f.push_back(Shifter(1.0));  f.push_back(Shifter(2.0)); 
  f.push_back(Shifter(3.0));  f.push_back(Shifter(4.0));
  
  vector<double> iz (5);
  iz[0]=0; iz[1]=1; iz[2]=2; iz[3]=3; iz[4]=4;
  
  { // simple test of range of numbers
    cout << "\nTest with 5 doubles:      " << make_range (iz);
    cout << "Summing up 0..4:          " << (plus<double>() | make_range (iz)) << endl;
    double product = multiplies<double>() | iz;
    cout << "Product up 0..4:          " << product << endl << endl;
    
    cout <<   "  squared via *:          " << make_range(iz) * make_range (iz);
    cout <<   "  range-range:            " << make_range(iz) - make_range (iz);
    cout <<   "  max of test vec       = " << *max_element(make_range(iz)) << endl;
    cout <<   "  shifted product vector: "
	 << make_unary_range(Shifter(6.0), make_range(iz) * make_range(iz));  
    cout <<   "  min shifted doubles   = "
	 << *min_element(make_unary_range(Shifter(6.0), make_range(iz))) << endl;
    cout << "Sum of shifting X by one is "
	 << accumulate(make_unary_range(Shifter(1.0), make_range(iz)), 0.0) << endl;
  }
  
  {
    cout << "\n\nEmployee illustrations\n";
    list<Employee> staff;
    staff.push_back( Employee("Fred", 110) );
    staff.push_back( Employee("Barney", 90) );
    staff.push_back( Employee("Wilma", 125) );
    staff.push_back( Employee("Betty", 105) );
    staff.push_back( Employee("Dino", 5) );
    cout << "Avg salary is "
	 << accumulate(make_unary_range(mem_fun_ref(&Employee::salary), make_range(staff)),0.0) / 5
	 << endl;
    cout << "Max salary is "
	 << *max_element(make_unary_range(mem_fun_ref(&Employee::salary), make_range(staff)))
	 << endl;
    cout << "Number above 100 is "
	 << count_if(make_unary_range(mem_fun_ref(&Employee::salary), make_range(staff)),
		     bind2nd(greater<double>(), 100))
	 << endl;
  }
  
  { // test of function iterators f_i(x)
    cout << endl;
    cout << "Shifts applied to 0: " << make_function_range(make_range(f), 0.0);
    double sum = accumulate(make_function_range(make_range(f), 0.0), 0.0);    
    cout << "Sum of shifts      = " << sum << endl;
    vector<double> wts(4,2.0);
    double sum2 = accumulate(make_function_range(make_range(f), 0.0) * make_range(wts), 0.0);
    cout << "Sum of wt shifts   = " << sum2 << endl;
  }
  
  {
    cout << "\nTests of binary range ops\n";
  
    vector<double> X;
    vector<double> Y;
    vector<double> Z;
    for(int i = 1; i <= 10; ++i)
      {
	X.push_back(i);
	Y.push_back(i*i);
	Z.push_back(i*i*i);
      }
      cout << accumulate(make_range(X) * make_range(X), 0) << endl;
      cout << accumulate(make_range(Y) / make_range(X), 0) << endl;
      cout << accumulate(make_range(X) + make_range(X), 0) << endl;
      cout << accumulate(make_range(X) - make_range(X), 0) << endl;
      cout << endl;
      
      cout << accumulate(make_range(X) * make_range(Y) * make_range(Z), 0) << endl;
      cout << accumulate(make_range(X) * make_range(Y) / make_range(Z), 0) << endl;
      cout << accumulate(make_range(X) - make_range(Z) / make_range(Y), 0) << endl;

      std::cout << "\nTests of APL style compress\n";
      cout << "+/ = " << (plus<double>() | X) << std::endl;

      cout << "-------------------  now without using make_range's ---------------" << endl;

      cout << accumulate(X * X, 0) << endl;
      cout << accumulate(Y / X, 0) << endl;
      cout << accumulate(X + X, 0) << endl;
      cout << accumulate(X - X, 0) << endl;
      cout << endl;
      cout << accumulate(X * Y * Z, 0) << endl;
      cout << accumulate(X * Y / Z, 0) << endl;
      cout << accumulate(X - Z / Y, 0) << endl;

      std::cout << "\nTests of APL style compress\n";
      cout << "+/ = " << (plus<double>() | X) << std::endl;

  }

  cout << "\n\nDONE."; 
  return 0;
}

