// $Id: range_ops.test.cc,v 1.41 2004/04/30 16:30:28 bob Exp $  

#include "sparse_iterator.h"
#include "range_ops.h"

#include "function_utils.h"
#include "function_iterators.h"

#include <vector>
#include <list>
#include <iostream>
#include <string>
#include <functional>


class Shifter : public std::unary_function<double,double> {
  double mShift;
public:
  Shifter() { }
  Shifter(double s) : mShift(s) { }
  double operator()(double x) const { return x + mShift; }
};

class Employee  {
  std::string mName;
  double mSalary;
public:
  Employee (std::string name, double salary)
    : mName(name), mSalary(salary) { }

  Employee (const Employee& e)
    : mName(e.mName), mSalary(e.mSalary) { }

  std::string name() const { return mName; }
  double salary() const { return mSalary; }
};

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


template<class Iter>
void
print_it(int n, Iter iter)
{
  for (int i=0; i<n; ++i, ++iter)
    std::cout << *iter << " ";
  std::cout << std::endl;
}





//  MAIN MAIN MAIN MAIN MAIN MAIN MAIN MAIN MAIN MAIN MAIN MAIN MAIN MAIN MAIN MAIN MAIN MAIN 

int main()
{
  using namespace range_ops;     //  < ---   Note

  std::vector< Shifter > f;
  f.push_back(Shifter(1.0));  f.push_back(Shifter(2.0)); 
  f.push_back(Shifter(3.0));  f.push_back(Shifter(4.0));

  std::vector<double> X, Y, Z;
  std::list<double> lst;
  for(int i = 1; i <= 10; ++i)
    {
      X.push_back(i);
      Y.push_back(i*i);
      Z.push_back(i*i*i);
      lst.push_back(i);
    }

  
  // check printing
  std::cout << "TEST: X is " << make_range(X) << " and lst is " << make_range(lst) << std::endl;
  
  {
    std::list<Employee> staff;
    staff.push_back( Employee("Fred", 110) );
    staff.push_back( Employee("Barney", 90) );
    staff.push_back( Employee("Wilma", 125) );
    staff.push_back( Employee("Betty", 105) );
    staff.push_back( Employee("Dino", 5) );
    std::cout << "\n\nEmployee illustrations\n";

    std::cout << *begin(make_unary_range(std::mem_fun_ref(&Employee::salary), make_range(staff))) << std::endl;
    std::cout << *++begin(make_unary_range(std::mem_fun_ref(&Employee::salary), make_range(staff))) << std::endl;

    accumulate(make_unary_range(std::mem_fun_ref(&Employee::salary), make_range(staff)),0.0);
    std::cout << "Avg salary is "
	      << accumulate(make_unary_range(std::mem_fun_ref(&Employee::salary), make_range(staff)),0.0) / 5
	      << std::endl;
     std::cout << "Max salary is "
 	      << *max_element(make_unary_range(std::mem_fun_ref(&Employee::salary), make_range(staff)))
 	      << std::endl;
     std::cout << "Number above 100 is "
 	      << count_if(make_unary_range(std::mem_fun_ref(&Employee::salary), make_range(staff)),
 			  bind2nd(std::greater<double>(), 100))
 	      << std::endl;
  }


  { // accumulate
    std::cout << std::endl << "Test of function iterators f_i(x)" << std::endl;
    std::cout << "Shifts applied to 0: " << make_function_range(0.0, f);
    double sum = accumulate(make_function_range(0.0,f), 0.0);    
    std::cout << "Sum of shifts      = " << sum << std::endl;
  }

  { // arithmetic
    std::cout << " Sum     of two ranges " << make_range(X) - make_range(Y) << std::endl;
    std::cout << " Product of two ranges " << make_range(X) * make_range(Y) << std::endl;
  }

  { // product of vector with scalar
    std::cout << "\nTest of inner product with range as a result" << std::endl;
    
    typedef std::vector<double>::const_iterator Iter;
    std::vector< range<Iter> > vecOfRanges;
    vecOfRanges.push_back(make_range(X));
    vecOfRanges.push_back(make_range(Y));
    vecOfRanges.push_back(make_range(Z));
    std::cout << "  mat as raw output  \n " <<  make_range(vecOfRanges) << std::endl;

    std::vector< double > y(3);
    for (int i=0; i<3; ++i) y[i] = i;

    // Not yet?
    // std::cout <<  make_range(y) * make_range(vecOfRanges);

  }

  
  // TABLE TABLE TABLE TABLE TABLE TABLE TABLE TABLE TABLE TABLE TABLE TABLE TABLE TABLE TABLE TABLE
  
  std::cout << "\nTable operations\n";
  std::vector<double> initial(X.size());

  {
    std::cout << "Test of mutating operators" << std::endl;

    fill (make_range(&initial), 0.0);
    std::cout << "Zero range       : " << make_range(initial) << std::endl;
    transform(make_range(X), begin(make_range(&initial)), Shifter(6.6));
    std::cout << "Shift of initial : " << make_range(initial) << std::endl;
  }
  
  {
    std::cout << "Test accumulating into a range" << std::endl;

    std::fill(initial.begin(), initial.end(), 0.0);
    //    std::cout << "Sum of table: +/f  \n"   // Sums over the 'first' aspect (as in apl)
    //	      << accumulate (make_unary_range(make_function_range<double>(f), X),
    //			     make_assignable_range(&initial))
    //	      << std::endl;
    std::cout << "Sum of table, from destination \n " << make_range(initial) << std::endl;
    
  }
  {

    typedef std::vector<std::pair<int,double> > m_type;
    typedef m_type::const_iterator ci_type;
    m_type m;
    m.push_back(std::make_pair(1,10.));
    m.push_back(std::make_pair(2,20.));
    m.push_back(std::make_pair(3,30.));
    m.push_back(std::make_pair(5,50.));
    m.push_back(std::make_pair(8,80.));
    m.push_back(std::make_pair(12,120.));
    m.push_back(std::make_pair(18,180.));
    m.push_back(std::make_pair(25,0));   // must be a sentinel for the last valid position.

    const_sparse_iterator<int,double> j(m,0);
    {

      std::cout << "strat of base vector:       ";
      print_it(20, j);

      std::cout << "Unary iterator of a sparse_iterator:\n";
      std::cout << "  shifted vector:           ";
      print_it(20, make_unary_iterator (Operator(6.6), j));

      /*  sparse not working yet
	  
      std::cout << "Incorrect accumulation:     "
		<< accumulate(make_unary_range(Operator(6.6), make_sparse_range(m)),0.0)
		<< " = " << accumulate(make_sparse_range(m),0.0)
		<< " + 6.6 * number non-zero" << std::endl ;
      
      std::cout << "Binary iterator of a sparse:\n";
      std::cout << "   doubling:                ";
      print_it(20, make_binary_iterator(std::plus<double>(), j, j));
      std::cout << "correct accumulation:     "
		<< accumulate(make_binary_range(std::plus<double>(), make_sparse_range(m), make_sparse_range(m)),0.0)
		<< " = 2 * " << accumulate(make_sparse_range(m),0.0)
		<< std::endl ;
      */

		
      /*    typedef binary_iterator< std::plus<double>,
	    std::vector<double>::iterator,std::vector<double>::iterator> Iter;
	    Iter b = make_binary_iterator(std::plus<double>(), iz.begin(), iz.begin());
	    Iter e = make_binary_iterator(std::plus<double>(), iz.end(), iz.end());
	    for(; b != e; ++b)
	    std::cout << *b << " ";
	    std::cout << std::endl;
      */
    }

  }  
  std::cout << "\n\nDONE." << std::endl; 
  return 0;
}
