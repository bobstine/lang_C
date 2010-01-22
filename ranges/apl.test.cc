#include "apl.h"
#include "range_ops.h"
#include "anonymous_iterator.h"

#include <vector>
#include <iostream>

void
trap()
{
  int i = 4;
  i = 5 * i;
}

class Shifter : public std::unary_function<double,double> {
  double mShift;
public:
  Shifter() {}
  Shifter(double s) : mShift(s) { }
  double operator()(double x) const { return x + mShift; }
  Shifter(const Shifter& rhs):
    mShift(rhs.mShift)
  {
    trap();
    std::cout << "\t\t\tCOPY CALLED." << std::endl;
  };
};


int main()
{
  using namespace range_ops;
  using namespace apl;
  
  std::vector< Shifter > f;
  f.push_back(Shifter(1.0));  f.push_back(Shifter(2.0)); 
  f.push_back(Shifter(3.0));  f.push_back(Shifter(4.0));

  std::vector<double> X, Y, Z;
  for(int i = 1; i <= 100; ++i)
    {
      X.push_back(i);
      Y.push_back(i*i);
      Z.push_back(i*i*i);
    }

  {
    std::cout << "Test with 10 doubles:     " << make_range(X);

    std::cout << "Summing up 1..10:         " << (std::plus<double>() | X) << std::endl;
    std::cout << "Summing up 2 + 1..10:     " << (std::plus<double>() | make_unary_range(Shifter(2.0),X)) << std::endl;
    std::cout << "Product up 0..9:          " << (std::multiplies<double>() | X) << std::endl;
    std::cout << "  squared via *:          " << X * X;
    std::cout << "  range-range:            " << X - X;
    std::cout << "  double via +:           " << X + X;
    std::cout << "  double via /:           " << X / X;
    std::cout << "  shifted product vector: " << make_unary_range(Shifter(6.0), X * X);
  }
  
    
  {
    std::cout << "Test accumulate, with implicit range conversions" << std::endl;
    
    //    std::cout << accumulate(2.0 * X, 0.0) << std::endl;     // Compiles, but not X * 2 (scalar iterator comparison fails)
    std::cout << accumulate(make_range(X) * make_range(X), 0.0) << std::endl;
    std::cout << accumulate(X * X, 0.0) << std::endl;
    std::cout << accumulate(Y / X, 0.0) << std::endl;
    std::cout << accumulate(X + X, 0.0) << std::endl;
    std::cout << accumulate(X - X, 0.0) << std::endl;
    std::cout << accumulate(X * Y * Z, 0.0) << std::endl;
    std::cout << accumulate(X * Y / Z, 0.0) << std::endl;
    std::cout << accumulate(X - Z / Y, 0.0) << std::endl;
  }  

  {
    std::cout << "\nTest of inner product with range as a result" << std::endl;
    
    typedef std::vector<double>::const_iterator Iter;
    std::vector< range<Iter> > vecOfRanges;
    vecOfRanges.push_back(make_range(X));
    vecOfRanges.push_back(make_range(Y));
    vecOfRanges.push_back(make_range(Z));
    std::cout << "  mat as raw output  \n " <<  make_range(vecOfRanges) << std::endl;

    //    std::cout << "  3 * v[0]  " <<  3. * vecOfRanges[0] << std::endl;

    std::vector< double > y(3);
    for (int i=0; i<3; ++i) y[i] = i;

    //    int i = y * vecOfRanges;
    //    std::cout << i << std::endl;
    
    //    std::cout << *begin(y * vecOfRanges) << std::endl;
    //    std::cout << "y * mat as raw output  " <<  make_range(y) * make_range(vecOfRanges) << std::endl;
    //    std::cout << "y * mat as raw output  " <<  y * vecOfRanges << std::endl;
    
    // std::vector< double > result (10);
    //     std::cout << "y.mat assigned to range "
    // 	      << accumulate( make_range(y) * make_range(matrix), make_assignable_range(&result))
    // 	      << std::endl;
  }


  { 
    std::cout << "\nTest with 10 doubles:     " << make_range(X);
    {
      //      std::cout << "  2 X             : " << 2.0 * make_range(X);    
      std::cout << "  double via +    : " << make_range(X) + make_range (X);
      std::cout << "  zero via -      : " << make_range(X) - make_range (X);
      std::cout << "  one via /       : " << make_range(X) / make_range (X);
      std::cout << "  squared         : " << make_range(X) * make_range (X);
      std::cout << "  shifted squares : " << make_unary_range(Shifter(6.0), make_range(X) * make_range(X));
    }
  }

  {
    std::cout << "\n Test accumulate 10 doubles:     " << make_range(X) ;

    std::cout << "  sum          : " << accumulate(make_range(X), 0.0) << std::endl;
    std::cout << "  double via + : " << accumulate(make_range(X) + make_range (X), 0.0)<< std::endl;
    std::cout << "  squared via *: " << accumulate(make_range(X) * make_range (X), 0.0)<< std::endl;
  }
  

  std::cout << "\n\nDONE." << std::endl; 
  return 0;
}

