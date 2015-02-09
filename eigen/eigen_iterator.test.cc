#include "eigen_iterator.h"
#include "regression.h"

#include <iostream>
#include <algorithm>

int
main(void)
{
  //  Eigen::VectorXd x(10);
  LinearRegression::Vector x(10);
  
  for(int i=0; i<10; ++i)
    x[i] = i;

  std::cout << "TEST: Iterator output... ";

  EigenVectorIterator xIter (&x);
  
  for(int i=0; i<10; ++i)
  { std::cout << " " << *xIter;
    ++xIter;
  }

  std::cout << std::endl;
  return 0;
}
