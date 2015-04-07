#include "eigen_iterator.h"
#include "linear_regression.h"

#include <iostream>
#include <algorithm>

int
main(void)
{
  typedef EigenVectorIterator::Scalar Scalar;
  typedef EigenVectorIterator::Vector Vector;
  
  Vector x(10);
  
  for(int i=0; i<10; ++i)
    x[i] = (Scalar)i;

  std::cout << "TEST: Iterator output... ";

  EigenVectorIterator xIter (&x);
  
  for(int i=0; i<10; ++i)
  { std::cout << " " << *xIter;
    ++xIter;
  }

  std::cout << std::endl;
  return 0;
}
