#include <iostream>

#include "confusion_matrix.h"
#include "eigen_iterator.h"

int main()
{
  size_t n = 100;

  Eigen::VectorXd y = Eigen::VectorXd::Random(n);
  for(size_t i=0; i<n; ++i)
  { if(y(i) < 0.5) y[i] = 0;
    else y(i) = 1;
  }
  std::cout << "TEST:  y   = " << y.transpose() << std::endl;
  Eigen::VectorXd yHat= Eigen::VectorXd::Random(n);
  std::cout << "TEST: yHat = " << yHat.transpose() << std::endl;

  ConfusionMatrix cm{n, EigenVectorIterator(&y), EigenVectorIterator(&yHat)};

  std::cout << cm << std::endl;
}
