#include "Eigen/Array"
#include "Eigen/QR"


#include <iostream>


int main(int, char **)
{
  std::cout.precision(2);

  int nRows (10);   // needs to be more than 400 to test distance matrix calculations
  int nCols ( 3);
 
    
  // form random matrix for response and predictors
  Eigen::VectorXd y (Eigen::VectorXd::Random(nRows));
  Eigen::MatrixXd X (Eigen::MatrixXd::Random(nRows,nCols));

  // QR factorization
  Eigen::QR<Eigen::MatrixXd> qr(X);
  std::cout << qr.matrixR() << std::endl;
  std::cout << qr.matrixQ() << std::endl << std::endl;

  // tail end of a vector; this one gets you the last 4
  std::cout << y.end(4).transpose() << std::endl;

  // element access
  std::cout << y.coeff(1) << std::endl;
  y.coeffRef(1) = 44;
  std::cout << y.coeff(1) << std::endl;

  // allows one to change without violating constness... const_cast_derived()
  return 0;
}
