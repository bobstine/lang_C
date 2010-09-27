#include "regression.h"

#include <iostream>
#include <fstream>


std::ostream&
operator<<(std::ostream& os, std::pair<double,double> p)
{
  os << " <" << p.first << "," << p.second << "> ";
  return os;
}



int main(int, char **)
{
  std::cout.precision(2);

  int nRows (30);   // needs to be more than 400 to test distance matrix calculations
  int nCols ( 3);
  
  // form random matrix for response and predictors
  Eigen::VectorXd y (Eigen::VectorXd::Random(nRows));
  Eigen::MatrixXd X (Eigen::MatrixXd::Random(nRows,nCols));
  Eigen::MatrixXd Z (Eigen::MatrixXd::Random(nRows,nCols));

  //  write data so that can check in JMP/R
  Eigen::MatrixXd data(nRows,1+2*nCols);
  data << y , X , Z;

  std::string fileName ("test.dat");
  std::ofstream output(fileName.c_str());
  output << data << std::endl;
    
  // build a regression
  LinearRegression regr(y,X);
  std::cout << regr << std::endl;

  std::cout << "Response     : " << y.transpose()                    << std::endl;
  std::cout << "Fitted values: " << regr.fitted_values().transpose() << std::endl;
  std::cout << "Predictions  : " << regr.predict(X).transpose()      << std::endl;
  std::cout << "Residuals    : " << regr.residuals().transpose()     << "  with sum  " << regr.residuals().sum() << std::endl;

  std::cout << "Test of X[2] : " << regr.f_test_predictor(X.col(2))  << std::endl;
  std::cout << "Test of Z[0] : " << regr.f_test_predictor(Z.col(0))  << std::endl;
  std::cout << "White   Z[0] : " << regr.f_test_predictor(Z.col(0),1)<< std::endl;
  std::cout << "Test of Z    : " << regr.f_test_predictors(Z)        << std::endl;
  std::cout << "White   Z    : " << regr.f_test_predictors(Z, 1)     << std::endl;
  std::cout << "White Z, b=5 : " << regr.f_test_predictors(Z, 5)     << std::endl;
  
  
  // tail end of a vector; this one gets you the last 4
  //  std::cout << y.end(4).transpose() << std::endl;

  /* element access
  std::cout << y.coeff(1) << std::endl;
  y.coeffRef(1) = 44;
  std::cout << y.coeff(1) << std::endl;
  */

  // allows one to change without violating constness... const_cast_derived()
  return 0;
}
