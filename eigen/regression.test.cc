#include "regression.h"

#include <iostream>
#include <fstream>

#include <time.h>

std::ostream&
operator<<(std::ostream& os, std::pair<double,double> p)
{
  os << " <" << p.first << "," << p.second << "> ";
  return os;
}

void print_time(time_t const& start)
{
  std::cout << "Time = " << double(clock() - start)/CLOCKS_PER_SEC << std::endl;
}


int main(int, char **)
{
  std::cout.precision(2);  std::cout << "TEST:  Test of regression begins...\n\n";

  // Timing
  //   p = 3 predictors      0.19 sec on 13" with n = 400,000; 0.12 on 17"
  //      20                                                   1.6
  //      40                                                   5.2           but takes forever to calc beta when printing???
  //      80                                                  21 
  int nRows (400000);   
  int nCols (    20);
  int nAdd  (     3);
  
  //  std::cin >> nRows;  std::cout << "Testing regression code with nRows = " << nRows << std::endl;
  
  // form random matrix for response and predictors
  Eigen::VectorXd y (Eigen::VectorXd::Random(nRows));
  Eigen::VectorXd z (Eigen::VectorXd::Random(nRows));
  Eigen::MatrixXd X (Eigen::MatrixXd::Random(nRows,nCols));
  Eigen::MatrixXd Z (Eigen::MatrixXd::Random(nRows,nAdd));

  //  write data so that can check in JMP/R
  /*
    Eigen::MatrixXd data(nRows,1+2*nCols);
    data << y , X , Z;
    std::string fileName ("test.dat");
    std::ofstream output(fileName.c_str());
    output << data << std::endl;
  */
  
  // build a regression
  clock_t start = clock();     LinearRegression regr(y,X);       print_time(start);
  std::cout << regr << std::endl;

  int show ((nRows > 10)?10:nRows);
  std::cout << "Response     : " << y.segment(0,show).transpose()                    << std::endl;
  std::cout << "Fitted values: " << regr.fitted_values().segment(0,show).transpose() << std::endl;
  std::cout << "Predictions  : " << regr.predict(X).segment(0,show).transpose()      << std::endl;
  std::cout << "Residuals    : " << regr.residuals().segment(0,show).transpose()     << "  with sum  " << regr.residuals().sum() << std::endl;

  // test the test routines
  start = clock(); std::cout << "Test of X[2]   : " << regr.f_test_predictor(X.col(2))   << std::endl; print_time(start);

  start = clock(); std::cout << "Test of col z  : " << regr.f_test_predictor(z)          << std::endl; print_time(start);
  start = clock(); std::cout << "Test 'matrix' z: " << regr.f_test_predictors(z)         << std::endl; print_time(start); std::cout << std::endl;
  
  start = clock(); std::cout << "Test of Z[0]   : " << regr.f_test_predictor(Z.col(0))   << std::endl; print_time(start);
  start = clock(); std::cout << "White   Z[0]   : " << regr.f_test_predictor(Z.col(0),1) << std::endl; print_time(start);
  start = clock(); std::cout << "White Z[0],b=5 : " << regr.f_test_predictor(Z.col(0),5) << std::endl; print_time(start); std::cout << std::endl;

  start = clock(); std::cout << "Test of Z      : " << regr.f_test_predictors(Z)         << std::endl; print_time(start);
  start = clock(); std::cout << "White   Z      : " << regr.f_test_predictors(Z, 1)      << std::endl; print_time(start);
  start = clock(); std::cout << "White Z, b=5   : " << regr.f_test_predictors(Z, 5)      << std::endl; print_time(start);

  start = clock(); std::cout << "Adding 1 pred  : ";  regr.add_predictor(z);                           print_time(start);
  start = clock(); std::cout << "Adding 3 pred  : ";  regr.add_predictors(Z);                          print_time(start);
  
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
