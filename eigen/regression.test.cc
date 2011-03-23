// -*- c++ -*-
#include "regression.h"
#include "debug.h"

#include <vector>
#include <iostream>
#include <sstream>
#include <fstream>
#include <algorithm>

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


//     main     main     main     main     main     main     main     main     main     main     main     main     

int main(int, char **)
{
  std::cout << "TEST:  Test of regression begins...\n\n";
  //    clock_t start;
  debugging::debug_init(std::cout,0);
  std::cout.precision(4);  

  // Timing to build regr with n = 400,000 rows    Time to test adding a 1 or 3 vars to a model with 30 predictors
  //                          13"        17"                 13"       13"
  //   p = 3 predictors      0.19 sec ; 0.12               Test Z[1]  Test Z[1:3]
  //      20                 2.6        1.6                 0.15      0.30        
  //      40                            5.2           
  //      80                           21 
  const int nRows   (100);   
  const int nCols     (3);
  const int nAdd      (3);
  
  //  std::cin >> nRows;  std::cout << "Testing regression code with nRows = " << nRows << std::endl;
  
  // form random matrix for response and predictors
  Eigen::VectorXd y (Eigen::VectorXd::Random(nRows));
  Eigen::VectorXd z (Eigen::VectorXd::Random(nRows));
  Eigen::VectorXd w (Eigen::VectorXd::Zero  (nRows));
  Eigen::MatrixXd X (Eigen::MatrixXd::Random(nRows,nCols));
  Eigen::MatrixXd Z (Eigen::MatrixXd::Random(nRows,nAdd));


  { // most basic test of regression
    double mean (y.sum()/y.size());
    std::cout << "TEST:  y-bar is " << mean << std::endl;
    std::cout << "       y        " << y(0) << "  " << y(1) << "  " << y(2) << std::endl;
    std::cout << "       centered " << y(0)-mean << "  " << y(1)-mean << "  " << y(2)-mean << std::endl;

    LinearRegression regr("yyy", y, 0);
    std::cout << "TEST: Initialized regression " << std::endl << regr << std::endl;
    std::cout << "TEST: Initial beta = " << regr.beta().transpose() << "    gamma = " << regr.gamma().transpose() << std::endl;
    std::cout << "TEST: Residuals (first 10) = " << regr.residuals().head(10).transpose() << std::endl << std::endl;
    
    std::cout << "TEST: F test of zzz " << regr.f_test_predictor("zzz", z) << std::endl;
    regr.add_predictors();
    std::cout << "TEST: regression after adding zzz " << std::endl << regr << std::endl;
    std::cout << "TEST: Initial beta = " << regr.beta().transpose() << "    gamma = " << regr.gamma().transpose() << std::endl;
    std::cout << "TEST: Residuals (first 10) = " << regr.residuals().head(10).transpose() << std::endl << std::endl;
    
  }
  /*
  
  // define the weight vector
  for (int i=0; i<nRows; ++i)
    w[i] = 1.0 + i % 4;
  std::cout << "TEST: weight vector is " << w.head(10).transpose() << std::endl;

  // names for variables
  std::vector<std::string> xNames;
  std::vector<std::string> zNames;
  for (int i=0; i<nCols; ++i)
  { std::ostringstream oss;
    oss << "X" << i;
    xNames.push_back(oss.str());
  }
  for (int i=0; i<nAdd; ++i)
  { std::ostringstream oss;
    oss << "Z" << i;
    zNames.push_back(oss.str());
  }
  */
  
  //  write data so that can check in JMP/R
  if (nRows < 1000)
  { Eigen::MatrixXd data(nRows,1+2*nCols);
    data << y , X , Z;
    std::string fileName ("test.dat");
    std::ofstream output(fileName.c_str());
    output.precision(7);
    output << data << std::endl;
  }


  // check validation model (dup validation and estimation cases)
  /*  
  if (true) {
    std::cout << "TEST: Testing validated model\n";
    bool   cv[2*nRows];
    double yPtr[nRows*2];
    for(int i=0; i<nRows; ++i)
    { yPtr[2*i] = y(i); yPtr[2*i+1] = y(i);
      cv[2*i] = true; cv[2*i+1]=false;
    }
    std::vector<std::pair<std::string, double*> > xcollection;
    for(int j=0; j<nCols; ++j)
    { double *xPtr = new double [nRows*2];
      for(int i=0; i<nRows; ++i)
      { xPtr[2*i] = X(i,j);
	xPtr[2*i+1] = X(i,j);
      }
      xcollection.push_back(make_pair(xNames[j],xPtr));
    }
    std::vector<std::pair<std::string, double*> > zcollection;
    for(int j=0; j<nAdd; ++j)
    { double *xPtr = new double [nRows*2];
      for(int i=0; i<nRows; ++i)
      { xPtr[2*i] = Z(i,j); xPtr[2*i+1] = Z(i,j); }
      zcollection.push_back(make_pair(xNames[j],xPtr));
    }
    { // test validated regression, standard F test
      std::cout << "\n\n-----------------------------------------------------------------------------------\nTEST: test of standard F p-values\n";
      ValidatedRegression vregr("Y", yPtr, cv, 2*nRows, 0, true);
      std::cout << vregr << std::endl;
      std::pair<double,double> result;
      result = vregr.add_predictors_if_useful (xcollection, 1.0);
      std::cout << "TEST: test of adding xcollection gives " << result << std::endl << vregr << std::endl;
      double *pFit  (new double[2*nRows]);
      vregr.fill_with_fit(pFit);
      std::cout << "TEST: First 10 fitted values from model with 3 X's are   ";
      std::for_each(pFit, pFit+10, [](double x){ std::cout << x << "  ";});
      std::cout << std::endl;
      {
	std::vector<std::pair<std::string, double*> > collect1;
	collect1.push_back(zcollection[0]);
	start = clock(); result = vregr.add_predictors_if_useful (collect1,0.00001); print_time(start);
      }
      std::cout << "TEST: test of adding z[0] to x[0,1,2] model gives " << result << std::endl << vregr << std::endl; 
      start = clock(); result = vregr.add_predictors_if_useful (zcollection, 1.0); print_time(start);
      std::cout << "TEST: test of adding zcollection gives " << result << std::endl << vregr << std::endl;
    }
    { // test validated regression, white tests
      const int blockSize (1);
      ValidatedRegression vregr("Y", yPtr, cv, 2*nRows, blockSize, true);
      std::cout << "\n\n-----------------------------------------------------------------------------------\nTEST: check white p-values with block size " << blockSize << "\n";
      std::cout << vregr << std::endl;
      std::pair<double,double> result;
      result = vregr.add_predictors_if_useful (xcollection, 1.0);
      std::cout << "TEST: test of adding initial xcollection gives " << result << std::endl << vregr << std::endl;
      {
	std::vector<std::pair<std::string, double*> > collect1;
	collect1.push_back(zcollection[0]);
	start=clock(); result = vregr.add_predictors_if_useful (collect1,0.00001); print_time(start);
      }
      std::cout << "TEST: test of adding z[0] to x[0,1,2] model gives " << result << std::endl << vregr << std::endl;
      start=clock(); result = vregr.add_predictors_if_useful (zcollection, 1.0); print_time(start);
      std::cout << "TEST: test of adding zcollection gives " << result << std::endl << vregr << std::endl;
    }
    { // test validated regression, white tests
      const int blockSize (5);
      ValidatedRegression vregr("Y", yPtr, cv, 2*nRows, blockSize, true);
      std::cout << "\n\n-----------------------------------------------------------------------------------\nTEST: check white p-values with block size " << blockSize << "\n";
      std::cout << vregr << std::endl;
      std::pair<double,double> result;
      result = vregr.add_predictors_if_useful (xcollection, 1.0);
      std::cout << "TEST: test of adding initial xcollection gives " << result << std::endl << vregr << std::endl;
      {
	std::vector<std::pair<std::string, double*> > collect1;
	collect1.push_back(zcollection[0]);
	start=clock(); result = vregr.add_predictors_if_useful (collect1,0.00001); print_time(start);
      }
      std::cout << "TEST: test of adding z[0] to x[0,1,2] model gives " << result << std::endl << vregr << std::endl;
      start=clock(); result = vregr.add_predictors_if_useful (zcollection, 1.0); print_time(start);
      std::cout << "TEST: test of adding zcollection gives " << result << std::endl << vregr << std::endl;
    }
  }
  */
  
  // weighted regression
  /* {
    std::cout << "\n\nTEST: build 3 predictor WLS fit\n";
    start = clock();     LinearRegression regr("y",y,w);     print_time(start);
    start = clock();     regr.add_predictors(xNames,X);      print_time(start);
    std::cout << regr << std::endl;
    
    int show ((nRows > 10)?10:nRows);
    std::cout << "Fitted values: " << regr.fitted_values().head(show).transpose()   << std::endl;
    std::cout << "Residuals    : " << regr.residuals().head(show).transpose()       << std::endl;
    std::cout << "Raw Residuals: " << regr.raw_residuals().head(show).transpose()   << std::endl;
  }
  */
  // build a regression; no shrinkage for initial variables
  /*
    {
    std::cout << "\n\n---------------------------------------------------------------------------------\nTEST: test of white regression\n";
    const int blockSize (0);
    start = clock();     LinearRegression regr("y",y,blockSize);       print_time(start);
    start = clock();     regr.add_predictors(xNames,X);                print_time(start);
    std::cout << regr << std::endl;
    
    int show ((nRows > 10)?10:nRows);
    std::cout << "Response     : " << y.segment(0,show).transpose()                    << std::endl;
    std::cout << "Fitted values: " << regr.fitted_values().segment(0,show).transpose() << std::endl;
    std::cout << "Predictions  : " << regr.predictions(X).segment(0,show).transpose()  << std::endl;
    std::cout << "Residuals    : " << regr.residuals().segment(0,show).transpose()     << "  with sum  " << regr.residuals().sum() << std::endl;
    
    // test the test routines
    std::cout << "\nStarting of the timing routines for testing/adding predictors\n";
    std::vector<std::string> names; names.push_back("z");
    
    start = clock(); std::cout << "Test of X[2]   : " << regr.f_test_predictor("X[2]", X.col(2))   << std::endl; print_time(start);
    start = clock(); std::cout << "Test of col z  : " << regr.f_test_predictor( " z ", z)          << std::endl; print_time(start);
    start = clock(); std::cout << "Test 'matrix' z: " << regr.f_test_predictors(names,z)           << std::endl; print_time(start); std::cout << std::endl;
    
    std::cout << "\n       Z[0] = " << Z.col(0).head(5).transpose() << " ... " << std::endl;
    start = clock(); std::cout << "Test of Z[0]     : " << regr.f_test_predictor ("Z[0]",Z.col(0))   << std::endl; print_time(start);
    start = clock(); std::cout << "Test of Z[0] (s) : " << regr.f_test_predictors(names, Z.col(0))   << std::endl; print_time(start);

    start = clock(); std::cout << "White Z[0],b=1    : " << regr.f_test_predictor ("Z[0]",Z.col(0)) << std::endl; print_time(start);
    start = clock(); std::cout << "White Z[0],b=1 (s): " << regr.f_test_predictors(names,Z.col(0)) << std::endl; print_time(start);

    start = clock(); std::cout << "Test of Z      : " << regr.f_test_predictors(zNames, Z)         << std::endl; print_time(start);
    
    start = clock(); std::cout << "Adding 1 pred  : ";  regr.add_predictors(names, z);             print_time(start);  std::cout << regr << std::endl;
    start = clock(); std::cout << "Adding k preds : ";  regr.add_predictors(zNames, Z);            print_time(start);  std::cout << regr << std::endl;
  }

  */

  // test shrinkage
  /*
    {
    std::cout << "\nTest: shrinkage in regression; refit models with shrinkage in place.\n";
    LinearRegression sregr("y", y);
    sregr.add_predictors(xNames, X);
    std::cout << "Shrinking 1 pred  : ";
    sregr.add_predictor("Z[0]", Z.col(0),sregr.f_test_predictor(Z.col(0)));    std::cout << sregr << std::endl;
    std::cout << "Shrinking 2 pred  : ";
    std::vector<std::string> twoNames; twoNames.push_back("Z[1]"); twoNames.push_back("Z[2]");
    sregr.add_predictors(twoNames, Z.block(0,1,Z.rows(),Z.cols()-1),sregr.f_test_predictors(Z.block(0,1,Z.rows(),Z.cols()-1)));  std::cout << sregr << std::endl;
  }
  */
  
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
