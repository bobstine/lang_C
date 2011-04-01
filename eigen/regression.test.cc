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
  debugging::debug_init(std::cout,3);
  std::cout.precision(4);  

  // Timing to build regr with n = 200,000 rows  (O4)        Add 3 to initial p
  //                          13"     13/GS(b=1)  17"           13/GS
  //   p = 3 predictors      0.09 s   0.02       0.06            0.10
  //      20                 1.3      0.04       0.8             0.23
  //      40                          0.06       2.6             0.40
  //      80                          0.10      10.5             0.78

  const int nRows    (200);   
  const int nCols     ( 3);
  const int nAdd      ( 3);
  
  // form random matrix for response and predictors
  Eigen::VectorXd y  (Eigen::VectorXd::Random(nRows));
  Eigen::VectorXd z  (Eigen::VectorXd::Random(nRows));
  Eigen::VectorXd w  (Eigen::VectorXd::Zero  (nRows));
  Eigen::MatrixXd X  (Eigen::MatrixXd::Random(nRows,nCols));
  Eigen::MatrixXd Z  (Eigen::MatrixXd::Random(nRows,nAdd));

  // shift the y's by 100 to check ordering in train/test data
  for(int i=0; i<nRows; ++i)
    y[i] = y[i] + i * 100;
  
  // define the weight vector
  for (int i=0; i<nRows; ++i)
    w[i] = 1.0 + i % 4;
  std::cout << "TEST: First 10 of weight vector = " << w.head(10).transpose() << std::endl;

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

  //  write data so that can check in JMP/R
  if (nRows < 1000)
  { Eigen::MatrixXd data(nRows,1+nCols+nAdd);
    data << y , X , Z; 
    std::cout << "TEST:  Writing data in external order as created, first four rows are\n" << data.topRows(4) << std::endl;
    std::string fileName ("test.dat");
    std::ofstream output(fileName.c_str());
    output.precision(7);
    output << data << std::endl;
  }


  
  if (false)  // basic test of the linear regression routine, adding variables one at a time
  {
    double mean (y.sum()/y.size());
    std::cout << "TEST:  y-bar is " << mean << std::endl;
    std::cout << "       y        " << y(0) << "  " << y(1) << "  " << y(2) << std::endl;
    std::cout << "       centered " << y(0)-mean << "  " << y(1)-mean << "  " << y(2)-mean << std::endl;

    LinearRegression regr("yyy", y, 0);
    std::cout << "TEST: Initialized regression " << std::endl << regr << std::endl;
    std::cout << "TEST: Initial beta = " << regr.beta().transpose() << "    gamma = " << regr.gamma().transpose() << std::endl;
    std::cout << "TEST: Residuals (first 10) = " << regr.residuals().head(10).transpose() << std::endl << std::endl;
    
    std::cout << "TEST: F test of X[0] " << regr.f_test_predictor("X[0]", X.col(0)) << std::endl;
    regr.add_predictors();
    std::cout << "TEST: regression after adding X[0] " << std::endl << regr << std::endl;
    std::cout << "TEST: Beta  = " << regr.beta().transpose() << std::endl;
    std::cout << "TEST: Residuals (first 10) = " << regr.residuals().head(10).transpose() << std::endl << std::endl;

    std::cout << "TEST: F test of X[1]" << regr.f_test_predictor("X[1]", X.col(1)) << std::endl;
    regr.add_predictors();
    std::cout << "TEST: regression after adding X[1] " << std::endl << regr << std::endl;
    std::cout << "TEST: Beta  = " << regr.beta().transpose() << std::endl;
    std::cout << "TEST: Residuals (first 10) = " << regr.residuals().head(10).transpose() << std::endl << std::endl;

    std::cout << "TEST: Several rows of X" << std::endl;
    for (int i=0; i<10; ++i)
      std::cout << "      [" << i << "]   " << regr.x_row(i).transpose() << std::endl;

    std::cout << "TEST: F test of adding X[1] again " << regr.f_test_predictor("X1 again", X.col(1)) << std::endl;               // ??? Why does it not detect singularity
    regr.add_predictors();
    std::cout << "TEST: regression after adding X[1] a second time " << std::endl << regr << std::endl;
    std::cout << "TEST: Beta  = " << regr.beta().transpose() << std::endl;
    std::cout << "TEST: Residuals (first 10) = " << regr.residuals().head(10).transpose() << std::endl << std::endl;
  }


  if (false)  // second test, adding 3 at once
  {
    LinearRegression regr("yyy", y, 0);
    std::cout << "TEST: Initialized regression " << std::endl << regr << std::endl;
    
    std::cout << "TEST: F test of X " << regr.f_test_predictors(xNames, X) << std::endl;
    regr.add_predictors();
    std::cout << "TEST: regression after adding X " << std::endl << regr << std::endl;
    std::cout << "TEST: Beta  = " << regr.beta().transpose() << std::endl;
    std::cout << "      SE    = " << regr.se_beta_ols().transpose() << std::endl;
    std::cout << "TEST: Residuals (first 10) = " << regr.residuals().head(10).transpose() << std::endl << std::endl;

    std::cout << "TEST: Several rows of X" << std::endl;
    for (int i=0; i<10; ++i)
      std::cout << "      [" << i << "]   " << regr.x_row(i).transpose() << std::endl;
  }


  if (true)   // check validation model (dup validation and estimation cases)
  { clock_t start;
    // assemble data
    std::cout << "TEST: Testing validated model\n";
    bool   cv[2*nRows];
    double yPtr[2*nRows];
    for(int i=0; i<nRows; ++i)
    { yPtr[2*i] = y(i); yPtr[2*i+1] = y(i);
      cv[2*i] = true; cv[2*i+1]=false;        // weave the training and test data
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
      zcollection.push_back(make_pair(zNames[j],xPtr));
    }

    if(true) //  validated regression, standard F test
    { std::cout << "\n\n-----------------------------------------------------------------------------------\nTEST: test of standard F p-values\n";
      ValidatedRegression vregr("Y", yPtr, cv, 2*nRows, 0, true);
      std::cout << vregr << std::endl;
      std::pair<double,double> result;
      // force to add by setting p-value threshold to 1
      result = vregr.add_predictors_if_useful (xcollection, 1.0);
      std::cout << "TEST: test of adding xcollection gives " << result << std::endl << vregr << std::endl;
      double *pFit  (new double[2*nRows]);
      vregr.fill_with_fit(pFit);
      std::cout << "TEST: First 10 fitted values from model with 3 X's are   ";
      std::for_each(pFit, pFit+10, [](double x){ std::cout << x << "  ";});
      std::cout << std::endl;
      // test where does not add
      std::vector<std::pair<std::string, double*> > collect1;
      collect1.push_back(zcollection[0]);
      start = clock(); result = vregr.add_predictors_if_useful (collect1,0.0000001); print_time(start);
      std::cout << "TEST: test of adding z[0] to x[0,1,2] model gives " << result << std::endl << vregr << std::endl;      
      // force to add all Z's
      start = clock(); result = vregr.add_predictors_if_useful (zcollection, 1.0); print_time(start);
      std::cout << "TEST: test of adding zcollection gives " << result << std::endl << vregr << std::endl;
      // test the writing of data
      //      vregr.write_data_to (std::cout);
    }

    if(false) // validated regression, white tests
    { bool shrink (false);
      const int blockSize (1);
      ValidatedRegression vregr("Y", yPtr, cv, 2*nRows, blockSize, shrink);
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

    if(false)    // test validated regression, white tests
    { bool shrink (false);
      const int blockSize (5);
      ValidatedRegression vregr("Y", yPtr, cv, 2*nRows, blockSize, shrink);
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


  return 0;
}
