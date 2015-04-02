// -*- c++ -*-
#include "eigen_base_types.h"
#include "linear_regression.h"
#include "fstatistic.h"

#include "debug.h"

#include <vector>
#include <iostream>
#include <sstream>
#include <fstream>
#include <algorithm>

#include <time.h>

using std::cout;
using std::endl;
using std::string;

// #define USE_WLS

std::ostream&
operator<<(std::ostream& os, std::pair<SCALAR,SCALAR> p)
{
  os << " <" << p.first << "," << p.second << "> ";
  return os;
}

void print_time(time_t const& start)
{
  cout << "Time = " << double(clock() - start)/CLOCKS_PER_SEC << endl;
}


//     main     main     main     main     main     main     main     main     main     main     main     main     

int main(int, char **)
{
  typedef LinearRegression::Scalar Scalar;
  typedef LinearRegression::Vector Vector;
  typedef LinearRegression::Matrix Matrix;
  
  cout << "TEST:  Test of linear regression begins...\n\n";
  debugging::debug_init(cout,2);
  cout.precision(4);
  
  // Time to fit regr with n = 200,000 rows  (optimization O4)       Add 3 to initial p
  //                          Eigen 2 (b=0)    Eigen 3,MGS,b=1          Eigen 3,MGS,b=1
  //                          13"     17"       13      17               13      17
  //   p = 3 predictors      0.09 s  0.06      0.02   0.013              0.10    0.07
  //      20                 1.3     0.8       0.04   0.025              0.23    0.15
  //      40                         2.6       0.06   0.035              0.40    0.24
  //      80                        10.5       0.10   0.060              0.78    0.44

  const int nRows       ( 300000 );   
  const int nCols       (      5 );
  const int nAdd        (    400 );
  
  // form random matrix for response and predictors
  std::cout << "TEST: building " << nAdd << " input data vectors of length " << nRows << " to add after fit initial model with " << nCols << "\n";
  Vector y  (Vector::Random(nRows));
  Vector z  (Vector::Random(nRows));
  Vector w  (Vector::Zero  (nRows));
  Matrix X  (Matrix::Random(nRows,nCols));
  Matrix Z  (Matrix::Random(nRows,nAdd));

  // shift the y's by 100 to check ordering in train/test data
  for(int i=0; i<nRows; ++i)
    y[i] = y[i] + 100;
  // add some signal to the Z's for checking validation    (this puts some huge leverage points into the data)
  Z(0,0) = y(0);
  Z(1,1) = y(1);
  Z(2,2) = y(2);
  
#ifdef USE_WLS
  // define the weight vector
  for (int i=0; i<nRows; ++i)
    w[i] = 10.0; //    w[i] = 1.0 + i % 4;
  cout << "TEST: First 10 of weight vector = " << w.head(10).transpose() << endl;
#endif
  
  // names for variables
  std::cout << "TEST: Adding names for the input variables. \n";
  std::vector<string> xNames;
  std::vector<string> zNames;
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
  if (false && nRows < 1000)
  { Matrix data(nRows,1+nCols+nAdd);
    data << y , X , Z; 
    cout << "TEST:  Writing data in external order as created, first four rows are\n" << data.topRows(4) << endl;
    std::string fileName ("regr_test_data.txt");
    std::ofstream output(fileName.c_str());
    output << "y";
    for(int i=0; i<X.cols(); ++i) output << "\tX_" << i;
    for(int i=0; i<Z.cols(); ++i) output << "\tZ_" << i;
    output.precision(7);
    output << std::endl << data << endl;
  }

    
  if (true)
  { cout << "\n\nTEST: basic test of the linear regression routine, adding variables one at a time." << endl;

    Scalar mean (y.sum()/(Scalar)y.size());
    cout << "TEST:  y-bar is " << mean << endl;
    cout << "       y        " << y(0) << "  " << y(1) << "  " << y(2) << endl;
    cout << "       centered " << y(0)-mean << "  " << y(1)-mean << "  " << y(2)-mean << endl;

    const int blockSize = 0;
    const size_t omegaDimension = 10;
#ifdef USE_WLS
    LinearRegression regr("yyy", y, w, blockSize);
#else
    LinearRegression      regr("yyy", y, blockSize);
    FastLinearRegression fRegr("yyy", y, blockSize, omegaDimension);
#endif
    cout << "TEST: Initialized regression " << endl << regr << endl;
    cout << "TEST: Initial beta = " << regr.beta().transpose() << "    gamma = " << regr.gamma().transpose() << endl;
    cout << "TEST: Residuals (first 10) = " << regr.raw_residuals().head(10).transpose() << endl << endl;
    cout << " -------- \n";
    cout << "TEST: Initialized fast regression " << endl << fRegr << endl;
    cout << "TEST: Initial fast gamma = " << fRegr.gamma().transpose() << endl;
    cout << "TEST: Residuals fast (first 10) = " << fRegr.raw_residuals().head(10).transpose() << endl << endl;
    cout << "\n========================================================================== \n";
    
    cout << "TEST: F test of X[0] " << regr.f_test_predictor("X[0]", X.col(0)) << endl;
    regr.add_predictors();
    cout << "TEST: regression after adding X[0] " << endl << regr << endl;
    cout << "TEST: Beta     = " << regr.beta().transpose() << endl;
    cout << "TEST: se(beta) = " << regr.se_beta().transpose() << endl;
    cout << "TEST: Residuals (first 10) = " << regr.raw_residuals().head(10).transpose() << endl << endl;
    cout << " -------- \n";
    cout << "TEST: F test of X[0] " << fRegr.f_test_predictor("X[0]", X.col(0)) << endl;
    fRegr.add_predictors();
    cout << "TEST: regression after adding X[0] " << endl << fRegr << endl;
    cout << "TEST: Beta     = " << fRegr.beta().transpose() << endl;
    cout << "TEST: se(beta) = " << fRegr.se_beta().transpose() << endl;
    cout << "TEST: Residuals (first 10) = " << fRegr.raw_residuals().head(10).transpose() << endl << endl;
    cout << "\n========================================================================== \n";

    cout << "TEST: F test of X[1]" << regr.f_test_predictor("X[1]", X.col(1)) << endl;
    regr.add_predictors();
    cout << "TEST: regression after adding X[1] " << endl << regr << endl;
    cout << "TEST: Gamma  = " << regr.gamma().transpose() << endl;
    cout << "TEST: se(gamma) = " << regr.se_gamma().transpose() << endl;
    cout << "TEST: Residuals (first 10) = " << regr.raw_residuals().head(10).transpose() << endl << endl;
    cout << " -------- \n";
    cout << "TEST: F test of X[1]" << fRegr.f_test_predictor("X[1]", X.col(1)) << endl;
    fRegr.add_predictors();
    cout << "TEST: fRegression after adding X[1] " << endl << fRegr << endl;
    cout << "TEST: Gamma  = " << fRegr.gamma().transpose() << endl;
    cout << "TEST: se(gamma) = " << fRegr.se_gamma().transpose() << endl;
    cout << "TEST: Residuals (first 10) = " << fRegr.raw_residuals().head(10).transpose() << endl << endl;
    cout << "\n========================================================================== \n";

    cout << "TEST: Several rows of X" << endl;
    for (int i=0; i<10; ++i)
      cout << "      [" << i << "]   " << regr.x_row(i).transpose() << endl;
    cout << "TEST: F test of adding X[1] again " << regr.f_test_predictor("X1 again", X.col(1)) << endl;    // ??? Why does it not detect singular
    regr.add_predictors();
    cout << "TEST: regression after adding X[1] a second time " << endl << regr << endl;
    cout << "TEST: Beta  = " << regr.beta().transpose() << endl;
    cout << "TEST: se(beta) = " << regr.se_beta().transpose() << endl;
    cout << "TEST: Residuals (first 10) = " << regr.raw_residuals().head(10).transpose() << endl << endl;
    cout << "TEST: R matrix of the internal Q matrix (as check for orthogonality)...\n" << regr.check_orthogonality_matrix() << endl;
    cout << "\n========================================================================== \n";

    {
      clock_t beginTime, endTime;
      double timeRegr=0, timeFast=0;
      for (int j=1; j<Z.cols(); ++j)
      { beginTime = clock();
	FStatistic f  =  regr.f_test_predictor("Z["+std::to_string(j)+"]", Z.col(j));
	regr.add_predictors();
	endTime = clock();
	timeRegr += (double)(endTime - beginTime);
	beginTime = clock();
	FStatistic ff = fRegr.f_test_predictor("Z["+std::to_string(j)+"]", Z.col(j));
	fRegr.add_predictors();
	endTime = clock();
	timeFast += (double)(endTime - beginTime);
	std::cout << "TEST: For j = " << j << " F stats are " << f.f_stat() << " and " << ff.f_stat() << endl;
      }
      std::cout << "TEST: time regr = " << timeRegr << "  time fast = " << timeFast << std::endl;
    }
  }

  
  if (true)  // second test, adding X bundle at once
  {
#ifdef USE_WLS
    LinearRegression regr("yyy", y, w, 0);
#else
    LinearRegression regr("yyy", y, 0);
#endif
    cout << "TEST: Initialized regression " << endl << regr << endl;
    cout << "TEST: F test of X " << regr.f_test_predictors(xNames, X) << endl;
    regr.add_predictors();
    cout << "TEST: regression after adding X " << endl << regr << endl;
    cout << "TEST: Beta  = " << regr.beta().transpose() << endl;
    cout << "      SE    = " << regr.se_beta_ols().transpose() << endl;
    cout << "TEST: Residuals (first 10) = " << regr.residuals().head(10).transpose() << endl << endl;
    cout << "TEST: Several rows of X" << endl;
    for (int i=0; i<10; ++i)
      cout << "      [" << i << "]   " << regr.x_row(i).transpose() << endl;
  }

  return 0;
}
