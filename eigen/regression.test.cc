// -*- c++ -*-
#include "regression.h"
#include "debug.h"

#include <vector>
#include <iostream>
#include <sstream>
#include <fstream>
#include <algorithm>

#include <time.h>


using std::cout;
using std::endl;


// #define USE_WLS

std::ostream&
operator<<(std::ostream& os, std::pair<double,double> p)
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
  cout << "TEST:  Test of regression begins...\n\n";
  debugging::debug_init(cout,2);  // Debug level 1 avoids most messages; raise number for more
  cout.precision(4);
  
  // Time to fit regr with n = 200,000 rows  (optimization O4)       Add 3 to initial p
  //                          Eigen 2 (b=0)    Eigen 3,MGS,b=1          Eigen 3,MGS,b=1
  //                          13"     17"       13      17               13      17
  //   p = 3 predictors      0.09 s  0.06      0.02   0.013              0.10    0.07
  //      20                 1.3     0.8       0.04   0.025              0.23    0.15
  //      40                         2.6       0.06   0.035              0.40    0.24
  //      80                        10.5       0.10   0.060              0.78    0.44

  const int nRows       (1000 );   
  const int nCols        (  3 );
  const int nAdd         ( 10 );
  
  // form random matrix for response and predictors
  Eigen::VectorXd y  (Eigen::VectorXd::Random(nRows));
  Eigen::VectorXd z  (Eigen::VectorXd::Random(nRows));
  Eigen::VectorXd w  (Eigen::VectorXd::Zero  (nRows));
  Eigen::MatrixXd X  (Eigen::MatrixXd::Random(nRows,nCols));
  Eigen::MatrixXd Z  (Eigen::MatrixXd::Random(nRows,nAdd));

  // shift the y's by 100 to check ordering in train/test data
  for(int i=0; i<nRows; ++i)
    y[i] = y[i] + i * 100;
  
#ifdef USE_WLS
  // define the weight vector
  for (int i=0; i<nRows; ++i)
    w[i] = 10.0; //    w[i] = 1.0 + i % 4;
  cout << "TEST: First 10 of weight vector = " << w.head(10).transpose() << endl;
#endif
  
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
    cout << "TEST:  Writing data in external order as created, first four rows are\n" << data.topRows(4) << endl;
    std::string fileName ("/Users/bob/Desktop/test.txt");
    std::ofstream output(fileName.c_str());
    output.precision(7);
    output << data << endl;
  }

    
  if (false)
  { cout << "\n\nTEST: basic test of the linear regression routine, adding variables one at a time." << endl;

    double mean (y.sum()/y.size());
    cout << "TEST:  y-bar is " << mean << endl;
    cout << "       y        " << y(0) << "  " << y(1) << "  " << y(2) << endl;
    cout << "       centered " << y(0)-mean << "  " << y(1)-mean << "  " << y(2)-mean << endl;

#ifdef USE_WLS
    LinearRegression regr("yyy", y, w, 0);
#else
    LinearRegression regr("yyy", y, 0);
#endif
    cout << "TEST: Initialized regression " << endl << regr << endl;
    cout << "TEST: Initial beta = " << regr.beta().transpose() << "    gamma = " << regr.gamma().transpose() << endl;
    cout << "TEST: Residuals (first 10) = " << regr.raw_residuals().head(10).transpose() << endl << endl;
    
    cout << "TEST: F test of X[0] " << regr.f_test_predictor("X[0]", X.col(0)) << endl;
    regr.add_predictors();
    cout << "TEST: regression after adding X[0] " << endl << regr << endl;
    cout << "TEST: Beta     = " << regr.beta().transpose() << endl;
    cout << "TEST: se(beta) = " << regr.se_beta().transpose() << endl;
    cout << "TEST: Residuals (first 10) = " << regr.raw_residuals().head(10).transpose() << endl << endl;

    cout << "TEST: F test of X[1]" << regr.f_test_predictor("X[1]", X.col(1)) << endl;
    regr.add_predictors();
    cout << "TEST: regression after adding X[1] " << endl << regr << endl;
    cout << "TEST: Beta  = " << regr.beta().transpose() << endl;
    cout << "TEST: se(beta) = " << regr.se_beta().transpose() << endl;
    cout << "TEST: Residuals (first 10) = " << regr.raw_residuals().head(10).transpose() << endl << endl;

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
  }


  if (false)  // second test, adding X bundle at once
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

    
  if(true)   // test threads for regression
  { cout << "TEST: Testing threads code for CV." << endl;
    int nFolds = 5;
    Eigen::MatrixXd results(Z.cols(), 4);
    validate_regression( y, X, Z, nFolds, results);
    cout << "TEST: Thread results for columns R2, RSS, AICc, and CVSS\n" << results << endl;
  }
  
  // --------------  from here below
  //     check validation model (dup validation and estimation cases)
  // --------------
  if (false)
  { clock_t start;
    // assemble data
    cout << "TEST: Testing validated model\n";
    bool   cv[2*nRows];
    double yPtr[2*nRows];
    double wts[2*nRows];
    for(int i=0; i<nRows; ++i)
    { yPtr[2*i] = y(i); yPtr[2*i+1] = y(i);
      cv[2*i] = true; cv[2*i+1]=false;        // weave the training and test data
      wts[2*i] = w[i] = 10.0;
    }
    cout << "TEST: Initial weights are " << wts[0] << " " << wts[1] << " " << wts[2] << endl;
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
  
    if(false) //  validated regression, standard F test
    { cout << "\n\n-----------------------------------------------------------------------------------\nTEST: test of standard F p-values\n";
#ifdef USE_WLS
      ValidatedRegression vregr("Y", yPtr, cv, wts, 2*nRows, 0, true);
#else
      ValidatedRegression vregr("Y", yPtr, cv,      2*nRows, 0, true);
#endif
      cout << vregr << endl;
      std::pair<double,double> result;
      // force to add by setting p-value threshold to 1
      result = vregr.add_predictors_if_useful (xcollection, 1.0);
      cout << "TEST: test of adding xcollection gives " << result << endl << vregr << endl;
      double *pFit  (new double[2*nRows]);
      vregr.fill_with_fit(pFit);
      cout << "TEST: First 10 fitted values from model with 3 X's are   ";
      std::for_each(pFit, pFit+10, [](double x){ cout << x << "  ";});
      cout << endl;
      // test where does not add
      std::vector<std::pair<std::string, double*> > collect1;
      collect1.push_back(zcollection[0]);
      start = clock(); result = vregr.add_predictors_if_useful (collect1,0.0000001); print_time(start);
      cout << "TEST: test of adding z[0] to x[0,1,2] model gives " << result << endl << vregr << endl;      
      // force to add all Z's
      start = clock(); result = vregr.add_predictors_if_useful (zcollection, 1.0); print_time(start);
      cout << "TEST: test of adding zcollection gives " << result << endl << vregr << endl;
      // test the writing of data
      //      vregr.write_data_to (cout);
    }
    
    if(false) // validated regression, white tests
    { bool shrink (false);
      const int blockSize (1);
      ValidatedRegression vregr("Y", yPtr, cv, 2*nRows, blockSize, shrink);
      cout << "\n\n----------------------------------------------------------------------\nTEST: check white p-values with block size " << blockSize << "\n";
      cout << vregr << endl;
      std::pair<double,double> result;
      result = vregr.add_predictors_if_useful (xcollection, 1.0);
      cout << "TEST: test of adding initial xcollection gives " << result << endl << vregr << endl;
      {
	std::vector<std::pair<std::string, double*> > collect1;
	collect1.push_back(zcollection[0]);
	start=clock(); result = vregr.add_predictors_if_useful (collect1,0.00001); print_time(start);
      }
      cout << "TEST: test of adding z[0] to x[0,1,2] model gives " << result << endl << vregr << endl;
      start=clock(); result = vregr.add_predictors_if_useful (zcollection, 1.0); print_time(start);
      cout << "TEST: test of adding zcollection gives " << result << endl << vregr << endl;
    }

    if(false)    // test validated regression, white tests
    { bool shrink (false);
      const int blockSize (5);
      ValidatedRegression vregr("Y", yPtr, cv, 2*nRows, blockSize, shrink);
      cout << "\n\n--------------------------------------------------------------------\nTEST: check white p-values with block size " << blockSize << "\n";
      cout << vregr << endl;
      std::pair<double,double> result;
      result = vregr.add_predictors_if_useful (xcollection, 1.0);
      cout << "TEST: test of adding initial xcollection gives " << result << endl << vregr << endl;
      {
	std::vector<std::pair<std::string, double*> > collect1;
	collect1.push_back(zcollection[0]);
	start=clock(); result = vregr.add_predictors_if_useful (collect1,0.00001); print_time(start);
      }
      cout << "TEST: test of adding z[0] to x[0,1,2] model gives " << result << endl << vregr << endl;
      start=clock(); result = vregr.add_predictors_if_useful (zcollection, 1.0); print_time(start);
      cout << "TEST: test of adding zcollection gives " << result << endl << vregr << endl;
    }
  }
  

  return 0;
}
