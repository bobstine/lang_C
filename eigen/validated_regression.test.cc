// -*- c++ -*-
#include "eigen_base_types.h"
#include "validated_regression.Template.h"
#include "debug.h"

#include <vector>
#include <iostream>
#include <sstream>
#include <fstream>
#include <algorithm>

#include <time.h>

using std::cout;
using std::endl;

// use following to add weights
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
  typedef LinearRegression                        Regression;
  typedef ValidatedRegression<Regression>::Scalar Scalar;
  typedef ValidatedRegression<Regression>::Vector Vector;
  typedef ValidatedRegression<Regression>::Matrix Matrix;
  
  cout << "TEST:  Test of regression begins...\n\n";
  debugging::debug_init(cout,2);  // Debug level 1 avoids most messages; raise number for more
  cout.precision(4);
  
  const int nRows       ( 25000 );   
  const int nCols       (     5 );
  const int nAdd        (    25 );
  
  // form random matrix for response and predictors
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

  if(false)   // test threads for regression
  { cout << "\n\nTEST: Testing threads code for CV." << endl;
    int nFolds = 20;
    Matrix results(1+Z.cols(), 4);           // extra row for base model
    cross_validate_regression( y, X, Z, nFolds, results);
    cout << "TEST: Thread results for columns R2, RSS, AICc, and CVSS\n" << results << endl;
  }
  
  // --------------  from here below
  //     check validation model (dup validation and estimation cases)
  // --------------
  if (true)
  { clock_t start;
    // assemble data
    cout << "TEST: Testing validated model\n";
    bool   cv[2*nRows];
    Scalar yPtr[2*nRows];
    Scalar wts[2*nRows];
    for(int i=0; i<nRows; ++i)
    { yPtr[2*i] = y(i); yPtr[2*i+1] = y(i);
      cv[2*i] = true; cv[2*i+1]=false;        // weave the training and test data
      wts[2*i] = w[i] = 10.0;
    }
    cout << "TEST: Initial weights are " << wts[0] << " " << wts[1] << " " << wts[2] << endl;
    std::vector<std::pair<std::string, Scalar*> > xcollection;
    for(int j=0; j<nCols; ++j)
    { Scalar *xPtr = new Scalar [nRows*2];
      for(int i=0; i<nRows; ++i)
      { xPtr[2*i] = X(i,j);
	xPtr[2*i+1] = X(i,j);
      }
      xcollection.push_back(make_pair(xNames[j],xPtr));
    }
    std::vector<std::pair<std::string, Scalar*> > zcollection;
    for(int j=0; j<nAdd; ++j)
    { Scalar *xPtr = new Scalar [nRows*2];
      for(int i=0; i<nRows; ++i)
      { xPtr[2*i] = Z(i,j); xPtr[2*i+1] = Z(i,j); }
      zcollection.push_back(make_pair(zNames[j],xPtr));
    }
  
    if(true) //  validated regression, standard F test
    { cout << "\n\n-----------------------------------------------------------------------------------\nTEST: test of standard F p-values\n";
#ifdef USE_WLS
      ValidatedRegression<Regression> vregr("Y", yPtr, cv, wts, 2*nRows, 0, true);
#else
      ValidatedRegression<Regression> vregr("Y", yPtr, cv,      2*nRows, 0, true);
#endif
      cout << vregr << endl;
      std::pair<Scalar,Scalar> result;
      // force to add by setting p-value threshold to 1
      result = vregr.add_predictors_if_useful (xcollection, 1.0);
      cout << "TEST: test of adding xcollection gives " << result << endl << vregr << endl;
      Scalar *pFit  (new Scalar[2*nRows]);
      vregr.fill_with_fit(pFit);
      cout << "TEST: First 10 fitted values from model with 3 X's are   ";
      std::for_each(pFit, pFit+10, [](Scalar x){ cout << x << "  ";});
      cout << endl;
      // test where does not add
      std::vector<std::pair<std::string, Scalar*> > collect1;
      collect1.push_back(zcollection[0]);
      start = clock(); result = vregr.add_predictors_if_useful (collect1, (Scalar)0.0000001); print_time(start);
      cout << "TEST: test of adding z[0] to x[0,1,2] model gives " << result << endl << vregr << endl;      
      // force to add all Z's
      start = clock(); result = vregr.add_predictors_if_useful (zcollection, (Scalar)1.0); print_time(start);
      cout << "TEST: test of adding zcollection gives " << result << endl << vregr << endl;
      // test the writing of data is quite slow for big problems
      // vregr.write_data_to (cout, 100, true);
    }
    
    if(true) // validated regression, white tests
    { bool shrink (false);
      const int blockSize (1);
      ValidatedRegression<Regression> vregr("Y", yPtr, cv, 2*nRows, blockSize, shrink);
      cout << "\n\n----------------------------------------------------------------------\nTEST: check white p-values with block size " << blockSize << "\n";
      cout << vregr << endl;
      std::pair<Scalar,Scalar> result;
      result = vregr.add_predictors_if_useful (xcollection, 1.0);
      cout << "TEST: test of adding initial xcollection gives " << result << endl << vregr << endl;
      {
	std::vector<std::pair<std::string, Scalar*> > collect1;
	collect1.push_back(zcollection[0]);
	start=clock(); result = vregr.add_predictors_if_useful (collect1,(Scalar)0.00001); print_time(start);
      }
      cout << "TEST: test of adding z[0] to x[0,1,2] model gives " << result << endl << vregr << endl;
      start=clock(); result = vregr.add_predictors_if_useful (zcollection, 1.0); print_time(start);
      cout << "TEST: test of adding zcollection gives " << result << endl << vregr << endl;
    }
  }
  return 0;
}
