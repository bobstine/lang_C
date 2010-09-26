// -*- c++ -*-

#ifndef _EIGEN_REGRESSION_H_
#define _EIGEN_REGRESSION_H_

// f test
#include "stat_utils.h"

#include <Eigen/Array>
#include <Eigen/QR>

#include <iostream>




/*
  24 Sep 2010 ... Created to start removing GSL components



  Issues that must be handled...

  - Shrinkage, particularly when adding a subspace
  - Weaving data in/out to separate training and test data
  - White estimator for SEs
  - Sampling/variance weights, WLS
  - Bennett for 0/1 or bounded response

  - Data structure used when add more variables
  
*/

class LinearRegression
{
  typedef Eigen::VectorXd Vector;
  typedef Eigen::MatrixXd Matrix;

 private:
  Vector  mY;
  Matrix  mX;
  Matrix  mQ;
  Matrix  mR;
  Vector  mResiduals;
  double  mResidualSS;
  double  mTotalSS;
  
 public:
  ~LinearRegression () { }

  LinearRegression (Vector const& y, Matrix const& x) :  mY(y), mX(insert_constant(x)) { initialize(); }

  
  int       n()                      const   { return mX.rows(); };
  int       p()                      const   { return mX.cols()-1; }                      // -1 for intercept 
  double    rmse()                   const   { return sqrt(mResidualSS/(n()-mX.cols())); }
  double    r_squared()              const   { return 1.0 - mResidualSS/mTotalSS; }

  Vector    residuals()              const   { return mResiduals; }
  Vector    fitted_values()          const   { return mY - mResiduals; }
  
  Vector    predict(Matrix const& x) const;
  
  Vector    beta()                   const;
  Vector    se_beta()                const;

  std::pair<double,double>  f_test_predictor  (Vector const& z, int blockSize = 0) const;   // <f,pval>  f == 0 implies singular; blocksize>0 for white
  std::pair<double,double>  f_test_predictors (Matrix const& z, int blockSize = 0) const; 
  
  
  void print_to (std::ostream& os) const;

 private:
  Matrix insert_constant(Matrix const& m) const;    // stuffs a 1 as first column
  void initialize();                                // factors X, computes residuals, SS
  
};

           
///////////////////////////  Printing Operators  /////////////////////////////

inline
std::ostream&
operator<<(std::ostream& os, LinearRegression const& regr)
{
  os << "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -" << std::endl;
  regr.print_to(os);
  os << "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -" << std::endl << std::endl;
  return os;
}

#endif
