// -*- c++ -*-

#ifndef _EIGEN_REGRESSION_H_
#define _EIGEN_REGRESSION_H_

// f test
#include "stat_utils.h"

#include <Eigen/Array>
#include <Eigen/QR>

#include <iostream>




/*
  30 Sep 2010 ... Playing with how to do shrinkage reasonably.
  24 Sep 2010 ... Created to start removing GSL components


  Issues that must be handled...


  - Weaving data in/out to separate training and test data
  - Sampling/variance weights, WLS
  - Bennett for 0/1 or bounded response

*/

class FStatistic
{
  typedef Eigen::VectorXd Vector;
  
private:
  double   mF;
  int      mNumDF, mDenDF;
  double   mPValue;
  Vector   mSSx;             // length mNumDF
  
public:
  ~ FStatistic() {}
  
  FStatistic()                                            // use empty version to signal singular
    : mF(0.0), mNumDF(0), mDenDF(0), mPValue(1.0), mSSx(Vector::Zero(1)) { }
  
  FStatistic(double f, int numDF, int denDF, Vector const& ssx)
    : mF(f), mNumDF(numDF), mDenDF(denDF), mSSx(ssx) { assert(ssx.size()==numDF); calc_p_value(); }
  
  FStatistic(double numSS, int numDF, double denSS, int denDF, Vector ssx)
    : mF((numSS/(double)numDF)/(denSS/(double)denDF)), mNumDF(numDF), mDenDF(denDF), mSSx(ssx) { assert(ssx.size()==numDF); calc_p_value(); }
  
  double f_stat()                   const { return mF; }
  double p_value()                  const { return mPValue; }
  Vector sum_of_squares()           const { return mSSx; }
  double critical_value(double p)   const;
  
  void   print_to(std::ostream& os) const { os << "F(" << mNumDF << "," << mDenDF << ") = " << mF << " (p=" << mPValue <<") { ss = " << mSSx.transpose() << " } "; }
  
private:
  void calc_p_value(); 
};


inline
std::ostream&
operator<<(std::ostream& os, FStatistic const& fStat)
{
  fStat.print_to(os);
  return os;
}




class LinearRegression
{
  typedef Eigen::VectorXd Vector;
  typedef Eigen::MatrixXd Matrix;
  typedef FStatistic      FStat;
  
private:
  int     mN;          // number of obs without pseudorows used for shrinkage
  Vector  mY;          // padded for shrinkage
  Matrix  mX;          // padded for shrinkage; shrinkage elements on diagonal
  Matrix  mQ;          // decomp of X (so padded too)
  Matrix  mR;
  Vector  mResiduals;
  double  mResidualSS;
  double  mTotalSS;
  
public:
  ~LinearRegression () { }

  LinearRegression (Vector const& y, Matrix const& x) :  mN(y.size()), mY(pad_vector(y,x.cols()+1)), mX(insert_constant(x)) { initialize(); }

  
  int       n()                      const   { return mN; };
  int       p()                      const   { return mX.cols()-1; }                      // -1 for intercept 
  double    rmse()                   const   { return sqrt(mResidualSS/(mN-mX.cols())); }
  double    r_squared()              const   { return 1.0 - mResidualSS/mTotalSS; }

  Vector    residuals()              const   { return mResiduals.start(mN); }  
  Vector    fitted_values()          const   { return mY.start(mN) - mResiduals.start(mN); }
  Vector    predict(Matrix const& x) const;
  
  Vector    beta()                   const;
  Vector    se_beta()                const;

  FStat     f_test_predictor  (Vector const& z, int blockSize = 0) const;                // <f,pval>  f == 0 implies singular; blocksize>0 for white
  FStat     f_test_predictors (Matrix const& z, int blockSize = 0) const; 

  void      add_predictor  (Vector const& z)                      { add_predictors(z, FStatistic()); } // no shrinkage
  void      add_predictor  (Vector const& z, FStat const& fstat)  { add_predictors(z, fstat); }
  void      add_predictors (Matrix const& z)                      { add_predictors(z,FStatistic()); }   // no shrinkage
  void      add_predictors (Matrix const& z, FStat const& fstat);
  
  void print_to (std::ostream& os) const;

 private:
  Vector pad_vector(Vector const& v, int k) const;
  Matrix insert_constant(Matrix const& m) const;    // stuffs a 1 as first column
  void initialize();                                // sets initial SS, calls orthgonalize
  void build_QR_and_residuals();                    // does the QR and finds residuals
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
