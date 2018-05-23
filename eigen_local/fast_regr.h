// -*- c++ -*-
#ifndef _EIGEN_REGRESSION_H_
#define _EIGEN_REGRESSION_H_

#include "eigen_base_types.h"
#include "fstatistic.h"

#include <Eigen/Core>

#include <iostream>


/*

  FastRegression objects perform a randomized partial sweep rather than
  a full sweep of each explanatory variable.

  This code is *not* weighted and does not implement blocks or shrinakge.

  Its meant to be fast and allow large models.
  
*/

class FastRegression
{
public:
  typedef SCALAR                   Scalar;
  typedef VECTOR                   Vector;
  typedef MATRIX                   Matrix;
  typedef FStatistic               FStat;
  typedef std::vector<std::string> StringVec;
  
private:
  size_t                   mN;             // number of rows
  size_t                   mK;             // number of columns (including constant) in the model
  size_t                   mQdim;          // dim of retained projection matrix
  std::string              mYName; 
  StringVec                mXNames;
  Vector                   mY;             // original response
  bool                     mBinary;        // y is in {0,1}
  Vector                   mGamma;         // coefficients of the orthogonal regression
  Vector                   mResiduals;
  Matrix                   mQ, mQO;        // random projection of used features, orthogonal version
  StringVec mutable        mTempNames;
  int       mutable        mTempK;         // number of predictors last tried, these are last mTempDim columns of Q
  Scalar                   mResidualSS;
  Scalar                   mTotalSS;
  
public:
  ~FastRegression () {  }

  FastRegression ()
    :  mN(0),mK(0),mQdim(0),mBlockSize(0) { }
  
  LinearRegression (std::string yName, Vector const& y, size_t qDim) 
    : mN((int)y.size()), mK(0), mQdim(qDim),
      mYName(yName), mXNames(), mY(y), mBinary(is_binary_vector(y)) { allocate_memory(); add_constant(); }
  
  bool      is_binary()         const   { return mBinary; }
  int       n()                 const   { return mN; };
  int       q()                 const   { return mK-1; }                                     // -1 for intercept 
  Scalar    rmse()              const   { return (Scalar) sqrt(mResidualSS/((Scalar)(mN-mK))); }  
  Scalar    residual_ss()       const   { return mResidualSS; }
  Scalar    aic_c()             const   { Scalar n((Scalar)mN), k((Scalar)mK); return (Scalar) n*(Scalar)log(mResidualSS/n) + (n+k)/(1-(k+2)/n); } // hurvich89,p300
  Scalar    r_squared()         const   { return (Scalar) 1.0 - mResidualSS/mTotalSS; }
  Scalar    adj_r_squared()     const   { return (Scalar) 1.0 - (mResidualSS/(Scalar)(mN-mK)) / (mTotalSS/(Scalar)(mN-1)); }
  Vector    y()                 const   { return mY; }
  Vector    residuals()         const   { return mResiduals; }
  Vector    fitted_values()     const   { return mY - mResiduals; }
  StringVec x_names()           const   { return mXNames; }
  Scalar    y_bar()             const   { return mY.sum()/mN; }
  Vector    gamma()             const   { return mGamma.head(mK); }
  Vector    se_gamma()          const;
  
  FStat     f_test_predictor  (std::string name, Vector const& z)       const;    // <f,pval>  f == 0 implies singular; uses Bennett if binary
  FStat     f_test_predictors (StringVec const& names, Matrix const& z) const;    //           blocksize > 0 implies blocked white.
  
  void      add_predictors ();                                                    // no shrinkage
  void      add_predictors (StringVec const& names, Matrix const& x);             // adds with no testing
  void      add_predictors (FStat const& fstat);
  
  void      print_to               (std::ostream& os, bool compact=false)         const;
  void      print_gamma_to         (std::ostream& os)                             const;
  void      compact_print_gamma_to (std::ostream& os, std::vector<size_t>indices) const;
  void      write_data_to          (std::ostream& os, int maxNumXCols)            const; // JMP style, with y followed by X columns (tab delimited) 
  
 private:
  void      allocate_memory();
  void      add_constant();
  Scalar    sweep_Q_from_column(int col)           const;              // only affect Q, R past those of current fit
  void      update_fit(StringVec xNames);
  bool      is_invalid_ss (Scalar ss, Scalar ssz)  const;              // checks for singularity, nan, neg, inf

  Vector squared_norm (Matrix const& a)                  const { return ((a.array() * a.array()).colwise().sum()); } // diagonal of a'a
};

#endif
