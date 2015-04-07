// -*- c++ -*-
#ifndef _LINEAR_REGRESSION_H_
#define _LINEAR_REGRESSION_H_

#include "eigen_base_types.h"
#include "little_functions.h"
#include "fstatistic.h"

#include <Eigen/Core>

#include <iostream>
#include <vector>

/*
  A LinearRegression object knows only about its data, and it uses
  all of the data to fit the model. External cases (X's with no y's)
  are carried in the optional last mTest rows of Q, but do not influence
  estimates of beta or gamma and do not appear in R.

  All regression models define a set of weights, and the software
  minimizes the sum w_i(y_i-y^_i)^2.  Weights initialized to 1's when
  input weights are not defined.

  25 Mar 2015 ... Add FastLinearRegression, forcing merged external X for validation
  25 Nov 2013 ... Introduction of CV using multithreads  
   4 Jul 2011 ... Use weighed version in all cases rather than two versions of code.
  20 Mar 2011 ... In place gram-schmidt branch; port to Eigen3
  14 Oct 2010 ... Split out fstatistic, using boost in place of gsl.
  30 Sep 2010 ... Playing with how to do shrinkage reasonably.
  24 Sep 2010 ... Created to start removing GSL components

  Issues that have *not* been handled...
    -- weighted with bennett
    -- logistic loss
    -- converting from data that is float to internal that is double *** conveniently ***.
*/

class LinearRegression
{
public:
  typedef SCALAR              Scalar;
  typedef VECTOR              Vector;
  typedef MATRIX              Matrix;
  typedef FStatistic          FStat;
  typedef std::string         string;
  typedef std::vector<string> StringVec;
  
protected:
  int                      mN;             // number of actual obs without pseudo-rows used for shrinkage
  int                      mTest;          // cases of X's reserved for validation testing
  int                      mK;             // number of columns (including constant) in the model
  int                      mBlockSize;     // 0 = ols, 1 = heteroscedastic white, 2+ for dependence
  std::string              mWeightStr;
  Vector                   mWeights;       // minimize sum w_i(y_i-y^_i)^2;  weight vec = 1 if not supplied (for ols, weight vector is a scalar)
  Vector                   mSqrtWeights;   // weights are only used for estimation (length mN)
  std::string              mYName; 
  StringVec                mXNames;
  Vector                   mY;             // original response, mN cases
  bool                     mBinary;        // y is in {0,1}
  Vector                   mLambda;        // shrinkage parameter
  Vector                   mGamma;         // coefficients of the orthogonal regression
  Vector                   mResiduals;     // only for the mN estimation cases
  Matrix    mutable        mQ, mR;         // Q holds mN+mTest cases; only changes are in the latter columns past Kth
  StringVec mutable        mTempNames;
  int       mutable        mTempK;         // number of predictors last tried, these are last mTempDim columns of Q
  Scalar                   mResidualSS;
  Scalar                   mTotalSS;
  
public:
  ~LinearRegression () {  }

  LinearRegression ()
    :  mN(0),mK(0),mBlockSize(0) { }
  
  LinearRegression (std::string yName, Vector const& y, int nTest, int blockSize) 
    : mN((int)y.size()), mTest(nTest), mK(0), mBlockSize(blockSize),
      mWeightStr(""), mWeights(Vector::Ones(mN)), mSqrtWeights(Vector::Ones(mN)),
      mYName(yName), mXNames(), mY(y), mBinary(is_binary_vector(y)) { allocate_memory(); add_constant(); }
  
  LinearRegression (std::string yName, Vector const& y, int nTest, std::vector<std::string> xNames, Matrix const& x, int blockSize)
    :  mN((int)y.size()), mTest(nTest), mK(0),  mBlockSize(blockSize),
       mWeightStr(""), mWeights(Vector::Ones(mN)), mSqrtWeights(Vector::Ones(mN)),
       mYName(yName), mXNames(), mY(y), mBinary(is_binary_vector(y)) { allocate_memory(); add_constant(); add_predictors(xNames, x); }
  
  // WLS: if weighted, all things held are weighted by square root of input weights in w
  LinearRegression (std::string yName, Vector const& y, int nTest, Vector const& w, int blockSize)
    :  mN((int)y.size()), mTest(nTest), mK(0), mBlockSize(blockSize), mWeightStr("Weighted "), mWeights(w), mSqrtWeights(w.array().sqrt()), mYName(yName),
       mY(y.array()*w.array().sqrt()), mBinary(is_binary_vector(y)) { allocate_memory(); add_constant(); }

  LinearRegression (std::string yName, Vector const& y, int nTest, std::vector<std::string> xNames, Matrix const& x, Vector const& w, int blockSize)
    :  mN((int)y.size()), mTest(nTest), mK(0), mBlockSize(blockSize), mWeightStr("Weighted "), mWeights(w), mSqrtWeights(w.array().sqrt()), mYName(yName), mXNames(),
       mY(y), mBinary(is_binary_vector(y)) { assert(nTest+mN==x.rows()); allocate_memory(); add_constant(); add_predictors(xNames,x); }  

  inline bool      has_binary_response()  const   { return mBinary; }
  inline int       block_size()           const   { return mBlockSize; }
  inline int       n()                    const   { return mN; };
  inline int       q()                    const   { return mK-1; }                                     // -1 for intercept 
  inline Scalar    rmse()                 const   { return (Scalar) sqrt(mResidualSS/((Scalar)(mN-mK))); }  
  inline Scalar    residual_ss()          const   { return mResidualSS; }
  inline Scalar    aic_c()                const   { Scalar n((Scalar)mN), k((Scalar)mK); return (Scalar) n*(Scalar)log(mResidualSS/n) + (n+k)/(1-(k+2)/n); } // hurvich89,p300
  inline Scalar    r_squared()            const   { return (Scalar) 1.0 - mResidualSS/mTotalSS; }
  inline Scalar    adj_r_squared()        const   { return (Scalar) 1.0 - (mResidualSS/(Scalar)(mN-mK)) / (mTotalSS/(Scalar)(mN-1)); }
  inline Vector    y()                    const   { return mY; }
  inline Vector    residuals()            const   { return mResiduals; }
  inline Vector    fitted_values()        const   { return mY - mResiduals; }
  inline Vector    raw_y()                const   { return mY.cwiseQuotient(mSqrtWeights); }                // raw versions remove the internal weights
  inline Vector    raw_residuals()        const   { return mResiduals.array()/mSqrtWeights.array(); }       //
  Vector           raw_fitted_values(bool truncate=false) const;                                             // truncated to soft limits on [0,1]
  Vector           x_row(int i)           const;
  Scalar           y_bar()                const   { if (mK>0) return (Scalar) sqrt(mN)*mGamma(0); else return 0.0; }
  Vector           gamma()                const   { return mGamma.head(mK); }
  Vector           se_gamma_ls()          const;
  Vector           se_gamma()             const;
  Vector           shrinkage_weights()    const   { return mLambda.head(mK); }
  Matrix const&    Q_basis_matrix()       const   { return mQ; }

  virtual bool  can_return_beta()           const   { return false; }
  Vector        beta()                      const;   // may not be available; check first with can_return_beta
  Vector        se_beta_ols()               const;
  Vector        se_beta()                   const;
  template <class Iter>
  void          fill_with_beta (Iter begin) const;
  
  StringVec predictor_names()                                               const   { return mXNames; }
  Vector    test_predictions  (bool truncate=false)                         const;    // truncate to soft limits on [0,1] using internal training cases
  Vector    predict           (Matrix const& matrix, bool truncate=false)   const;    // external matrix

  FStat     f_test_predictor  (std::string name, Vector const& z)       const;    // <f,pval>  f == 0 implies singular; uses Bennett if binary
  FStat     f_test_predictors (StringVec const& names, Matrix const& z) const;    //           blocksize > 0 implies blocked white.
  
  void      add_predictors ();                                                    // no shrinkage
  void      add_predictors (StringVec const& names, Matrix const& x);             // adds with no testing
  void      add_predictors (FStat const& fstat);
  
  void      print_to               (std::ostream& os, bool compact=false)         const;
  void      print_gamma_to         (std::ostream& os)                             const;
  void      compact_print_gamma_to (std::ostream& os, std::vector<size_t>indices) const;
  void      print_beta_to          (std::ostream& os)                             const;
  void      write_data_to          (std::ostream& os, int maxNumXCols, bool includeValidation) const; // JMP style, with y followed by X columns (tab delimited) 

  Matrix    check_orthogonality_matrix ()                         const; // returns r matrix from householder QR of internal G-S Q matrix
  
 private:
  void      allocate_memory();
  void      add_constant();
  std::pair<Scalar,Scalar> bennett_evaluation ()   const;              // 0/1 response only; operates on column mK (one past those in use)

protected:
  StringVec name_vec(std::string name)             const;              // inits a vector with one string
  bool      is_invalid_ss (Scalar ss, Scalar ssz)  const;              // checks for singularity, nan, neg, inf
  Scalar    approximate_ss(Vector const& x)        const;              // one-pass estimate of the SS around mean 

  virtual string output_label()                    const       { return "Linear Regression"; }
  virtual Scalar sweep_Q_from_column_and_normalize(int col)      const;              // only affect Q, R past those of current fit
  virtual   void update_fit(StringVec xNames);

  // idioms
  Vector squared_norm (Matrix const& a)                  const { return ((a.array() * a.array()).colwise().sum()); } // diagonal of a'a
};

inline
std::ostream&
operator<<(std::ostream& os, LinearRegression const& regr)
{
  os << "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -" << std::endl;
  regr.print_to(os);
  os << "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -" << std::endl;
  return os;
}


template <class Iter>
void LinearRegression::fill_with_beta (Iter begin) const
{
  Vector b (beta());
  for(int i=0; i<b.size(); ++i)
    *begin++ = b[i];
  return;
}

//     FastLinearRegression     FastLinearRegression     FastLinearRegression     FastLinearRegression     FastLinearRegression     


class FastLinearRegression : public LinearRegression
{
private:
  size_t    mOmegaDim;               // number of columns in random projection
  Matrix    mM;                      // random projection of predictors
  Matrix    mTtT;                    // 'square' of upper triangular portion of M = Q T
  size_t    mGradientPeriod;         // resweep after adding this many predictors (0 means ignore correction)
  size_t    mGradientCounter;

public:
  FastLinearRegression ()
    : LinearRegression() { }
  
  FastLinearRegression (std::string yName, Vector const& y, int nTest, int blockSize)                                                            // match signature of linear_regression
    : LinearRegression(yName, y, nTest, blockSize), mOmegaDim(10), mGradientPeriod(100), mGradientCounter(0) { allocate_projection_memory();  }    // 0 for no blocking; lock in omega dim

  void set_gradient_period (size_t period)    { mGradientPeriod = period; }
  
private:
  void           allocate_projection_memory();
  void           apply_gradient_correction();                         // sweeps all past predictors from residuals, updating mGamma        
  virtual string output_label()                             const   { return "Fast " + LinearRegression::output_label(); }
  virtual Scalar sweep_Q_from_column_and_normalize(int col) const;    // sweeps using M and T formed from random projection
  virtual   void update_fit(StringVec xNames);
};
  
#endif
