// -*- c++ -*-
#ifndef _EIGEN_REGRESSION_H_
#define _EIGEN_REGRESSION_H_

#include "base_types.h"
#include "fstatistic.h"
#include "eigen_iterator.h"
#include "confusion_matrix.h"

#include <Eigen/Core>

#include <iostream>
#include <vector>

/*

  A LinearRegression object knows only about its data, and it uses
  all of the data to fit the model.  A ValidatedRegression understands
  the concept that some data will be reserved for validation purposes.

  All regression models define a set of weights, and the software
  minimizes the sum w_i(y_i-y^_i)^2.  Weights initialized to 1's when
  input weights are not defined.

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



//     LinearRegression      LinearRegression      LinearRegression      LinearRegression      LinearRegression      LinearRegression      

class LinearRegression
{
public:
  typedef SCALAR                   Scalar;      // vs double
  typedef VECTOR                   Vector;
  typedef MATRIX                   Matrix;
  typedef FStatistic               FStat;
  typedef std::vector<std::string> StringVec;
  
private:
  int                      mN;             // number of actual obs without pseudo-rows used for shrinkage
  int                      mK;             // number of columns (including constant) in the model
  int                      mBlockSize;     // 0 = ols, 1 = heteroscedastic white, 2+ for dependence
  std::string              mWeightStr;
  Vector                   mWeights;       // minimize sum w_i(y_i-y^_i)^2;  weight vec = 1 if not supplied (for ols, weight vector is a scalar)
  Vector                   mSqrtWeights;
  std::string              mYName; 
  StringVec                mXNames;
  Vector                   mY;             // original response
  bool                     mBinary;        // y is in {0,1}
  Vector                   mLambda;        // shrinkage parameter
  Vector                   mGamma;         // coefficients of the orthogonal regression
  Vector                   mResiduals;
  Matrix    mutable        mQ, mR;         // only changes are in the latter columns past Kth
  StringVec mutable        mTempNames;
  int       mutable        mTempK;         // number of predictors last tried, these are last mTempDim columns of Q
  Scalar                   mResidualSS;
  Scalar                   mTotalSS;
  
public:
  ~LinearRegression () {  }

  LinearRegression ()
    :  mN(0) { }
  
  LinearRegression (std::string yName, Vector const& y, int blockSize) 
    : mN((int)y.size()), mK(0), mBlockSize(blockSize),
      mWeightStr(""), mWeights(Vector::Ones(mN)), mSqrtWeights(Vector::Ones(mN)),
      mYName(yName), mXNames(), mY(y), mBinary(is_binary_vector(y)) { allocate_memory(); add_constant(); }
  
  LinearRegression (std::string yName, Vector const& y, std::vector<std::string> xNames, Matrix const& x, int blockSize)
    :  mN((int)y.size()), mK(0), mBlockSize(blockSize),
       mWeightStr(""), mWeights(Vector::Ones(mN)), mSqrtWeights(Vector::Ones(mN)),
       mYName(yName), mXNames(), mY(y), mBinary(is_binary_vector(y)) { allocate_memory(); add_constant(); add_predictors(xNames, x); }
  
  // WLS: if weighted, all things held are weighted by square root of input weights in w
  LinearRegression (std::string yName, Vector const& y, Vector const& w, int blockSize)
    :  mN((int)y.size()), mK(0), mBlockSize(blockSize), mWeightStr("Weighted "), mWeights(w), mSqrtWeights(w.array().sqrt()), mYName(yName),
       mY(y.array()*w.array().sqrt()), mBinary(is_binary_vector(y)) { allocate_memory(); add_constant(); }

  LinearRegression (std::string yName, Vector const& y, std::vector<std::string> xNames, Matrix const& x, Vector const& w, int blockSize)
    :  mN((int)y.size()), mK(0), mBlockSize(blockSize), mWeightStr("Weighted "), mWeights(w), mSqrtWeights(w.array().sqrt()), mYName(yName), mXNames(),
       mY(y), mBinary(is_binary_vector(y)) { allocate_memory(); add_constant(); add_predictors(xNames,x); }  

  inline bool      is_binary()         const   { return mBinary; }
  inline int       block_size()        const   { return mBlockSize; }
  inline int       n()                 const   { return mN; };
  inline int       q()                 const   { return mK-1; }                                     // -1 for intercept 
  inline Scalar    rmse()              const   { return (Scalar) sqrt(mResidualSS/((Scalar)(mN-mK))); }  
  inline Scalar    residual_ss()       const   { return mResidualSS; }
  inline Scalar    aic_c()             const   { Scalar n((Scalar)mN), k((Scalar)mK); return (Scalar) n*(Scalar)log(mResidualSS/n) + (n+k)/(1-(k+2)/n); } // hurvich89,p300
  inline Scalar    r_squared()         const   { return (Scalar) 1.0 - mResidualSS/mTotalSS; }
  inline Scalar    adj_r_squared()     const   { return (Scalar) 1.0 - (mResidualSS/(Scalar)(mN-mK)) / (mTotalSS/(Scalar)(mN-1)); }
  inline Vector    y()                 const   { return mY; }
  inline Vector    residuals()         const   { return mResiduals; }
  inline Vector    fitted_values()     const   { return mY - mResiduals; }
  inline Vector    raw_y()             const   { return mY.cwiseQuotient(mSqrtWeights); }                // raw versions remove the internal weights
  inline Vector    raw_residuals()     const   { return mResiduals.array()/mSqrtWeights.array(); }       //
  inline Vector    raw_fitted_values() const   { return fitted_values().array()/mSqrtWeights.array(); }  //

  Vector    raw_fitted_values(Scalar lo, Scalar hi)  const;                                             // truncated to indicated range
  Vector    x_row(int i)                    const; 

  Scalar    y_bar()                  const   { if (mK>0) return (Scalar) sqrt(mN)*mGamma(0); else return 0.0; }
  Vector    gamma()                  const   { return mGamma.head(mK); }
  Vector    se_gamma_ls()            const;
  Vector    se_gamma()               const;
  Vector    beta()                   const;
  Vector    se_beta_ols()            const;
  Vector    se_beta()                const;
  Vector    shrinkage_weights()      const   { return mLambda.head(mK); }

  template <class Iter>
  void      fill_with_beta (Iter begin) const;
  
  StringVec predictor_names()        const   { return mXNames; }
  Vector    predictions  (Matrix const& matrix)  const;
  Vector    predictions  (Matrix const& matrix, Scalar lo, Scalar hi)   const;    // truncate to range

  FStat     f_test_predictor  (std::string name, Vector const& z)       const;    // <f,pval>  f == 0 implies singular; uses Bennett if binary
  FStat     f_test_predictors (StringVec const& names, Matrix const& z) const;    //           blocksize > 0 implies blocked white.
  
  void      add_predictors ();                                                    // no shrinkage
  void      add_predictors (StringVec const& names, Matrix const& x);             // adds with no testing
  void      add_predictors (FStat const& fstat);
  
  void      print_to       (std::ostream& os)                   const;
  void      print_gamma_to (std::ostream& os)                   const;
  void      print_beta_to  (std::ostream& os)                   const;
  void      write_data_to  (std::ostream& os, int maxNumXCols)  const; // JMP style, with y followed by X columns (tab delimited)

  Matrix    check_orthogonality_matrix ()                       const; // returns r matrix from householder QR of internal G-S Q matrix
  
 private:
  void      allocate_memory();
  void      add_constant();
  Scalar    sweep_Q_from_column(int col)           const;              // only affect Q, R past those of current fit
  void      update_fit(StringVec xNames);
  StringVec name_vec(std::string name)             const;              // inits a vector with one string
  Scalar    approximate_ss(Vector const& x)        const;              // one-pass estimate of the SS around mean 
  bool      is_binary_vector(Vector const& y)      const;              // used to determine whether to use Bennett bounds
  bool      is_invalid_ss (Scalar ss, Scalar ssz)  const;              // checks for singularity, nan, neg, inf
  std::pair<Scalar,Scalar> bennett_evaluation ()   const;              // 0/1 response only; operates on column mK (one past those in use)

  // idioms
  Vector squared_norm (Matrix const& a)                  const { return ((a.array() * a.array()).colwise().sum()); } // diagonal of a'a
};



//     ValidatedRegression     ValidatedRegression     ValidatedRegression     ValidatedRegression     ValidatedRegression     ValidatedRegression     

class ValidatedRegression
{
public:
  typedef LinearRegression::Scalar        Scalar;
  typedef LinearRegression::Vector        Vector;
  typedef LinearRegression::Matrix        Matrix;

private:
  const int             mLength;            // total length estimation + validation
  const bool            mShrink;        
  int                   mN;                 // number of estimation rows as identified on start
  std::vector<int>      mPermute;           // permute the input for 0/1 cross-validation scrambling; length of validation + estimation
  Vector                mValidationY;
  Matrix                mValidationX;       // append when variable is added to model
  Scalar                mValidationSS;      // cache validation ss, computed whenever model changes
  LinearRegression      mModel;
  
public:
  ~ValidatedRegression () {  }
  
  ValidatedRegression() : mLength(0), mShrink(false), mN(0), mPermute() { }
  
  template<class Iter, class BIter>
  ValidatedRegression(std::string yName, Iter Y, BIter B, int len, int blockSize, bool shrink)
    :  mLength(len), mShrink(shrink), mN(0), mPermute(len) { initialize(yName, Y, B, blockSize); }

  template<class Iter, class BIter, class WIter>
  ValidatedRegression(std::string yName, Iter Y, BIter B, WIter W, int len, int blockSize, bool shrink)
    :  mLength(len), mShrink(shrink), mN(0), mPermute(len) { initialize(yName, Y, B, W, blockSize); }

  Scalar goodness_of_fit()                      const  { return mModel.r_squared(); }
  int block_size()                              const  { return mModel.block_size(); }
  int q()                                       const  { return mModel.q(); }   // number of slopes (not including intercept)
  int residual_df()                             const  { return n_estimation_cases() - 1 - mModel.q(); }
  LinearRegression const& model()               const  { return mModel; }

  Scalar y_bar()                                const  { return mModel.y_bar(); }
  std::vector<Scalar>  beta()                   const  { std::vector<Scalar> b(mModel.q()+1); mModel.fill_with_beta(b.begin()); return b; }
  std::vector<std::string> predictor_names()    const  { return mModel.predictor_names(); }

  int n_total_cases()                           const  { return mLength; }
  int n_validation_cases()                      const  { return mLength - mN; }
  int n_estimation_cases()                      const  { return mN; }
  
  Scalar estimation_ss()                        const  { return mModel.residual_ss(); }
  Scalar validation_ss()                        const  { return mValidationSS; }
  std::pair<Scalar,Scalar> sums_of_squares()    const  { return std::make_pair(estimation_ss(), mValidationSS); }

  ConfusionMatrix estimation_confusion_matrix(float threshold=0.5) const;
  ConfusionMatrix validation_confusion_matrix(float threshold=0.5) const;

  template <class Iter>                             // iterators must include training & test cases, ordered as in initial y (pval=1 adds if nonsing)
  std::pair<Scalar,Scalar> add_predictors_if_useful (std::vector<std::pair<std::string, Iter> > const& c, Scalar pToEnter);

  template <class Iter> void fill_with_fit(Iter it)                const  { fill_with_fit(it,false); }
  template <class Iter> void fill_with_fit(Iter it, bool truncate) const;
  
  void print_to     (std::ostream& os, bool useHTML=false) const;
  void write_data_to(std::ostream& os, int maxNumXCols)    const;       //  written in the internal order (estimation ->, then validation <-
  
private:
  
  template<class Iter, class BIter>
  void   initialize(std::string yName, Iter Y, BIter B, int blockSize);
  template<class Iter, class BIter, class WIter>
  void   initialize(std::string yName, Iter Y, BIter B, WIter W, int blockSize);
  
  void   initialize_validation_ss();
  
  template<class Iter>
  Vector permuted_vector_from_iterator(Iter it) const;
  
};


//      cross-validation     cross-validation     cross-validation     cross-validation     cross-validation

void
validate_regression(LinearRegression::Vector const& Y,     // response
		    LinearRegression::Matrix const& Xi,    // initial block of Xs to initialize model
		    LinearRegression::Matrix const& X,     // sequence to compute AIC, CVSS
		    int nFolds,                            // how many folds for CV (0 means no CV)
		    LinearRegression::Matrix      &result, // 4 columns: R2, RSS, AICc, CVSS; n = cols(X) rows
		    unsigned randomSeed=26612);            // used to control random splits (does not call srand).
                                                           // 0 forces deterministic split 0 1 2 3 0 1 2 3 etc for 4 fold

///////////////////////////  Printing Operators  /////////////////////////////

inline
std::ostream&
operator<<(std::ostream& os, LinearRegression const& regr)
{
  os << "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -" << std::endl;
  regr.print_to(os);
  os << "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -" << std::endl;
  return os;
}


inline
std::ostream&
operator<<(std::ostream& os, ValidatedRegression const& regr)
{
  os << "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -" << std::endl;
  regr.print_to(os);
  os << "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -" << std::endl;
  return os;
}




#include "regression.Template.h"


#endif
