// -*- c++ -*-
#ifndef _EIGEN_REGRESSION_H_
#define _EIGEN_REGRESSION_H_

#include "fstatistic.h"

#include <Eigen/Core>

#include <iostream>
#include <vector>

/*

  A LinearRegression object knows only about its data, and it uses
  all of the data to fit the model.  A ValidatedRegression understands
  the concept that some data will be reserved for validation purposes.

  
  20 Mar 2011 ... In place gram-schmidt branch; port to Eigen3
  14 Oct 2010 ... Split out fstatistic, using boost in place of gsl.
  30 Sep 2010 ... Playing with how to do shrinkage reasonably.
  24 Sep 2010 ... Created to start removing GSL components


  Issues that have *not* been handled...
    -- weighted with bennett
    -- logistic loss

*/



//     LinearRegression      LinearRegression      LinearRegression      LinearRegression      LinearRegression      LinearRegression      

class LinearRegression
{
public:
  typedef Eigen::VectorXd          Vector;
  typedef Eigen::MatrixXd          Matrix;
  typedef FStatistic               FStat;
  typedef std::vector<std::string> StringVec;
  
private:
  int                      mN;             // number of actual obs without pseudo-rows used for shrinkage
  int                      mK;             // number of columns (including constant) in the model
  int                      mBlockSize;     // 0 = ols, 1 = heteroscedastic white, 2+ for dependence
  Vector                   mWeights;       // Var(y_i) = 1/W_ii; weight vector is length 1 if not supplied (ie, for ols, weight vector is a scalar)
  Vector                   mSqrtWeights;
  std::string              mYName; 
  StringVec                mXNames;
  Vector                   mY;             // original response
  bool                     mBinary;        // y is in {0,1}
  Vector                   mLambda;
  Vector                   mGamma;         // coefficients of the orthogonal regression
  Vector                   mResiduals;
  mutable Matrix           mQ, mR;         // only changes are in the latter columns past Kth
  mutable StringVec        mTempNames;
  mutable int              mTempK;         // number of predictors last tried, form last mTempDim columns of Q
  double                   mResidualSS;
  double                   mTotalSS;
  
public:
  ~LinearRegression () {  }

  LinearRegression ()
    :  mN(0) { }

  
  // OLS
  LinearRegression (std::string yName, Vector const& y, int blockSize)
    :  mN(y.size()), mK(0), mBlockSize(blockSize), mWeights(1), mSqrtWeights(1), mYName(yName), mXNames(),
       mY(y), mBinary(is_binary_vector(y)) { allocate_memory(); add_constant(); }
  
  LinearRegression (std::string yName, Vector const& y, std::vector<std::string> xNames, Matrix const& x, int blockSize)
    :  mN(y.size()), mK(0), mBlockSize(blockSize), mWeights(1), mSqrtWeights(1), mYName(yName), mXNames(),
       mY(y), mBinary(is_binary_vector(y)) { allocate_memory(); add_predictors(xNames, x); }

  
  // WLS: if weighted, all things held are weighted by W
  LinearRegression (std::string yName, Vector const& y, Vector const& w, int blockSize)
    :  mN(y.size()), mK(0), mBlockSize(blockSize), mWeights(w), mSqrtWeights(w.array().sqrt()), mYName(yName),
       mY(y), mBinary(is_binary_vector(y)) { allocate_memory(); add_constant(); }

  LinearRegression (std::string yName, Vector const& y, std::vector<std::string> xNames, Matrix const& x, Vector const& w, int blockSize)
    :  mN(y.size()), mK(0), mBlockSize(blockSize), mWeights(w), mSqrtWeights(w.array().sqrt()), mYName(yName), mXNames(),
       mY(y), mBinary(is_binary_vector(y)) { allocate_memory(); add_predictors(xNames,x); }  

  bool      is_binary()              const   { return mBinary; }
  bool      is_wls()                 const   { return mWeights.size() > 1; }
  bool      is_ols()                 const   { return mWeights.size() == 1; }
  int       block_size()             const   { return mBlockSize; }
  int       n()                      const   { return mN; };
  int       q()                      const   { return mK-1; }                      // -1 for intercept 
  double    rmse()                   const   { return sqrt(mResidualSS/(mN-mK)); }  
  double    residual_ss()            const   { return mResidualSS; }
  double    r_squared()              const   { return 1.0 - mResidualSS/mTotalSS; }

  Vector    x_row(int i)             const; 
  Vector    residuals()              const   { return mResiduals; }
  Vector    raw_residuals()          const;
  Vector    fitted_values()          const   { return mY - mResiduals; }
  Vector    fitted_values(double lo, double hi)  const;                            // truncated to indicated range

  double    y_bar()                  const   { if (mK>0) return sqrt(mN)*mGamma(0); else return 0.0; }
  Vector    gamma()                  const   { return mGamma.head(mK); }
  Vector    se_gamma_ols()           const;
  Vector    se_gamma()               const;
  Vector    beta()                   const;
  Vector    se_beta_ols()            const;
  Vector    se_beta()                const;
  Vector    shrinkage_weights()      const   { return mLambda.head(mK); }

  template <class Iter>
  void      fill_with_beta (Iter begin) const;
  
  StringVec predictor_names()   const   { return mXNames; }
  Vector    predictions  (Matrix const& matrix)  const;

  FStat     f_test_predictor  (std::string name, Vector const& z)                      const; // <f,pval>  f == 0 implies singular; uses Bennett if binary
  FStat     f_test_predictors (StringVec const& names, Matrix const& z) const;
  
  void      add_predictors ()                                                                { add_predictors(FStatistic()); }           // no shrinkage
  void      add_predictors (StringVec const& names, Matrix const& x);                                                 // adds with no testing
  void      add_predictors (FStat const& fstat);
  
  void      print_to       (std::ostream& os)      const;
  void      print_gamma_to (std::ostream& os)      const;
  void      print_beta_to  (std::ostream& os)      const;
  void      write_data_to  (std::ostream& os)      const;            // JMP style, with y followed by X columns (tab delimited)

  Matrix    check_orthogonality_matrix ()          const;            // returns r matrix from householder QR of internal G-S Q matrix
  
 private:
  void      allocate_memory();
  void      add_constant();
  double    sweep_Q_from_column(int col)           const;            // only affect Q, R past those of current fit
  void      update_fit(StringVec xNames);
  StringVec name_vec(std::string name)             const;            // inits a vector with one string
  double    approximate_ss(Vector const& x)        const;            // one-pass estimate of the SS around mean 
  bool      is_binary_vector(Vector const& y)      const;            // used to determine whether to use Bennett bounds
  bool      is_invalid_ss (double ss, double ssz)  const;            // checks for singularity, nan, neg, inf
  std::pair<double,double> bennett_evaluation ()   const;            // 0/1 response only; operates on column mK (one past those in use)

  // idioms
  Vector squared_norm (Matrix const& a)                  const { return ((a.array() * a.array()).colwise().sum()); } // diagonal of a'a
};



//     ValidatedRegression     ValidatedRegression     ValidatedRegression     ValidatedRegression     ValidatedRegression     ValidatedRegression     ValidatedRegression     

class ValidatedRegression
{
public:
  typedef LinearRegression::Vector  Vector;
  typedef LinearRegression::Matrix  Matrix;
  
private:
  const int             mLength;            // total length estimation + validation
  const bool            mShrink;        
  int                   mN;                 // number of estimation rows as identified on start
  std::vector<int>      mPermute;           // permute the input for 0/1 cross-validation scrambling; length of validation + estimation
  Vector                mValidationY;
  Matrix                mValidationX;       // append when variable is added to model
  double                mValidationSS;      // cache validation ss, computed whenever model changes
  LinearRegression      mModel;
  
public:
  ~ValidatedRegression () {  }
  
  ValidatedRegression() : mLength(0), mShrink(false), mN(0), mPermute() { }
  
  template<class Iter, class BIter>
  ValidatedRegression(std::string yName, Iter Y, BIter B, int len, int blockSize, bool shrink)
    :  mLength(len), mShrink(shrink), mN(0), mPermute(len) { initialize(yName, Y, B, blockSize); }

  double goodness_of_fit() const  { return mModel.r_squared(); }
  int block_size()         const  { return mModel.block_size(); }
  int q()                  const  { return mModel.q(); }
  int residual_df()        const  { return n_estimation_cases() - 1 - mModel.q(); }

  std::vector<std::string> predictor_names() const { return mModel.predictor_names(); }
  
  int n_total_cases()      const  { return mLength; }
  int n_validation_cases() const  { return mLength - mN; }
  int n_estimation_cases() const  { return mN; }
  
  double estimation_ss()   const  { return mModel.residual_ss(); }
  double validation_ss()   const  { return mValidationSS; }

  std::pair<double, double> sums_of_squares() { return std::make_pair(estimation_ss(), mValidationSS); }

  template <class Iter>                                                  // iterators must include training and test cases, ordered as in initial y
  std::pair<double,double> add_predictors_if_useful (std::vector<std::pair<std::string, Iter> > const& c, double pToEnter);

  double                     y_bar()                const  { return mModel.y_bar(); }
  template <class Iter> void fill_with_fit(Iter it) const;
  
  void print_to     (std::ostream& os, bool useHTML=false) const;
  void write_data_to(std::ostream& os)                     const;       //  written in the internal order (estimation ->, then validation <-
  
private:
  
  template<class Iter, class BIter>
  void   initialize(std::string yName, Iter Y, BIter B, int blockSize);

  void   initialize_validation_ss();
  
  template<class Iter>
  Vector permuted_vector_from_iterator(Iter it) const;
  
};




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
