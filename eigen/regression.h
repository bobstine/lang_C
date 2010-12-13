// -*- c++ -*-
#ifndef _EIGEN_REGRESSION_H_
#define _EIGEN_REGRESSION_H_

#include "fstatistic.h"

#include <Eigen/Array>
#include <Eigen/QR>

#include <iostream>
#include <vector>

/*

  A LinearRegression object knows only about its data, and it uses
  all of the data to fit the model.  A ValidatedRegression understands
  the concept that some data will be reserved for validation purposes.

  
  14 Oct 2010 ... Split out fstatistic, using boost in place of gsl.
  30 Sep 2010 ... Playing with how to do shrinkage reasonably.
  24 Sep 2010 ... Created to start removing GSL components


  Issues that have *not* been handled...
  - Bennett for 0/1 or bounded response

*/



//     LinearRegression      LinearRegression      LinearRegression      LinearRegression      LinearRegression      LinearRegression      

class LinearRegression
{
public:
  typedef Eigen::VectorXd Vector;
  typedef Eigen::MatrixXd Matrix;
  typedef FStatistic      FStat;
  
private:
  int                      mN;           // number of actual obs without pseudo-rows used for shrinkage
  Vector                   mWeights;     // Var(y_i) = 1/W_ii; weight vector is length 1 if not supplied (ie, for ols, weight vector is a scalar)
  Vector                   mSqrtWeights;
  std::string              mYName; 
  Vector                   mY;
  double                   mYBar;
  Matrix                   mX;           // padded for shrinkage; shrinkage elements on diagonal of bottom rows
  std::vector<std::string> mXNames; 
  Matrix                   mQ;           // decomp of X, without the added rows from shrinkage
  Matrix                   mR;
  Vector                   mResiduals;
  double                   mResidualSS;
  double                   mTotalSS;
  
public:
  ~LinearRegression () { }

  LinearRegression ()
    :  mN(0), mYBar(0.0) { }

  
  // OLS
  LinearRegression (std::string yName, Vector const& y)
    :  mN(y.size()), mWeights(1), mSqrtWeights(1), mYName(yName), mY(y), mYBar(y.sum()/mN), mX(init_x_matrix()), mXNames(name_vec("Intercept")) { initialize(); }
  
  LinearRegression (std::string yName, Vector const& y, std::vector<std::string> xNames, Matrix const& x)
    :  mN(y.size()), mWeights(1), mSqrtWeights(1), mYName(yName), mY(y), mYBar(y.sum()/mN), mX(init_x_matrix(x)), mXNames(xNames) { initialize(); }

  
  // WLS: if weighted, all things held are weighted by W
  LinearRegression (std::string yName, Vector const& y, Vector const& w)
    :  mN(y.size()), mWeights(w), mSqrtWeights(w.cwise().sqrt()),
       mYName(yName), mY(y), mYBar(y.dot(w)/w.sum()), mX(init_x_matrix()), mXNames(name_vec("Intercept")) { initialize(); }

  LinearRegression (std::string yName, Vector const& y, std::vector<std::string> xNames, Matrix const& x, Vector const& w)
    :  mN(y.size()), mWeights(w), mSqrtWeights(w.cwise().sqrt()),
       mYName(yName), mY(y), mYBar(y.dot(w)/w.sum()), mX(init_x_matrix(x)), mXNames(xNames) { initialize(); }  

  bool      is_wls()                 const   { return mWeights.size() > 1; }
  bool      is_ols()                 const   { return mWeights.size() == 1; }
  
  int       n()                      const   { return mN; };
  int       q()                      const   { return mX.cols()-1; }                      // -1 for intercept 
  double    rmse()                   const   { return sqrt(mResidualSS/(mN-mX.cols())); }  
  double    residual_ss()            const   { return mResidualSS; }
  double    r_squared()              const   { return 1.0 - mResidualSS/mTotalSS; }

  Vector    residuals()              const   { return mResiduals; }
  Vector    fitted_values()          const   { return mY - mResiduals; }
  Vector    x_row(int i)             const   { return mX.row(i); }
  Vector    raw_residuals()          const   { if (is_ols()) return mResiduals; else return mResiduals.cwise()/mSqrtWeights; } 
  Vector    predict(Matrix const& x) const;

  double    y_bar()                  const   { return mYBar; }
  Vector    beta()                   const;
  Vector    se_beta()                const;
  
  std::vector<std::string>   predictor_names() const { return mXNames; }

  template <class Iter> void fill_with_predictions   (Matrix const& x, Iter begin) const;
  template <class Iter> void fill_with_fitted_values (Iter begin)                  const;
  template <class Iter> void fill_with_beta          (Iter begin)                  const;

  FStat     f_test_predictor  (Vector const& z, int blockSize = 0) const;                // <f,pval>  f == 0 implies singular; blocksize>0 for white
  FStat     f_test_predictors (Matrix const& z, int blockSize = 0) const; 

  void      add_predictor  (std::string name, Vector const& z)                               { add_predictors(name_vec(name), z, FStatistic()); } // no shrinkage
  void      add_predictor  (std::string name, Vector const& z, FStat const& fstat)           { add_predictors(name_vec(name), z, fstat); }
  void      add_predictors (std::vector<std::string> const& names, Matrix const& z)          { add_predictors(names, z, FStatistic()); }          // no shrinkage
  void      add_predictors (std::vector<std::string> const& names, Matrix const& z, FStat const& fstat);

  
  void print_to      (std::ostream& os) const;
  void write_data_to (std::ostream& os) const;                                           // JMP style, with y followed by X columns (tab delimited)

 private:
  std::vector<std::string> name_vec(std::string name) const;          // inits a vector with one string
  Matrix init_x_matrix()                const;                        // takes on the constant, terms for shrinkage
  Matrix init_x_matrix(Matrix const& m) const;                        // stuffs a 1 as first column
  void   initialize();                                                // sets initial SS, calls orthgonalize
  void   build_QR_and_residuals();                                    // does the QR and finds residuals

  // idioms
  Vector squared_norm (Matrix const& a)                  const { return ((a.cwise() * a).colwise().sum()); } // diagonal of a'a
};



//     ValidatedRegression     ValidatedRegression     ValidatedRegression     ValidatedRegression     ValidatedRegression     ValidatedRegression     ValidatedRegression     

class ValidatedRegression
{
public:
  typedef LinearRegression::Vector  Vector;
  typedef LinearRegression::Matrix  Matrix;
  
private:
  const int        mLength;         // total length estimation + validation
  int              mBlockSize;
  int              mN;              // number of estimation rows as identified on start
  std::vector<int> mPermute;        // permute the input for 0/1 cross-validation scrambling; length of validation + estimation
  Vector           mValidationY;
  Matrix           mValidationX;    // append when variable is added to model
  LinearRegression mModel;
  
public:
  ~ValidatedRegression () {  }
  
  ValidatedRegression() : mLength(0), mBlockSize(0), mN(0), mPermute() { }
  
  template<class Iter, class BIter>
  ValidatedRegression(std::string yName, Iter Y, BIter B, int len, int blockSize = 0)
    :  mLength(len), mBlockSize(blockSize), mN(0), mPermute(len)    { initialize(yName, Y, B); }

  double goodness_of_fit() const  { return mModel.r_squared(); }
  int q()                  const  { return mModel.q(); }
  int residual_df()        const  { return n_estimation_cases() - 1 - mModel.q(); }

  std::vector<std::string> predictor_names() const { return mModel.predictor_names(); }
  
  int n_total_cases()      const  { return mLength; }
  int n_validation_cases() const  { return mLength - mN; }
  int n_estimation_cases() const  { return mN; }
  
  double estimation_ss()   const  { return mModel.residual_ss(); }
  double validation_ss()   const;

  std::pair<double, double> sums_of_squares() { return std::make_pair(estimation_ss(), validation_ss()); }

  // iterators must include both the estimation and validation cases, as identified at creation
  template <class Iter> std::pair<double,double> add_predictors_if_useful (std::vector<std::pair<std::string, Iter> > const& c, double pToEnter);

  double                     y_bar()                const  { return mModel.y_bar(); }
  template <class Iter> void fill_with_fit(Iter it) const;
  
  void print_to     (std::ostream& os, bool useHTML=false) const;
  void write_data_to(std::ostream& os) const;
  
private:
  
  template<class Iter, class BIter>
  void initialize(std::string yName, Iter Y, BIter B);
  
  template<class Iter>
  LinearRegression::Vector split_iterator(Iter it) const;
  
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
