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



//     FStatistic     FStatistic     FStatistic     FStatistic     FStatistic     FStatistic     FStatistic     FStatistic     FStatistic     FStatistic     

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


//     LinearRegression      LinearRegression      LinearRegression      LinearRegression      LinearRegression      LinearRegression      

class LinearRegression
{
public:
  typedef Eigen::VectorXd Vector;
  typedef Eigen::MatrixXd Matrix;
  typedef FStatistic      FStat;
  
private:
  int         mN;          // number of obs without pseudorows used for shrinkage
  std::string mYName; 
  Vector      mY;          // padded for shrinkage
  Matrix      mX;          // padded for shrinkage; shrinkage elements on diagonal
  Matrix      mQ;          // decomp of X (so padded too)
  Matrix      mR;
  Vector      mResiduals;
  double      mResidualSS;
  double      mTotalSS;
  std::vector<std::string> mXNames; 
  
public:
  ~LinearRegression () { }

  LinearRegression ()
    :  mN(0) { }
  
  LinearRegression (std::string yName, Vector const& y)
    :  mN(y.size()), mYName(yName), mY(y), mX(initial_x_matrix()), mXNames(name_vec("Intercept")) { initialize(); }

  LinearRegression (std::string yName, Vector const& y, std::vector<std::string> xNames, Matrix const& x)
    :  mN(y.size()), mYName(yName), mY(y), mX(insert_constant(x)), mXNames(xNames) { initialize(); }
  
  int       n()                      const   { return mN; };
  int       q()                      const   { return mX.cols()-1; }                      // -1 for intercept 
  double    rmse()                   const   { return sqrt(mResidualSS/(mN-mX.cols())); }  
  double    residual_ss()            const   { return mResidualSS; }
  double    r_squared()              const   { return 1.0 - mResidualSS/mTotalSS; }

  Vector    residuals()              const   { return mResiduals.start(mN); }  
  Vector    fitted_values()          const   { return mY.start(mN) - mResiduals.start(mN); }
  Vector    predict(Matrix const& x) const;

  Vector    beta()                   const;
  Vector    se_beta()                const;

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
  Vector pad_vector(Vector const& v, int k) const;
  std::vector<std::string> name_vec(std::string name) const;
  Matrix initial_x_matrix() const;
  Matrix insert_constant(Matrix const& m) const;                        // stuffs a 1 as first column
  void   initialize();                                                  // sets initial SS, calls orthgonalize
  void   build_QR_and_residuals();                                      // does the QR and finds residuals
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
  
  int n_all_cases()        const  { return mLength; }
  int n_validation_cases() const  { return mLength - mN; }
  int n_estimation_cases() const  { return mN; }

  double estimation_ss()   const  { return mModel.residual_ss(); }
  double validation_ss()   const;

  std::pair<double, double> sums_of_squares() { return std::make_pair(estimation_ss(), validation_ss()); }
    
  template <class Iter> std::pair<double,double> add_predictor_if_useful  (std::string name, Iter it, double pToEnter);
  template <class Iter> std::pair<double,double> add_predictors_if_useful (std::vector<std::pair<std::string, Iter> > const& c, double pToEnter);
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
