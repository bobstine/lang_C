// -*- c++ -*-
#ifndef _VALIDATED_REGRESSION_H_
#define _VALIDATED_REGRESSION_H_

#include "eigen_base_types.h"
#include "fstatistic.h"
#include "linear_regression.h"
#include "eigen_iterator.h"
#include "confusion_matrix.h"

#include <Eigen/Core>

#include <iostream>
#include <vector>

/*

  A LinearRegression object only uses first N cases to fit model.
  A ValidatedRegression understands the concept that some data will be
  reserved for validation purposes.

  25 Nov 2013 ... Introduction of CV using multithreads  
   4 Jul 2011 ... Use weighed version in all cases rather than two versions of code.
  20 Mar 2011 ... In place gram-schmidt branch; port to Eigen3
  14 Oct 2010 ... Split out fstatistic, using boost in place of gsl.
  30 Sep 2010 ... Playing with how to do shrinkage reasonably.
  24 Sep 2010 ... Created to start removing GSL components

  Issues that have *not* been handled...
    -- weighted with bennett
    -- logistic loss

*/



//     ValidatedRegression     ValidatedRegression     ValidatedRegression     ValidatedRegression     ValidatedRegression     ValidatedRegression     

template<class Regr>
class ValidatedRegression
{
public:
  typedef Regr                     Regression;
  typedef typename Regr::Scalar    Scalar;
  typedef typename Regr::Vector    Vector;
  typedef typename Regr::Matrix    Matrix;

private:
  const int          mLength;            // total length estimation + validation
  const bool         mShrink;        
  int                mN;                 // number of estimation rows (identified from CV indicator in initialization)
  std::vector<int>   mPermute;           // permute the input for 0/1 cross-validation scrambling; length of validation + estimation
  Vector             mValidationY;
  Scalar             mValidationSS;      // cache validation ss, computed whenever model changes
  Regr               mModel;
  
public:
  ~ValidatedRegression () {  }
  
  ValidatedRegression() : mLength(0), mShrink(false), mN(0), mPermute() { }
  
  template<class Iter, class BIter>
  ValidatedRegression(std::string yName, Iter Y, BIter B, int totalCases, int blockSize, bool shrink)
    :  mLength(totalCases), mShrink(shrink), mN(0), mPermute(totalCases) { initialize(yName, Y, B, blockSize); }

  template<class Iter, class BIter, class WIter>
  ValidatedRegression(std::string yName, Iter Y, BIter B, WIter W, int totalCases, int blockSize, bool shrink)
    :  mLength(totalCases), mShrink(shrink), mN(0), mPermute(totalCases) { initialize(yName, Y, B, W, blockSize); }

  Scalar      goodness_of_fit()                 const  { return mModel.r_squared(); }
  int         block_size()                      const  { return mModel.block_size(); }
  int         q()                               const  { return mModel.q(); }   // number of slopes (not including intercept)
  Scalar      sigma_hat()                       const  { return mModel.rmse(); }
  int         residual_df()                     const  { return n_estimation_cases() - 1 - mModel.q(); }
  Regr   &    regression()                             { return mModel; }       // *reference* to the model

  Scalar y_bar()                                const  { return mModel.y_bar(); }
  std::vector<std::string> predictor_names()    const  { return mModel.predictor_names(); }
  std::vector<Scalar>  beta()                   const;

  int n_total_cases()                           const  { return mLength; }
  int n_validation_cases()                      const  { return mLength - mN; }
  int n_estimation_cases()                      const  { return mN; }
  
  Scalar estimation_ss()                        const  { return mModel.residual_ss(); }
  Scalar validation_ss()                        const  { return mValidationSS; }
  std::pair<Scalar,Scalar> sums_of_squares()    const  { return std::make_pair(estimation_ss(), mValidationSS); }

  ConfusionMatrix estimation_confusion_matrix(Scalar threshold=0.5) const;
  ConfusionMatrix validation_confusion_matrix(Scalar threshold=0.5) const;

  template <class Iter>                                                        // iterators include training & test cases, ordered as initial y (pval=1 adds if nonsing)
  std::pair<Scalar,Scalar> add_predictors_if_useful (std::vector<std::pair<std::string, Iter> > const& c, Scalar pToEnter);

  template <class Iter>
  void                     fill_with_fit(Iter it)                  const  { fill_with_fit(it,false); }
  template <class Iter>
  void                     fill_with_fit(Iter it, bool truncate)   const;
  template <class Iter>
  void                     fill_with_residuals (Iter it)         const;
  
  void print_to     (std::ostream& os, bool compact=false)         const;
  void print_html_to(std::ostream& os)                             const;
  void write_data_to(std::ostream& os, int maxNumXCols)            const;       //  written in the internal order (estimation ->, then validation <-)
  
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
cross_validate_regression(LinearRegression::Vector const& Y,     // response
		    LinearRegression::Matrix const& Xi,    // initial block of Xs to initialize model
		    LinearRegression::Matrix const& X,     // sequence to compute AIC, CVSS
		    int nFolds,                            // how many folds for CV (0 means no CV)
		    LinearRegression::Matrix      &result, // 4 columns: R2, RSS, AICc, CVSS; n = cols(X) rows
		    unsigned randomSeed=26612);            // used to control random splits (does not call srand).
                                                           // 0 forces deterministic split 0 1 2 3 0 1 2 3 etc for 4 fold

///////////////////////////  Printing Operators  /////////////////////////////

template<class Regr>
inline
std::ostream&
operator<<(std::ostream& os, ValidatedRegression<Regr> const& regr)
{
  os << "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -" << std::endl;
  regr.print_to(os);
  os << "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -" << std::endl;
  return os;
}

#endif
