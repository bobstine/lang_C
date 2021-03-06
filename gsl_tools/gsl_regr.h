// -*- c++ -*-

#ifndef _GSL_REGR_H_
#define _GSL_REGR_H_

#include "gsl_data.h"
#include "gsl_engine.h"

// f test
#include "stat_utils.h"

#include <vector>
#include <iostream>
#include <math.h>

/*
 The GSL regression object acts as an API to the GSL matrix routines.
 All IO from these is as a pointer to double, *except* for the input
 vectors which are allowed to be arbitrary iterators.  (overall input
 length = len) >= (calculation length = n) All data is permuted when
 read into a packed form in which 'n' rows are data used in the
 calculation, and the remaining rows held.  The remaining rows are
 *only* used when fitted values are requested.
 
 Fitted values are computed in a lazy fashion when the function
 fitted_values is called.  Otherwise, Xb may not be current.
 
 The calculations evaluate predictors in two stages. When the
 predictors are not in the model (Z), use a fast method based on the
 QR factorization of current model and ratios of sums of
 squares. Bennett testing is possible at this step since the weights
 are those of the prior iteration; we can thus do a version of the
 partial regression test. Once Z is added to the model, update the QR
 factorization to add these columns.
 
 For weighted analysis, only the QR terms are weighted so that have
 the ability to reweight.  Thus sweep on X uses QR, not the mX matrix
 (which is not weighted).  Similarly, residuals have been weighted,
 but not Y.  X and Y are only centered which can be corrected when
 weights change.

 This class is made to handle selection weighting, as in APL's
 compression vector for reduction.  Other types of weighting must be
 handled externally.  If no selection weights are supplied, the full
 range is used, with n set to the length of the input response.
 
 All selection subsetting is handled in the gslData object that is
 kept in the model. The data used in the gslRegression is of length n,
 the length of data to be used in calculations.
 
 Shapes:
       X and Z held in usual form, with one row per case

  10 Mar 10 ... Clean up code for the White estimator to just do the F.
   5 Jan 08 ... Check calibration of the fit using a spline.
  29 Nov 07 ... After trying more complicated methods, revert to sqrt(w) approach to WLS. Works nicely.
   5 Dec 05 ... Selection weighting added.
  20 May 03 ... Start to add sweeping features.
  10 Dec 02 ... Created to support new model code, with added iterators.
*/

const int gslRegression_Max_Q (255)  ; // default max size of the model
const int gslRegression_Max_P ( 31)  ; // max number to evaluate for adding

// State preserves enough information to return to a prior phase in the analysis

///////////////////////////  State Preservation  /////////////////////////////


class gslRegressionState
{
private:
  int mQ;
  int mN;
  double mYBar, mRSS;
  gsl_vector* mY;
  gsl_vector* mXBar;
  gsl_vector* mRes;
  gsl_vector* mWts;
  gsl_vector* mBeta;
public:
  gslRegressionState (int q, double yBar, gsl_vector const* xbar, gsl_vector const* beta, double rss, gslData const* data) 
    :  mQ(q), mN(data->n()), mYBar(yBar), mRSS(rss) { initialize(xbar, beta, data); }
  ~gslRegressionState () { free(); }
  
  int                  q()  const { return mQ; }
  int                  n()  const { return mN; }
  int        residual_df()  const { return mN - 1 - mQ; }
  double            yBar()  const { return mYBar; }
  double             rss()  const { return mRSS; }
  gsl_vector const*    y()  const { return mY; }
  gsl_vector const* xBar()  const { return mXBar; }
  gsl_vector const*  res()  const { return mRes; }
  gsl_vector const*  wts()  const { return mWts; }
  gsl_vector const* beta()  const { return mBeta; }
  
private:
  void initialize(gsl_vector const* xBar, gsl_vector const* beta, gslData const* data);
  void free();
  gsl_vector* copy_vector (gsl_vector const* v, int n);
  gslRegressionState& operator=(const gslRegressionState& state);
};


//  gslRegression     gslRegression     gslRegression     gslRegression     gslRegression     gslRegression

// Note that data (mQR, mZ) within the regression are padded by "an identity" of size mMaxQ
// to facilitate calculating shrinkage factors.  These pseudo cases come before the real data;
// the validation cases (if any) come after those used for estimation.


template <class Data, class Engine>
class gslRegression
{    
protected:
  Engine        mEngine;                  // Model specializes certain matrix arithmetic on n dim vectors, goodness of fit   
  Data   *const mpData;                   // See interface of gslData for example of necessary policy
  const int     mProtection;
  const int     mBlockSize;               // Blocking that allows for dependence within blocks
  
  int           mN, mQ, mMaxQ, mDimZ;     // mN remains length of observed data, not including pseudo obs
  double        mTSS, mRSS, mWhiteF;      // last white F stat
  gsl_vector   *mBeta;                    // logistic fit modifies beta, so this is only protected
  double        mYBar;
  gsl_vector   *mXBar;
  gsl_matrix   *mXtXinv;
  gsl_matrix   *mZResids;                 // residuals after sweeping X from z vector
  gsl_matrix   *mZZ;        
  bool          mZIsSingular;             // set when evaluate a predictor
  gsl_vector   *mShrinkage;
  
private:
  gsl_matrix *mQR;                        // current QR factorization, centered and weighted as needed
  gsl_vector *mTau;
  bool        mXtXinvIsCurrent;
  gsl_vector *mC;                         // partial slope of e on z
  gsl_matrix *mGammaZ;                    // slopes for sweeping X from Z, one column for each X
  gsl_matrix *mZ;                         // new predictors Z, held as (mN x mDimZ)
  gsl_vector *mZE;
  gsl_vector *mZBar;                      // covariances
  gsl_matrix *mZX;
 
public:
  ~gslRegression ();

  gslRegression (gslData *data, int protection, int blockSize=1)
    : mEngine(data), mpData(data), mProtection(protection), mBlockSize(blockSize) { initialize(); }
  
  //  --- Change weights ---
  void reweight(gsl_vector const* w);
  void reweight(gsl_vector const* w, gsl_vector const* y);               // change both for IRLS
  
  
  //  --- Accessors ---
  int              n() const { return mN; }                              // n are used in estimation, len are available with cv
  int            len() const { return mpData->length(); }
  int              q() const { return mQ; }
  int           maxQ() const { return mMaxQ; }
  int         df_fit() const { return mQ+1; }
  int    df_residual() const { return mN - df_fit(); }

        double        yBar() const { return mYBar; }
  const double*       xBar() const { return gsl_vector_ptr(mXBar,0); }
  const double*       beta() const { return gsl_vector_ptr(mBeta,0); }   // no intercept
  const double*  shrinkage() const { return gsl_vector_ptr(mShrinkage,0); }
        double   intercept() const;
  
  std::pair<double,double> sums_of_squares () const;                     // in-sample, out-of-sample

  template <class Iter> void fill_with_beta (Iter begin) const;  
  template <class Iter> void fill_with_fitted_values (Iter fit) const;
  
  //  --- Evaluate potential predictors (test stat, p-value) ---
  //      Pass in predictor at this step, then call routines to evaluate these predictors
  //      Each predictor is a pair<string, begin iterator> 
 
  template<class Iter> bool prepare_predictor(std::string const& name, Iter Z);                     // returns mZIsSingular indicator
  template<class C>    bool prepare_predictors(C predictor_collection);
  
  template<class Iter> void fill_with_diagonal_XtXinv(Iter begin, double scalingFactor=1.0) const;  // get std error
  
  typedef typename std::pair<double,double> TestResult;                                             // test stat, p-value
  
  TestResult  f_test()           const     { double dss (change_in_rss()); return Stat_Utils::f_test(dss, mDimZ, mRSS-dss, df_residual()-mDimZ); }
  TestResult  white_f_test()               { mWhiteF = white_f_stat();     return Stat_Utils::f_test(mWhiteF, mDimZ, df_residual()-mDimZ); }
  
  TestResult  Bennett_evaluation ()         { return Bennett_evaluation(0.0,1.0); }  // binomial y=0 or y=1
  TestResult  Bennett_evaluation (double m, double M);                               // response must be of form m <= y <= M       
  TestResult  Bennett_evaluation (double const* z, double const* y, double const* mu, double m, double M); // num is dot of z'(y-mu)
  
  int add_current_predictors ();                                                     // return size of expanded model; puts in shrinkage
  
  //  save and restore state
  gslRegressionState state()                                const { return gslRegressionState(mQ, mYBar, mXBar, mBeta, mRSS, mpData); }
  void       restore_state(gslRegressionState const& state);
  
  //  printing and output
  void print_header_to (std::ostream &os)       const;
  void print_to (std::ostream &os, int depth=0) const;                              // depth controls printing of y,x observations
  void write_data_to (std::ostream &os)         const;
  

private:   // -------------------------------------------------------------------------
    
  // Initialize 
  void initialize();
  void allocate_memory();
  
  // Calculations for candidate predictors
  void sweep_x_from_z_into_zres();
  bool z_appears_singular() const;
  void compute_cross_products_z();
  void compute_partial_coef_z();  
  void compute_fitted_values(int nUse);
  
  double change_in_rss ()   const; 
  double white_f_stat  ();

  void prepare_shrinkage();
  int  qr_decomposition (int first, int size);

protected:
  // center the vector by subtracting its mean; return the mean
  double center_data_vector (gsl_vector *v) const;
  int    update_XtXinv ();

private:
  gslRegression& operator=(const gslRegression& regr);
};

           
///////////////////////////  Printing Operators  /////////////////////////////

template <class Data, class Engine>
std::ostream&
operator<<(std::ostream& os, gslRegression<Data,Engine> const& regr)
{
  const int number_to_print(5);
  os << "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -" << std::endl;
  regr.print_to(os, number_to_print);
  os << "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -" << std::endl << std::endl;
  os.flush();
  return os;
}

///////////////////////////  Auxiliary GSL Function  /////////////////////////////


int
gsl_linalg_partial_QR_decomp (gsl_matrix * A, gsl_vector * tau, size_t start);

                     
                     
#include "gsl_regr.template.h"

#endif
