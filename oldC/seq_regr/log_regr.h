// $Id: log_regr.h,v 1.30 2004/08/25 22:15:28 bob Exp $
#ifndef _LOG_REGR_H_
#define _LOG_REGR_H_

/*

Ranges are used to hold vectors in data space (n dim), whereas GSL
used for things like the hessian and gradiant that live in the space
of estimators.

Caution: GSL routines use standardized versions of the predictors to
gain some numerical stability.  These coefs have to be rescaled when
summarized.

I am using GSL with f set to the gradient, and the hessian as the f'.
These are to be read by the "friend" from the model itself when
running, so that GSL handles the underlying optimization with access
to the model class in order to update derivatives and such.

  20 Apr 04 ... Score test.
   7 Apr 04 ... Centering.
  13 Feb 04 ... Hold ranges as vectors with X held as vector of rows.
  22 Jan 04 ... Running, agrees with JMP, but need p-values and SE.
  21 Jan 04 ... Compiling, but not working nicely with GSL.
  19 Jan 04 ... Start port from sequential ols regression code.

*/

#include <vector>
#include <string>
#include <iostream>

#include <gsl/gsl_matrix.h>
#include <gsl/gsl_multiroots.h>

#include "range.h"
#include "range_ops.h"
#include "range_stats.h"
#include "anonymous_iterator.h"
#include "matrix.h"
#include "smoothing_spline.h"

class LogisticRegression
{
 public:
  typedef anonymous_iterator_envelope<std::random_access_iterator_tag,double> Iter;
  typedef range<Iter>                                                         Anonymous;
  typedef std::vector<double>                                                 Vector;
  typedef SmoothingSplineOperator                                             SplineOp;
  typedef std::pair<double,double>                                            TestResult; // stat, pvalue
 
 private:
  int                      mN;
  std::string              mYName;        
  Vector                   mY;                       // better be all 0/1
  Matrix                   mX;                       // constant leading range, most recent sits on end
  Vector                   mXCenter;                 // centering for X (does not have to be mean)
  Vector                   mXScale;                  // scale for normalizing (does not have to be sd)
  std::vector<std::string> mNames;                   // names of columns in X
  Vector                   mBeta;                    // coefs of most recent model
  std::vector<Vector>      mBetaHistory;             // coefs of prior fits, as a triagular array
  int                      mNumPredictorsEvaluated;  // number of predictors considered
  int                      mNumPredictorsSinceAdded; // number of predictors since added one
  double                   mLogLike;                 // current value of log likelihood
  Vector                   mFit, mRes, mXB;          // used to compute likelihood
  int                      mUpdateCount;             // control how often to update hessian
  gsl_vector              *mGradGSL;                 // gradient of log like (score function)
  gsl_matrix              *mHessGSL;                 // current information matrix
  bool                     mCalibrated; 
  SplineOp                 mSplineOp;                // used for calibration
  
 public:
  ~LogisticRegression();

  LogisticRegression (LogisticRegression const& regr)
    :
    mN(regr.mN), mYName(regr.mYName), mY(regr.mY), 
    mX(regr.mX), mXCenter(regr.mXCenter),mXScale(regr.mXScale), mNames(regr.mNames), mBeta(regr.mBeta), mBetaHistory(regr.mBetaHistory),
    mNumPredictorsEvaluated (regr.mNumPredictorsEvaluated), mNumPredictorsSinceAdded(regr.mNumPredictorsSinceAdded),
    mLogLike(regr.mLogLike), mFit(regr.mFit), mRes(regr.mRes), mXB(regr.mXB), mUpdateCount(regr.mUpdateCount),
    mGradGSL(0), mHessGSL(0), mCalibrated(regr.mCalibrated), mSplineOp(regr.mSplineOp)
    {
      std::cout << "LREG: Copying constructing logistic regression.\n";
      initialize_gsl_components(regr.mGradGSL->size);
      copy_gsl_components(regr);
    }
  LogisticRegression (std::string const& name, Anonymous y, int n)
    :
    mN (n), mYName(name), mY(mN), 
    mX(mN,1,1.0), mXCenter(), mXScale(), mNames(), mBeta(), mBetaHistory(),
    mNumPredictorsEvaluated (0), mNumPredictorsSinceAdded(0), mLogLike(0.0),
    mFit(mN), mRes(mN), mXB(mN), mUpdateCount(0), mGradGSL(0), mHessGSL(0), mCalibrated(false), mSplineOp()
    {
      if(initialize_y(y))
	{ mNames.push_back("Intercept"); mXCenter.push_back(1.0); mXScale.push_back(1.0);  // intercept
	  initialize_gsl_components();
	  initialize_X();
	  find_mle();
	}
    }

  int         number_of_observations()             const { return mN; }
  int         number_of_predictors()               const { return mBeta.size()-1; }
  int         dim()                                const { return mBeta.size(); }
  
  Vector      x_center()                           const { return mXCenter; }
  Vector      x_scale()                            const { return mXScale; }
  Vector      beta()                               const { return rescale_coefs(mBeta); }
  Vector      standard_errors()                    const; 
  Vector      z_beta()                             const;

  Vector      y()                                  const { return mY; }
  Vector      fit()                                const { return mFit; }
  Vector      xb()                                 const { return mXB; }

  double      goodness_of_fit()                    const { return log_likelihood(); }
  double      log_likelihood()                     const;
  void        fill_with_gradient (gsl_vector *g)   const;
  void        fill_with_hessian  (gsl_matrix *h)   const;
  
  TestResult  score_predictor(std::string const& name, Anonymous x, double xCenter, double xScale);
  TestResult  add_predictor  (std::string const& name, Anonymous x, double xCenter, double xScale, double pToEnter);

  bool        calibrated()                         const { return mCalibrated; }
  void        add_calibrator(int df);
  void        remove_last_predictor();
  Vector      calibration_beta()                   const { return rescale_coefs(mBetaHistory.back()); }
  SplineOp    calibration_operator()               const { assert (mCalibrated); return mSplineOp; }
  
  void        read_from     (std::istream& is);
  void        write_to      (std::ostream& os)     const;
  void        write_data_to (std::ostream& os)     const;
  void        print_to      (std::ostream& os)     const;

  void        write_without_calibration_to (std::ostream& os)  const;
  void        write_calibrator_to(std::ostream& os)const;


  friend int  logistic_gradient   (const gsl_vector *x, void *pM, gsl_vector *f);
  friend int  logistic_hessian    (const gsl_vector *x, void *pM, gsl_matrix *H);
  friend int  logistic_components (const gsl_vector *x, void *pM, gsl_vector *g, gsl_matrix *H);

 private:
  bool     initialize_y(Anonymous y);
  bool     initialize_X();

  void     set_beta_from_gsl_vector(gsl_vector const* b);
  void     update_derivatives();
  void     update_fit_and_residuals();                              // using mBeta
  void     update_gradient();
  void     update_hessian();
  bool     hessian_is_singular() const;

  void     save_state();                                            // used prior to finding mle
  void     restore_state();
  Vector   rescale_coefs(Vector const& b) const;                    // convert slopes back to original coordinates
  int      find_mle();                                              // returns status, altering mBeta
  int      find_mle(gsl_vector *b);                                 // returns status
  void     print_optimizer_state (int iter, int dim, gsl_multiroot_fdfsolver * s);

  void     copy_gsl_components (LogisticRegression const& regr);
  void     initialize_gsl_components ();
  void     initialize_gsl_components (int n);
  void     resize_gsl_components(size_t size);
  void     free_gsl_components();
};

inline
std::ostream&
operator<< (std::ostream& os, LogisticRegression const& regr)
{ regr.print_to(os); return os; } 

#endif
