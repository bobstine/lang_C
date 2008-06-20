// $Id: seq_regr.h,v 1.9 2004/04/30 04:56:09 bob Exp $

/*
   5 Dec 03 ... Revised names from old 'Model' class to SequentialRegression
  25 Aug 03 ... Name tags added.
   5 May 03 ... Revised for range style regression.
  18 Dec 02 ... Revised with range code in mind, GSL numerics.
  21 Jun 01 ... Created for new version of sweeper.
*/

#ifndef _SEQ_REGR_H_
#define _SEQ_REGR_H_ 

#include <vector>
#include <string>
#include <ostream>   

#include "gsl_regr.h"
#include "tags.h"

#include "range.h"
#include "range_ops.h"
#include "range_stats.h"
#include "anonymous_iterator.h"

const int MAX_REGR_MODEL_SIZE(500);


class SequentialRegression
{
 public:
  typedef Tag<std::string, std::vector<int> >                                 ID;
  typedef anonymous_iterator_envelope<std::random_access_iterator_tag,double> Iter;
  typedef range<Iter>                                                         Vector;
  typedef std::vector<Vector>                                                 Matrix;
  typedef std::pair<double,double>                                            test_stat;

 private:
  int                     mN;
  Vector                  mY;
  Matrix                  mX;                    // most recent sits on end
  std::vector<ID>         mTags;                 // tagged names of the predictors
  std::vector<int>        mIndex;                // identify the spacing of predictors
  Vector                  mZ;                    // current var being considered
  unsigned int            mPredictorsEvaluated;  // number of predictors considered
  unsigned int            mPredictorsSinceAdded; // number of predictors since added one
  double                  mLastZtZ, mLastZtY;
  double*                 mLastZtX;
  gslRegression           mRegr;
  
 public:
  SequentialRegression (Vector y)
    :
    mN(end(y)-begin(y)), mY(y), mX(), mIndex(), mZ(make_anonymous_range(y)),
    mPredictorsEvaluated (0), mPredictorsSinceAdded(0),
    mLastZtZ(0.0), mLastZtY(0.0), mLastZtX(new double[MAX_REGR_MODEL_SIZE]),
    mRegr(MAX_REGR_MODEL_SIZE,
	  mN,
	  begin(y),
	  range_stats::average(y, mN),
	  range_stats::sum_of_squares(y, range_stats::average(y, mN))
	  )
    { }

  int                    number_of_observations() const  { return mN; }
  int                    number_of_predictors()   const  { return mX.size(); }
  
  double                 y_bar()                  const;
  Vector                 x_bar()                  const;
  Vector                 beta()                   const;      // beta[0] is intercept
  std::vector<double>    z_scores()               const;      // no intercept, just slopes

  ID const&              last_predictor_tag ()    const  { return mTags[mTags.size()-1]; }
  std::vector<ID> const& predictor_tags ()        const  { return mTags; }
  
  double                 TSS()                    const  { return mRegr.ss_total(); }
  double                 RSS()                    const  { return mRegr.ss_residual(); }
  double                 R2()                     const  { return (mRegr.ss_total()-mRegr.ss_residual())/mRegr.ss_total(); } 
  double                 PSS(Vector const& yCopy) const;
  double                 code_length ()           const;
  
  Vector                 residuals()              const;
  Vector                 fitted_values()          const;
  Vector                 weights()                const;  

  bool                   evaluate_gaussian_predictor (Vector x, double xBar);                  // returns true if adds, zero otherwise
  bool                   evaluate_gaussian_predictor (Vector x, double xBar, double threshold);
  bool                   evaluate_bennett_predictor  (Vector x, double xBar);
  bool                   evaluate_bennett_predictor  (Vector x, double xBar, double threshold);

  test_stat              ols_predictor_stats      (Vector x, double xBar) ;          // returns z_beta, dRSS (unadjusted)
  test_stat              gaussian_predictor_stats (Vector x, double xBar) ;          // returns F-stat, p-value; saves Z for later
  test_stat              gaussian_predictor_stats (Vector x) { return gaussian_predictor_stats (x, range_stats::average(x, mN));}
  test_stat              bennett_predictor_stats  (Vector x, double xBar);
  test_stat              bennett_predictor_stats  (Vector x) { return bennett_predictor_stats(x, range_stats::average(x, mN)); }

  double                 add_predictor(ID const& id, Vector x, double xBar);    // returns change in RSS
  double                 add_predictor(ID const& id, Vector x)  { return add_predictor(id, x, range_stats::average(x,mN)); }
  
  void                   write_to (std::ostream& os) const;
  void                   print_to (std::ostream& os) const;
  void                   read_from (std::istream& is);
  
 private:
  
  double*                predictor_cross_products (Vector z, double zBar) const;

};

std::ostream&            operator<< (std::ostream& os, SequentialRegression const& regr); 

#endif
