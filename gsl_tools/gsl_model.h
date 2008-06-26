// $Id: gsl_model.h,v 1.15 2008/01/22 21:15:07 bob Exp $
#ifndef _GSL_MODEL_H_
#define _GSL_MODEL_H_

#include "gsl_regr.h"
#include "gsl_data.h"

#include <iostream>

/*
 A model puts a face on the underlying regression object, offering the
 appropriate summary (SS vs LL) and controlling the fitting process. The data
 object must support the type of calls illustrated in gsl_data.
  
 The functions write into the data object (via the "live" group of accessors)
 and will alter variables (typically involved with centering).  The data object
 is updated to suit the needs of these calculations and is not suitable for
 long-term store.
 
  4 Dec 07 ... Created to support partitioning of linear and logistic models
 */

template <class Data, class Engine>
class LinearModel : public gslRegression<Data,Engine> {
    
public:
  typedef gslRegression<Data,Engine> GSLR;
  typedef std::pair<double,double> TestResult;

  ~LinearModel() { } 
  
  LinearModel (gslData *data) : gslRegression<Data,Engine>(data) { }
  
  inline Data *data() const { return GSLR::mpData; }
  inline int      n() const { return GSLR::mN; }
  inline int      q() const { return GSLR::mQ; }
  inline double  s2() const { return GSLR::mRSS/(n()-(1+q())); }  
  
  template <class Iter>       TestResult add_predictor_if_useful (Iter it, double pToEnter);
  template <class Collection> TestResult add_predictors_if_useful (Collection c, double pToEnter);
  TestResult  check_calibration(int df = 5) const;

  template <class Iter> void fill_with_se(Iter begin, int origin) const;
  template <class Iter> void fill_with_predictions(Iter it) const { GSLR::fill_with_fitted_values (it); } // export tracks 0/1 compression

  void   print_to    (std::ostream& os) const;
  void   print_gof_to(std::ostream& os) const;
  
private:
  LinearModel& operator=(const LinearModel& m);
};



template <class Data>
class LogisticModel : public gslRegression<Data,wlsEngine> {
  
private:
  gsl_vector* mOriginalY;  // need to preserve for IRLS calculation
  double      mLL0, mLL1;  // log likelihood
  
public:
  typedef gslRegression<Data,wlsEngine> GSLR;
  typedef std::pair<double,double> TestResult;

  ~LogisticModel() { free(); }
  
  LogisticModel (gslData *data) : gslRegression<Data,wlsEngine>(data), mLL0(0), mLL1(0)   
  { allocate();
    gsl_vector_memcpy(mOriginalY, &(gsl_vector_const_subvector(GSLR::mpData->y(),0,n()).vector));
    gsl_vector_add_constant(mOriginalY, GSLR::mYBar);  // Yuk ... have to add back ???
    mLL0 = mLL1 = calc_initial_log_likelihood();       // Also sets xb to log(n1/n0)
  }
  
  inline Data *data()       { return GSLR::mpData; }
  inline int      n() const { return GSLR::mN; }
  inline int      q() const { return GSLR::mQ; }
  
  double          initial_log_likelihood() const { return mLL0; }
  double          current_log_likelihood() const { return mLL1; }
  double             calc_log_likelihood() const ; 
  double     calc_initial_log_likelihood() const ;
  TestResult     maximize_log_likelihood(int df, int max_iterations = 10);  // returns change in log-like, p-value

  template <class Iter>       TestResult add_predictor_if_useful (Iter it, double pToEnter);
  template <class Collection> TestResult add_predictors_if_useful (Collection c, double pToEnter);
                              TestResult check_calibration(int df = 5) const;
    
  template <class Iter> void fill_with_se(Iter begin, int origin) const; 
  template <class Iter> void fill_with_predictions(Iter it); // export tracks 0/1 compression

  void   print_to    (std::ostream& os) const;
  void   print_gof_to(std::ostream& os) const;

private:
  void allocate() { mOriginalY = gsl_vector_alloc(n()); }
  void free    () { if (mOriginalY) gsl_vector_free(mOriginalY); }
  
  double * estimated_probability (int num);   // uses temp space 0, so beware (also means cannot be const)
  
  LogisticModel& operator=(LogisticModel const& m);
};


std::ostream&
operator<< (std::ostream& os, LinearModel<gslData,olsEngine> const& m);

std::ostream&
operator<< (std::ostream& os, LinearModel<gslData,wlsEngine> const& m);

std::ostream&
operator<< (std::ostream& os, LogisticModel<gslData> const& m);


#include "gsl_model.template.h"

#endif
