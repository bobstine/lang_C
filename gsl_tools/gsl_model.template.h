/* -*- c++ -*- 
 *  gsl_model.Template.h
 *  seq_regr
 *
 *  Created by Robert Stine on 12/7/07.
 *  Copyright 2007. All rights reserved.
 *
 */

// gsl iterators
#include "gsl_iterator.h"
// output
#include "print_utils.h"

// logistic transformation
#include "function_utils.h"

// accumulate
#include <numeric>
// sqrt, log
#include <math.h>
// p-value for spline
#include <gsl/gsl_cdf.h>


////////   Linear    Linear    Linear    Linear    Linear    Linear    Linear    

template <class Data, class Engine>
template <class Iter> 
std::pair<double,double> 
LinearModel<Data,Engine>::add_predictor_if_useful (Iter x, double pToEnter)
{
  std::vector<Iter> v;
  v.push_back(x);
  return add_predictors_if_useful(v,pToEnter);
}  

template <class Data, class Engine>
template <class Collection> 
std::pair<double,double>  
LinearModel<Data,Engine>::add_predictors_if_useful (Collection c, double pToEnter)
{
  std::pair<double,double> result;
  // first check to see if pass an initial loose score test with inflated threshold
  evaluate_predictors(c);
  result = GSLR::f_test_evaluation();
  if (result.second > 3 * pToEnter)  { return result;  }
  std::cout << "LINM: Predictor passes initial evaluation; p-value " << result.second << " <  3 * " << pToEnter << std::endl;
  // if passes, then do more expensive bennett or white test before adding
  result = GSLR::White_evaluation();
  if (result.second > pToEnter)  { return result;  }
  std::cout << "LINM: Predictor passes second evaluation;  p-value " << result.second << " <      " << pToEnter << std::endl;
  // add them to the model
  GSLR::add_current_predictors();
  return result;
}

template <class Data, class Engine>
template <class Iter> 
void 
LinearModel<Data,Engine>::fill_with_se(Iter begin, int origin) const
{
  if (1 == origin) ++begin;
  double sigma2 (s2());
  GSLR::fill_with_diagonal_XtXinv (begin, sigma2);
  for (int j=0; j<q(); ++j, ++begin)
    *begin = sqrt(*begin);
}

///////////////////////////////////  Calibration  //////////////////////////////////

template <class Data, class Engine>
std::pair<double,double>
LinearModel<Data,Engine>::check_calibration(int df) const
{
  // pass y, e to spline; temp vec 0 used in logistic regression
  Data *  data   (GSLR::mpData);
  int     n      (data->n()); 
  double  regrSS (GSLR::mEngine.smooth(df, data->Xb(), data->e(), data->temp_vec(1)));
  
  // test by F test of explained SS
  double totalSS (GSLR::mRSS);
  double f ((regrSS/df)/(totalSS/(n-df)));
  return std::make_pair(f,gsl_cdf_fdist_Q(f, df, n-df));
}


//////////////////////////////////  Printing  ////////////////////////////////////


template <class Data, class Engine>
void 
LinearModel<Data,Engine>::print_gof_to(std::ostream& os) const
{
  os << "    SS " << GSLR::mTSS  << " --> " << GSLR::mRSS  << " R2 = " << 1.0-GSLR::mRSS/GSLR::mTSS << std::endl;
}


template <class Data, class Engine>
void 
LinearModel<Data,Engine>::print_to    (std::ostream& os) const
{
  os << "- - - - - - - - - - - - - - - - - - - - - - - - - - - -\n";
  os << "Linear Model: (n=" << GSLR::mN << ") "; print_gof_to(os);
  if (0 == GSLR::mQ)
    os << "      Model has no explanatory variables.\n";
  else 
  { gsl_vector *b  (gsl_vector_alloc(1+GSLR::mQ));
    gsl_vector_set(b,0,GSLR::intercept());
    GSLR::fill_with_beta(++begin(b));
    gsl_vector *se (gsl_vector_alloc(1+GSLR::mQ));
    gsl_vector_set (se,0,0.0);
    fill_with_se (begin(se), 1);  // skip intercept
    print_stat_summary_table (1+GSLR::mQ, begin(b), begin(se), os);
    gsl_vector_free(se);   
  }
  os << "- - - - - - - - - - - - - - - - - - - - - - - - - - - -\n";
}




//////////  Logistic     Logistic     Logistic     Logistic     Logistic     Logistic     Logistic     

template <class Data>
double *
LogisticModel<Data>::estimated_probability (int num)
{
  double      *  xb  (data()->Xb()->data);
  double      * prob (data()->temp_vec(0)->data);
  std::transform(xb, xb+num, prob, Function_Utils::LogisticNeg());
  return prob;
}


template <class Data>
template <class Iter> 
std::pair<double,double> 
LogisticModel<Data>::add_predictor_if_useful (Iter x, double pToEnter)
{
  std::vector<Iter> v;
  v.push_back(x);
  return add_predictors_if_useful(v,pToEnter);
}


template <class Data>
template <class Collection> 
std::pair<double,double> 
LogisticModel<Data>::add_predictors_if_useful (Collection c, double pToEnter)
{
  std::pair<double,double> result;
  // use bennett to screen before revising likelihood
  // note that logistic regression has pseudo-y as the response, pseudo-resids as the residuals
  evaluate_predictors(c);   // leaves centered vars in Z, sweeps X and weights  Zres, and leaves (Zres)'W(Zres) in mZZ
  // call bennett
  const double     *pMu (estimated_probability(GSLR::mN));
  std::cout << "TEST: in adding_predictors_if_useful; pMu[0] = " << pMu[0] << std::endl;
  // need to unweight Z's
  gsl_vector       *pZv (gsl_vector_alloc(GSLR::mN));
  gsl_vector const* pZ0 (&gsl_matrix_const_column(GSLR::mZResids,0).vector);// includes weights
  gsl_vector_memcpy(pZv, pZ0);
  GSLR::mEngine.unweight(pZv);
  const double     * pZ (gsl_vector_const_ptr(pZv,0));
  const double     * pY (gsl_vector_const_ptr(mOriginalY,0));        
  result = GSLR::Bennett_evaluation(pZ, pY, pMu, 0, 1);                     //  result = GSLR::f_test_evaluation();
  std::cout << "TEST: bennett returns " << result.first << " " << result.second << std::endl;
  if (result.second > pToEnter)  { return result;  }
  std::cout << "LOGM: Predictor passes initial evaluation; p-value " << result.second << " <  3 * " << pToEnter << std::endl;
  // if passes, then maximize likelihood with it included
  gslRegressionState state (GSLR::state());                           // save in case need to back out
  GSLR::add_current_predictors();
  result = maximize_log_likelihood(1); 
  if (result.second > pToEnter)  
  { std::cout << "LOGM: Predictor fails to maximize likelihood;  p-value " << result.second << std::endl;
    GSLR::restore_state(state);
  } 
  else std::cout << "LOGM: Predictor improves likelihood; added to model\n";
  gsl_vector_free(pZv);
  return result;
}


template <class Data>
template <class Iter> 
void 
LogisticModel<Data>::fill_with_se(Iter begin, int origin) const
{
  if (1 == origin) ++begin;
  GSLR::fill_with_diagonal_XtXinv (begin);
  for (int j=0; j<q(); ++j, ++begin)
    *begin = sqrt(*begin);
}

template <class Data>
template <class Iter> 
void 
LogisticModel<Data>::fill_with_predictions(Iter it)
{ 
  int         len    (GSLR::mpData->length());
  double      *pProb (estimated_probability(len));  // put these in temp vector 0
  GSLR::mpData->permuted_copy_to_iterator(GSLR::mpData->temp_vec(0), it, len);
}   


template <class Data>
std::pair<double,double> 
LogisticModel<Data>::maximize_log_likelihood(int df, int max_iterations)
{
  double      *  xb  (data()->Xb()->data);
  double      * prob (data()->temp_vec(0)->data);
  gsl_vector  * wts  (data()->temp_vec(1));      
  gsl_vector  *  ys  (data()->temp_vec(2));     
  std::cout << "LOGM: Maximizing likelihood, starting from " << mLL1 << std::endl;
  // iterate until no change in log like or until reach max_iterations
  double saveLL (mLL1);
  int iter (0);
  while(iter++ < max_iterations) {
    double log_like_0 (mLL1); 
    std::transform(xb, xb+n(), prob, Function_Utils::LogisticNeg());  // 1/(1+e(-x))
    for (int i=0; i<n(); ++i) {
      double pi (prob[i]);
      double wi (pi*(1.0-pi));
      gsl_vector_set(wts,i, wi );
      gsl_vector_set( ys,i, xb[i] + (gsl_vector_get(mOriginalY,i)-prob[i])/wi);  // pseudo-y response
    }
    GSLR::reweight(wts, ys);        // calls QR factorization, pseudo-y residuals
    mLL1 = calc_log_likelihood();   
    std::cout << "LOGM: At step " << iter << "   Log-likelihood = " << mLL1 << ", beta = ";
    for(int j=0; j<GSLR::mQ; ++j) std::cout << gsl_vector_get(GSLR::mBeta,j) << " ";  
    std::cout << std::endl;
    if (mLL1 < log_like_0) 
      std::cout << "LOGM: *** Warning ***  Step did not improve log likelihood; continuing.\n";
    else if ((mLL1 - log_like_0) < 0.05) break; 
  }
  if (mLL1 <= saveLL) 
    std::cout << "LOGM: **** Error ****  IRLS did not improve log likelihood.\n";
  else // only update if likelihood has improved
    GSLR::update_XtXinv();
  if (iter == max_iterations) 
    std::cout << "LOGM: **** Error ****  Did not converge in " << max_iterations << " steps.\n";
  double dLL (mLL1 - saveLL);
  return std::make_pair(dLL, gsl_cdf_chisq_Q(2.0*dLL, df));    // return change in log-like, chi-squre probability
}


template <class Data>
double 
LogisticModel<Data>::calc_log_likelihood() const
{ 
  double ll (0.0);
  gsl_vector const* xb (GSLR::mpData->Xb());
  for(int i=0; i<n(); ++i)
    ll += Function_Utils::LogisticLikeTerm()(gsl_vector_get(mOriginalY,i),gsl_vector_get(xb,i));
  return ll;
}


template <class Data>
double 
LogisticModel<Data>::calc_initial_log_likelihood() const
{ 
  double ll  (0.0);
  double n1  (std::accumulate(begin(mOriginalY), end(mOriginalY),0.0));
  double n0  (n() - n1);
  double b0  (log (n1/n0) );
  double *xb (GSLR::mpData->live_Xb()->data);
  for(int i=0; i<n(); ++i, ++xb) {
    *xb = b0;
    ll += Function_Utils::LogisticLikeTerm()(gsl_vector_get(mOriginalY,i),b0);  }
  return ll;
}

///////////////////////////////////  Calibration  //////////////////////////////////
           
// ??? are these the right residuals for calibrating the logistic regression ???

template <class Data>
std::pair<double,double>
LogisticModel<Data>::check_calibration(int df) const
{
  // pass y, e to spline; temp vec 0 used for estimated probabilities
  Data *  data   (GSLR::mpData);
  int     n      (data->n()); 
  double  regrSS (GSLR::mEngine.smooth(df, estimated_probability(n), data->e(), n, data->temp_vec(1)));
  
  // test by F test of explained SS
  double totalSS (GSLR::mRSS);
  double f ((regrSS/df)/(totalSS/(n-df)));
  return std::make_pair(f,gsl_cdf_fdist_Q(f, df, n-df));
}


//////////////////////////////////  Printing  ////////////////////////////////////
                                                   
template <class Data>
void 
LogisticModel<Data>::print_to    (std::ostream& os) const
{
  os << "- - - - - - - - - - - - - - - - - - - - - - - - - - - -\n";
  os << "Logistic Model: (n=" << GSLR::mN << ") "; print_gof_to(os);
  if (0 == GSLR::mQ)
    os << "      Model has no explanatory variables.\n";
  else 
  { gsl_vector *b  (gsl_vector_alloc(1+GSLR::mQ));
    gsl_vector_set(b,0,GSLR::intercept());
    GSLR::fill_with_beta(++begin(b));
    gsl_vector *se (gsl_vector_alloc(1+GSLR::mQ));
    gsl_vector_set (se,0,0.0);
    fill_with_se (begin(se), 1);  // skip intercept
    print_stat_summary_table (1+GSLR::mQ, begin(b), begin(se), os);
    gsl_vector_free(se);   
    os << "- - - - - - - - - - - - - - - - - - - - - - - - - - - -\n";
  }
}


template <class Data>
void 
LogisticModel<Data>::print_gof_to(std::ostream& os) const
{
  os << " -2 LL " << -2.0 * mLL0 << " --> " << -2.0 * mLL1 << " G2 = " << (mLL0-mLL1)/mLL0 << std::endl;
}



