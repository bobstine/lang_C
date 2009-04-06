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

// wraps single argument as a vector to call general routine
template <class Data, class Engine>
template <class Iter> 
std::pair<double,double> 
LinearModel<Data,Engine>::add_predictor_if_useful (std::string const& name, Iter x, double pToEnter)
{
  std::vector< std::pair<std::string, Iter> >v;
  v.push_back( std::make_pair(name, x));
  return add_predictors_if_useful(v,pToEnter);
}  


/* Protection levels
   0  none, use Gaussian p-values
   1  white variance, relative to TSS-RegrSS
   2  white variance, relative to TSS
   3  2 + conservative p-value  (not yet implemented)
*/

template <class Data, class Engine>
template <class Collection> 
std::pair<double,double>  
LinearModel<Data,Engine>::add_predictors_if_useful (Collection c, double pToEnter)
{
  std::pair<double,double> result (std::make_pair(0.0,1.1));
  // first check to see if pass an initial loose score test with inflated threshold
  // return with p-value larger than 1 if singular predictors
  prepare_predictors(c);
  if (GSLR::mZIsSingular) return result;
  // if no protection, return raw p-value; otherwise exit if looks too poor
  result = GSLR::f_test_evaluation();
  // further testing if higher protection
  if (protection() == 0)
  { if (result.second > pToEnter) return result;        // exit
    std::cout << "LINM: Predictor passes standard evaluation; p-value " << result.second << " < " << pToEnter << std::endl;
  }
  else
  { if (result.second > 2 * pToEnter)   return result;  // exit
    // do more computational bennett or white test before adding
    std::cout << "LINM: Predictor passes initial evaluation; p-value " << result.second << " <  2 * " << pToEnter << std::endl;
    bool useTSS (protection() > 1);
    result = GSLR::White_evaluation(useTSS);
    if (result.second > pToEnter)   return result;  
    std::cout << "LINM: Predictor passes second evaluation;  p-value " << result.second << " <  " << pToEnter << std::endl;
  }
  // add them to the model for those that get this far
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
  os << "    SS " << GSLR::mTSS  << " --> " << GSLR::mRSS  << " R2 = " << gof();
}


template <class Data, class Engine>
void 
LinearModel<Data,Engine>::print_to    (std::ostream& os, bool useHTML) const
{
  if (useHTML)
  { os << "<HR><P>\n";
    os << "Linear Model: (n=" << GSLR::mN << ", protection=" << protection() <<") "; print_gof_to(os); os << "<br>\n";
    if (0 == GSLR::mQ)
      os << "             Model has no explanatory variables.<br>\n";
    else 
      os << "             Model has " << GSLR::mQ << " explanatory variables.<br>\n";
  }
  else
  { os << "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n";
    os << "Linear Model: (n=" << GSLR::mN << ") "; print_gof_to(os); os << std::endl;
    if (0 == GSLR::mQ)
      os << "             Model has no explanatory variables.\n";
    else 
      os << "             Model has " << GSLR::mQ << " explanatory variables.\n";
  }
  if (GSLR::mQ > 0)
  { // model does not hold intercept, so have to get this part separately
    gsl_vector *b  (gsl_vector_alloc(1+GSLR::mQ));
    gsl_vector *se (gsl_vector_alloc(1+GSLR::mQ));
    gsl_vector_set(b,0,GSLR::intercept());
    GSLR::fill_with_beta(++begin(b));
    // no se for intercept; skip over this element
    gsl_vector_set (se,0,0.0);
    fill_with_se (begin(se), 1);
    // add "intercept" name to output names
    std::vector< std::string > predictorNames;
    predictorNames.push_back("Intercept");
    for (int i=0; i<GSLR::mQ; ++i)
      predictorNames.push_back(GSLR::mpData->x_names()[i]);
    if (useHTML)
    { print_stat_summary_table_in_html (1+GSLR::mQ, predictorNames.begin(), begin(b), begin(se), os);
      os << "<HR>\n";
    }
    else
    { print_stat_summary_table (1+GSLR::mQ, predictorNames.begin(), begin(b), begin(se),  os);
      os << "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n";
    }
    gsl_vector_free(b);   
    gsl_vector_free(se);
  }
}


//////////  Logistic     Logistic     Logistic     Logistic     Logistic     Logistic     Logistic     

template <class Data>
double *
LogisticModel<Data>::estimated_probability (int num)
{
  double      *  xb  (data()->Xb()->data);
  double      * prob (data()->temp_vec(0)->data);                  // note the use of temp
  std::transform(xb, xb+num, prob, Function_Utils::LogisticNeg());
  return prob;
}


template <class Data>
template <class Iter> 
std::pair<double,double> 
LogisticModel<Data>::add_predictor_if_useful (std::string const& name, Iter x, double pToEnter)
{
  std::vector< std::pair<std::string, Iter> > v;
  v.push_back( std::make_pair(name,x) );
  return add_predictors_if_useful(v,pToEnter);
}

  
  class Sqrt : public std::unary_function<double,double> {
  public:
    double operator()(double x) const { if (x >= 0) return sqrt(x); else return -7.7; }
  };


template <class Data>
template <class Collection> 
std::pair<double,double> 
LogisticModel<Data>::add_predictors_if_useful (Collection c, double pToEnter)
{
  std::pair<double,double> result (std::make_pair(0.0,1.1));
  // use bennett to screen before revising likelihood
  // logistic regression has pseudo-y as the response, pseudo-resids as the residuals
  // evaluate_predictors leaves centered vars in Z, sweeps X and weights Zres, and leaves (Zres)'W(Zres) in mZZ
  // return if the model is singular with p-value larger than 1
  GSLR::prepare_predictors(c);
  if (GSLR::mZIsSingular) return result;
  // call bennett using the 
  const double     *pMu (estimated_probability(GSLR::mN));
  // need to unweight Z's
  gsl_vector       *pZ  (gsl_vector_alloc(GSLR::mN));
  gsl_vector       *pZW (&gsl_matrix_const_column(GSLR::mZResids,0).vector);  // ZW includes weights;
  gsl_vector_memcpy(pZ, pZW);
  GSLR::mEngine.unweight(pZ);
  const double     * z (gsl_vector_const_ptr(pZ,0));
  const double     * y (gsl_vector_const_ptr(mOriginalY,0));        
  result = GSLR::Bennett_evaluation(z, y, pMu, 0, 1);                     //  result = GSLR::f_test_evaluation();
  std::cout << "TEST: bennett returns " << result.first << " " << result.second << std::endl;
  // Insert test code (from below) here
  if (result.second > pToEnter)  { return result;  }
  std::cout << "LOGM: Predictor passes initial evaluation; p-value " << result.second << pToEnter << std::endl;
  // if passes, then maximize likelihood with z included
  gslRegressionState state (GSLR::state());                              // save in case need to back out
  GSLR::add_current_predictors();
  result = maximize_log_likelihood(1); 
  if (result.second > pToEnter)  
  { std::cout << "LOGM: Predictor fails to maximize likelihood;  p-value " << result.second << std::endl;
    GSLR::restore_state(state);
  } 
  else std::cout << "LOGM: Predictor improves likelihood; added to model\n";
  gsl_vector_free(pZ);
  return result;
}

// Test code to verify logistic calculations
  /*
  // Wts are from *prior* iteration, not current fit in pMu; note that weights != length of sqrt wts
  for (int i=0; i<5; ++i) {
  std::cout << "  p(1-p) = " << pMu[i]*(1-pMu[i]) << "    w = " << gsl_vector_get(GSLR::mEngine.weights(),i) << std::endl;
  }
  */
  /*
  // check that z vector is W-orthogonal to prior X's
  double dp;
  gsl_blas_ddot(GSLR::mEngine.sqrt_wts(), pZW, &dp);
  std::cout << "TEST: z.1 = " <<  dp << std::endl;         // w'z
  gsl_blas_ddot(                     pZW, pZW, &dp); 
  std::cout << "TEST: z.z = " <<  dp << std::endl;         // z'wz
  std::cout << "TEST: z.z = " <<  gsl_matrix_get(GSLR::mZZ,0,0) << std::endl;         // z'wz
  if (q() > 0) {
  for (int j=0; j<q(); ++j) {
  gsl_vector const* col (&gsl_matrix_const_column(GSLR::mpData->x(),j).vector);
  gsl_vector const* xj  (&gsl_vector_const_subvector(col,0,GSLR::mN).vector);
  dp = range_ops::weighted_inner_product(make_range(pZW), make_range(xj), make_range(GSLR::mEngine.sqrt_wts()), 0.0);
  std::cout << "TEST: z.(z[j]) = " <<  dp << std::endl;
  }    
  */


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
  double      *pProb (estimated_probability(len));  // these are held in temp_vec(0)
  //  GSLR::mpData->permuted_copy_to_iterator(GSLR::mpData->temp_vec(0), it, len);
  GSLR::mpData->permuted_copy_to_iterator(pProb, it, len);
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
    else if ((mLL1 - log_like_0) < 0.01) break; 
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
LogisticModel<Data>::print_to    (std::ostream& os, bool useHTML) const
{
  if (useHTML)
  { os << "<HR><P>\n";
    os << "Logistic Model: (n=" << GSLR::mN << ") "; print_gof_to(os); os << "<br>\n";
    if (0 == GSLR::mQ)
      os << "             Model has no explanatory variables.<br>\n";
    else 
      os << "             Model has " << GSLR::mQ << " explanatory variables.<br>\n";
  }
  else
  { os << "- - - - - - - - - - - - - - - - - - - - - - - - - - - -\n";
    os << "Logistic Model: (n=" << GSLR::mN << ") "; print_gof_to(os); os << std::endl;
    if (0 == GSLR::mQ)
      os << "      Model has no explanatory variables.\n";
    else
      os << "             Model has " << GSLR::mQ << " explanatory variables.<br>\n";
  }
  if (GSLR::mQ > 0)
  { gsl_vector *b  (gsl_vector_alloc(1+GSLR::mQ));
    gsl_vector *se (gsl_vector_alloc(1+GSLR::mQ));
    gsl_vector_set(b,0,GSLR::intercept());
    GSLR::fill_with_beta(++begin(b));
    gsl_vector_set (se,0,0.0);
    fill_with_se (begin(se), 1);  // skip intercept
    if (useHTML)
    { print_stat_summary_table_in_html (1+GSLR::mQ, GSLR::mpData->x_names().begin(), begin(b), begin(se), os);
      os << "<HR>\n";
    }
    else
    { print_stat_summary_table (1+GSLR::mQ, GSLR::mpData->x_names().begin(), begin(b), begin(se), os);
      os << "- - - - - - - - - - - - - - - - - - - - - - - - - - - -\n";
    }
    gsl_vector_free(b);
    gsl_vector_free(se);
  }
}


template <class Data>
void 
LogisticModel<Data>::print_gof_to(std::ostream& os) const
{
  os << " -2 LL " << -2.0 * mLL0 << " --> " << -2.0 * mLL1 << " G2 = " << gof();
}


