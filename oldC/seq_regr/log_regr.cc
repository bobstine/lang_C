// $Id: log_regr.cc,v 1.37 2004/08/25 22:15:28 bob Exp $

#include "log_regr.h"
#include "function_utils.h"
#include "print_utils.h"   // for printing tags

#include <gsl/gsl_multiroots.h>
#include <gsl/gsl_cdf.h>
#include <gsl/gsl_linalg.h>

#include <fstream> // debugging
#include <iomanip>

////  Destructor  ////

LogisticRegression::~LogisticRegression()
{
  std::clog << "LREG: Deleting logistic regression with n = "
	    << mN << ", q = " << number_of_predictors()
	    << "." << std::endl;
  free_gsl_components();
}


////  Initialize components,
////         Check input response for all 0/1 

namespace {
  class DataCheck : public std::unary_function<double,int> {
  public:
    int operator()(double x) const
      {
	if (x != 0.0 && x != 1.0) return 1 ;
	else return 0;
      }
  };
}

bool
LogisticRegression::initialize_y(Anonymous y)
{
  int count = range_ops::accumulate( make_unary_range(DataCheck(), mY),  0);
  if (count > 0)
  { std::cout << "LREG: *** Error *** Input Y vector has " << count << " cases != 0 or 1.\n";
    return false;
  }
  else
  { range_ops::copy(y, mY.begin());
    return true;
  }
}


bool
LogisticRegression::initialize_X()
{
  double n1 (accumulate(mY.begin(), mY.end(), 0.0));
  double n0 (mN - n1);
  // X matrix was created with 1's in first column
  mBeta.push_back(log(n1/n0)); // starting value for beta[0]
  update_fit_and_residuals();
  double sse = range_ops::accumulate(make_unary_range(Function_Utils::Square(),make_range(mRes)),0.0);
  mLogLike = log_likelihood();
  std::cout << "LREG: Setting intercept with n0 = " << n0 << " n1 = " << n1
	    << " has SSE = " << sse << " and log likelihood = " << mLogLike << " @ b0 = " << mBeta[0] << std::endl;
  return mLogLike != 0.0;
}


////  Printing  ////

void
LogisticRegression::print_to (std::ostream& os) const
{
  int q (number_of_predictors());
  double sse = range_ops::accumulate(make_unary_range(Function_Utils::Square(),make_range(mRes)),0.0);
  os << std::endl << "   ____________ Logistic Regression for " << mYName << " _________________ " << std::endl;
  os << "    n =  " << number_of_observations()<< std::endl;
  os << "    q =  " << q << " with " << mNumPredictorsEvaluated
     << " evaluated and " << mNumPredictorsSinceAdded << " since last added." << std::endl;
  os << "    SSE = " << sse << "     LL = " << mLogLike << std::endl;
  Vector se(standard_errors());
  os << std::setprecision(4);
  Vector b;
  b = beta();
  for (unsigned int j=0; j<mBeta.size(); ++j)
    std::cout << "        [" << std::setw(2) << j << "]"
	      << std::setw(25) << mNames[j] << "  "
	      << std::setw(10) <<      b[j] << "  "
	      << std::setw(10) <<     se[j] << "  "
	      << std::setw( 5) << fabs(b[j])/se[j] << std::endl;
  os << std::setprecision(6)
     << std::endl << "   __________________________________________________________ " << std::endl;
}

////  Write out model  ////

void
LogisticRegression::write_to (std::ostream& os) const
{
  int q (number_of_predictors());
  os << "LogisticRegression \n" << number_of_observations() << " " << q << std::endl;
  std::vector<double> b  (beta());
  // std::vector<double> se (standard_errors());
  for (int j=0; j<=q; ++j)
    os << b[j] << std::endl;
}

void
LogisticRegression::write_without_calibration_to (std::ostream& os) const
{
  if (!mCalibrated)
    write_to(os);
  else
  { int q (number_of_predictors()-1);             // write the logistic fit prior to calibration
    os << "LogisticRegression \n" << number_of_observations() << " " << q << std::endl;
    std::vector<double> b (calibration_beta());
    for (int j=0; j<=q; ++j)
      os << b[j] << std::endl;
  }
}

void
LogisticRegression::write_calibrator_to(std::ostream& os) const
{
  SmoothingSplineOperator ss (calibration_operator());
  os << ss << std::endl;
  ss.write_to(os);
  os << std::endl;
}

////  Write Data Out  ////

void
LogisticRegression::write_data_to (std::ostream& os) const
{
  std::cout << mX << std::endl; 
  unsigned int dim (mBeta.size());
  os << mYName << '\t';
  for (unsigned int j=1; j<dim-1; ++j) // skip constant column
    os << mNames[j] << '\t';
  os << mNames[dim-1] << std::endl;
  for (int i = 0; i<number_of_observations(); ++i)
  { os << mY[i] << '\t';
    for(unsigned int j=1; j<dim-1; ++j)
      os << mXCenter[j] + mXScale[j] * mX.element(i,j) << '\t';  // back to raw scale
    os << mXCenter[dim-1] + mXScale[dim-1] * mX.element(i,dim-1) << std::endl;
  }
}


////  GSL components  ////

void
LogisticRegression::free_gsl_components()
{
  if (mGradGSL) gsl_vector_free(mGradGSL);  
  if (mHessGSL) gsl_matrix_free(mHessGSL);
}

void
LogisticRegression::initialize_gsl_components ()
{
  const int size (101);  // 100 x's plus an intercept
  mGradGSL = gsl_vector_alloc(size);      gsl_vector_set_zero (mGradGSL);
  mHessGSL = gsl_matrix_alloc(size,size); gsl_matrix_set_zero (mHessGSL);
}  


void
LogisticRegression::initialize_gsl_components (int size)
{
  mGradGSL = gsl_vector_alloc(size);      gsl_vector_set_zero (mGradGSL);
  mHessGSL = gsl_matrix_alloc(size,size); gsl_matrix_set_zero (mHessGSL);
}  

void
LogisticRegression::resize_gsl_components (size_t newSize)
{
  int oldSize = mGradGSL->size;
  std::cout << "LREG: Exceeding length of gradient vector; increasing size from "
	    << oldSize << " to " << newSize;
  gsl_vector *g = gsl_vector_alloc(newSize);         gsl_vector_set_zero (g);
  gsl_matrix *h = gsl_matrix_alloc(newSize,newSize);
  for(int i=0; i<oldSize; ++i)
  { 
    gsl_vector_set(g, i, gsl_vector_get(mGradGSL,i));
  }
  for(int i=0; i<oldSize; ++i)
    for (int j=0; j<oldSize; ++j)
      gsl_matrix_set(h,i,j,gsl_matrix_get(mHessGSL,i,j));
  free_gsl_components();
  mGradGSL = g;
  mHessGSL = h;
  std::cout << " Done. " << std::endl;
}

void
LogisticRegression::copy_gsl_components (LogisticRegression const& regr)
{
  for (unsigned int i=0; i<mGradGSL->size; ++i)
  { gsl_vector_set(mGradGSL,i,gsl_vector_get(regr.mGradGSL,i));
    for (unsigned int j=0; j<mGradGSL->size; ++j)
      gsl_matrix_set(mHessGSL,i,j,gsl_matrix_get(regr.mHessGSL,i,j));
  }
}
    


////  Accessors  ////

std::vector<double>
LogisticRegression::rescale_coefs(std::vector<double> const& beta)  const
{
  std::vector<double> b (beta);
  for (unsigned int j=1; j<b.size(); ++j)
  { b[j] /= mXScale[j];
    b[0] -= b[j] * mXCenter[j];
  }
  return b;
}

std::vector<double>
LogisticRegression::standard_errors()        const
{
  int d (dim());
  // view within hessian, temp space for matrix inverse
  gsl_matrix_view  hess = gsl_matrix_submatrix(mHessGSL,0,0,d,d);
  gsl_matrix      *temp = gsl_matrix_alloc (d,d);
  gsl_matrix_memcpy(temp, &hess.matrix);  // syntax: dest,src
  // rescale, factor, then invert
  for (int j=0; j<d; ++j)
    for (int k=0; k<d; ++k)
      gsl_matrix_set(temp,j,k, gsl_matrix_get(temp,j,k) * mXScale[j] * mXScale[k]);
  int parity;
  gsl_matrix      *inverse = gsl_matrix_alloc (d,d);
  gsl_permutation *p       = gsl_permutation_alloc (d);
  gsl_linalg_LU_decomp(temp, p, &parity);
  gsl_linalg_LU_invert(temp, p, inverse);
  // move diagonal to result
  std::vector<double> se (d);
  for (int j=0; j<d; ++j)
  { double mjj (-gsl_matrix_get(inverse,j,j));
    if (mjj < 0.0)
    { std::cout << "LREG: SE for beta[" << j <<"] invalid; set to zero.   m_jj=" << mjj << std::endl;
      se[j] = 0.0;
    }
    else se[j] = sqrt(mjj);
  }
  // clean up
  gsl_permutation_free (p);
  gsl_matrix_free (temp);
  gsl_matrix_free (inverse);
  return se;
}


std::vector<double>
LogisticRegression::z_beta()                 const
{
  std::vector<double> z  (beta());
  std::vector<double> se (standard_errors());
  range_ops::transform(make_range(z), make_range(se), z.begin(), std::divides<double>());
  return z;
}

///  Likelihood  ///

double
LogisticRegression::log_likelihood() const
{ 
  return range_ops::accumulate
    (make_binary_range(Function_Utils::LogisticLikeTerm(),
		       make_range(mY),
		       make_range(mXB)),
     0.0);
}
 
///  Scoring a predictor  ///

std::pair<double,double>
LogisticRegression::score_predictor(std::string const& name, Anonymous z, double zCenter, double zScale)
{
  int d   (dim());
  // define vector of weights  
  ++mNumPredictorsEvaluated;
  Vector wts (mFit);
  for (int i=0; i<mN; ++i)
    wts[i] *= (1.0 - wts[i]);
  // accumulate cross products with standardized new predictor
  double etz (range_ops::inner_product(make_range(mRes), make_unary_range(Function_Utils::Standardize(zCenter, zScale), z), 0.0));
  double zWz (range_stats::weighted_sum_of_squares(make_unary_range(Function_Utils::Standardize(zCenter, zScale), z), make_range(wts)));
  Vector XWz (d + 1);
  mX.Xty(make_binary_range(std::multiplies<double>(),
			   make_range(wts),
			   make_unary_range(Function_Utils::Standardize(zCenter, zScale), z)),
	 XWz.begin());
  XWz[d] = zWz;
  // insert new elements into expanded hessian
  gsl_matrix      *temp = gsl_matrix_alloc (d+1,d+1);
  gsl_matrix_view  hess = gsl_matrix_submatrix(mHessGSL,0,0,d,d);
  gsl_matrix_view  tvw  = gsl_matrix_submatrix(temp,0,0,d,d);
  gsl_matrix_memcpy(&tvw.matrix, &hess.matrix);  // syntax: dest,src
  for (int j=0; j<d; ++j)
  { gsl_matrix_set(temp, j,d,XWz[j]);
    gsl_matrix_set(temp, d,j,XWz[j]);
  }
  gsl_matrix_set(temp,d,d,zWz);
  // factor, then invert
  int parity;
  gsl_matrix      *inverse = gsl_matrix_alloc (d+1,d+1);
  gsl_permutation *p       = gsl_permutation_alloc (d+1);
  int err = gsl_linalg_LU_decomp(temp, p, &parity);
  if (not err)
    err = gsl_linalg_LU_invert(temp, p, inverse);
  // clean up
  double score (etz * etz * gsl_matrix_get(inverse,d,d));
  gsl_permutation_free (p);
  gsl_matrix_free (temp);
  gsl_matrix_free (inverse);
  if (err)
  { std::cout << "LREG: *** ERROR *** GSL error " << err <<" inverting information matrix for " << name << ".\n";
    return std::make_pair(0.0,1.0);
  }
  else
  { // compute score quadratic form  (most elements are zero!)
    double pVal  (1.0);
    if (score > 0.0) pVal = gsl_cdf_chisq_Q (score, 1.0);
    std::cout << "LREG: Score test for " << name << "    Chi-sq = " << score << " with pv = " << pVal << ".\n";
    return std::make_pair(score, pVal);
  }
}

////  Adding a predictor, likelihood ratio test for predictor



///  Maximization iterations  ///

void
LogisticRegression::update_derivatives()
{
  update_fit_and_residuals();
  update_gradient();
  if (mUpdateCount == 0)
  { mUpdateCount = 3;  // controls how many steps before update hessian
    update_hessian();
  }
  --mUpdateCount;
}

void
LogisticRegression::update_fit_and_residuals()
{
  // std::cout << "LREG: Update fit and residuals using b = " <<mBeta << std::endl;
  mX.Xb(mBeta, mXB.begin());
  range_ops::transform(make_range(mXB), mFit.begin(), Function_Utils::LogisticNeg());
  range_ops::transform(make_range(mFit), mY, mRes.begin(), Function_Utils::AXPY(-1.0));
}

void
LogisticRegression::update_gradient()
{
  int d (dim());
  if (mBeta.size() > mGradGSL->size)
    resize_gsl_components(mGradGSL->size + 50);
  std::vector<double> total (d);
  mX.Xty(mRes,total.begin()); 
  for (int j=0; j<d; ++j)
    gsl_vector_set(mGradGSL, j, total[j]);
}

void
LogisticRegression::update_hessian()
{
  std::vector<double> wts (mN);
  for(int i=0; i<mN; ++i)
    wts[i] = mFit[i] * (1.0-mFit[i]);
  int d (dim());
  std::vector<double> xtx ( (d*(d+1))/2 );
  mX.XtWX (wts, xtx.begin());
  int i (0);
  for (int j=0; j<d; ++j)
  { gsl_matrix_set(mHessGSL,j,j, -xtx[i]);
    ++i;
    for (int k=j+1; k<d; ++k)
    { 
      gsl_matrix_set(mHessGSL,j,k, -xtx[i]);
      gsl_matrix_set(mHessGSL,k,j, -xtx[i]);
      ++i;
    }
  }
  /*  std::cout << "LREG: 2x2 corner of Hessian update to "
	    << gsl_matrix_get(mHessGSL,0,0) << ", "
	    << gsl_matrix_get(mHessGSL,1,0) << ", "
	    << gsl_matrix_get(mHessGSL,0,1) << ", "
	    << gsl_matrix_get(mHessGSL,1,1) << std::endl;
  */
}

void
LogisticRegression::save_state()
{
  mBetaHistory.push_back(mBeta);
}

void
LogisticRegression::restore_state()
{
  mBeta = mBetaHistory.back();
  std::cout << "LREG: Restoring state from current history of " << mBetaHistory.size() << " beta vectors.\n";
  mBetaHistory.pop_back();
  mX.pop_back();
  mXCenter.pop_back();
  mXScale.pop_back();
  mNames.pop_back();
  update_fit_and_residuals();
  update_gradient();
  update_hessian();
}

std::pair<double,double>
LogisticRegression::add_predictor(std::string const& name, Anonymous x, double xCenter,double xScale, double threshold)
{
  ++mNumPredictorsEvaluated;
  save_state();
  // make room in beta
  mBeta.push_back(0.0);
  // center and scale predictor; save center, scale and name
  mX.push_back(make_unary_range(Function_Utils::Standardize(xCenter, xScale), x));
  mXCenter.push_back(xCenter);
  mXScale.push_back(xScale);
  mNames.push_back(name);
  // temp beta vector
  int newDim (dim());
  gsl_vector *b = gsl_vector_alloc(newDim);
  for(int j=0; j<newDim; ++j)
    gsl_vector_set(b, j, mBeta[j]);
  int problem = find_mle(b);
  if (problem) std::cout << "LREG: **** find_mle returns non-zero problem status " << problem << std::endl;
  gsl_vector_free(b);
  double ll = mLogLike;
  if (!problem) ll = log_likelihood();
  double chiSq (2.0*(ll - mLogLike));
  double pVal  (1.0);
  if (chiSq > 0.0) pVal = gsl_cdf_chisq_Q (chiSq, 1.0);
  if (ll > mLogLike && threshold >= pVal)
  {
    std::cout << "LREG: Log like improves from " << mLogLike << " to " << ll << " @ " << mBeta << std::endl;
    std::cout << "LREG: Chi-square = (" << (sqrt(chiSq)) << ")^2  with pv = " << pVal << " < " << threshold << std::endl;
    update_hessian();
    mLogLike = ll;
    mNumPredictorsSinceAdded = 0;
  }
  else
  { std::cout << "LREG: Log like change from " << mLogLike << " to " << ll
	      << " was not significant; p-value = " << pVal <<std::endl;
    restore_state();
    ++mNumPredictorsSinceAdded;
  }
  return std::make_pair(chiSq, pVal);
}

// This version smooths in y space
/*
  void
  LogisticRegression::add_calibrator(int df)
  {
  std::cout << "LREG: Adding p*(1-p) calibrator with " << df << " df.\n" ;
  std::vector<double> f (fit());
  std::vector<double> weightedError (mN);
  std::vector<double> y (y());
  
  for(int i=0; i<mN; ++i)
  weightedError[i] = (y[i]-f[i])/(f[i] * (1.0-f[i]));
  if (mN > 1000)
  { std::cout << "LREG: Rounding x scale to 0.001 prior to calibration.\n";
  for (int i=0; i<mN; ++i)
  f[i] = ((double) round( 1000.0 * f[i] ))/ 1000.0;
  }
  // compute spline and smoothed values
  SmoothingSpline ss(df, f, weightedError);
  std::vector<double> z (mN);
  ss.fill_with_smooth(df, z.begin());
  // std::cout << "LREG: 0,1,2 & last smoothed values are " << z[0] << " " << z[1] << " " << z[2] << " " << z[mN-1] << std::endl;
  double      meanCal (range_stats::average(make_range(z), mN));
  double        sdCal (range_stats::standard_deviation(make_range(z), meanCal, mN));
  std::cout << "LREG: Calibrator mean = " << meanCal << " with sd = " << sdCal << std::endl;
  std::string nameCal ("Calibrator");
  add_predictor(nameCal, make_anonymous_range(z), meanCal, sdCal, 1.0);        // p to enter
  mCalibrated = true;
  mSplineOp = ss.spline_operator(); 
  }
*/

//  This version does the smoothing, then applies the logit to learn how to change Xb
void
LogisticRegression::add_calibrator(int df)
{
  std::cout << "LREG: Adding calibrator with " << df << " df.\n" ;
  std::vector<double> f (fit());
  std::vector<double> y (y());
  if (mN > 1000)
  { std::cout << "LREG: Rounding x scale to 0.001 prior to calibration.\n";
    for (int i=0; i<mN; ++i)
      f[i] = ((double) round( 1000.0 * f[i]))/ 1000.0;
  }
  SmoothingSpline ss(df, f, y);
  std::vector<double> smth (mN);
  ss.fill_with_smooth(df, smth.begin());
  std::cout << "LREG: 0,1,2 & last values of smooth are " << smth[0] << " " << smth[1] << " " << smth[2] << " " << smth[mN-1] << std::endl;
  std::transform(smth.begin(), smth.end(), smth.begin(), Function_Utils::Logit());       // L^-1(smth)
  std::cout << "                              L-1(smth) " << smth[0] << " " << smth[1] << " " << smth[2] << std::endl;
  std::vector<double> xb (xb());
  double sum (0.0);
  for (int i=0; i<mN; ++i)
  { smth[i] -= xb[i];                                           // L^-1(smth) - xb
    sum += smth[i];
  }
  double      meanCal (sum / mN);
  double        sdCal (range_stats::standard_deviation(make_range(smth), meanCal, mN));
  std::cout << "LREG: Calibrator mean = " << meanCal << " with sd = " << sdCal << std::endl;
  std::string nameCal ("Calibrator");
  add_predictor(nameCal, make_anonymous_range(smth), meanCal, sdCal, 1.0);        // p to enter
  mCalibrated = true;
  mSplineOp = ss.spline_operator();
}


void
LogisticRegression::remove_last_predictor()
{
  // resize X-space vectors
  restore_state();
  // uses the current mBeta, which restore has set to prior solution
  int problem = find_mle();  
  if (problem) std::cout << "LREG: **** find_mle returns non-zero code " << problem << "when remove last predictor.\n";
  if (!problem)
  {
    mLogLike = log_likelihood();
    std::cout << "LREG: Log like = " << mLogLike  << " @ " << mBeta << " after removing last predictor.\n";
    update_hessian();
  }
  else mLogLike = 0.0;
  if (mCalibrated) mCalibrated = false;
}
  

///  GSL optimization functions  ///


void
LogisticRegression::fill_with_gradient(gsl_vector *v) const
{
  for (int j=0; j<dim(); ++j)
    gsl_vector_set(v,j,gsl_vector_get(mGradGSL,j));
}

void
LogisticRegression::fill_with_hessian(gsl_matrix *h) const
{
  for (int k=0; k<dim(); ++k)
    for (int j=0; j<dim(); ++j)
      gsl_matrix_set(h,k,j,gsl_matrix_get(mHessGSL,k,j));
}

void
LogisticRegression::set_beta_from_gsl_vector(gsl_vector const* b)
{
  for (int j=0; j<dim(); ++j)
    mBeta[j] = gsl_vector_get(b,j);
}

void
LogisticRegression::print_optimizer_state (int iter, int dim, gsl_multiroot_fdfsolver * s)
{
  std::cout << "LREG: Solver status at iteration " << iter << std::endl
	    << "        @ ";
  for (int i=0; i<dim; ++i)
    std::cout << gsl_vector_get (s->x, i) << " ";
  std::cout << std::endl
	    << "      f'= ";
  for (int i=0; i<dim; ++i)
    std::cout << gsl_vector_get (s->f, i) << " ";
  std::cout << std::endl;
  /* Show elements from hessian
	    << "     H\\= ";
  for (int i=0; i<dim; ++i)
  {
    for (int j=i; j<dim; ++j)
      std::cout << gsl_matrix_get (mHessGSL, i,j) << " ";
    if (i < dim-1) std::cout << ", ";
  }
  std::cout << std::endl;
  */
}



namespace {  // GSL error handling
  void
  error_handler (const char *reason, const char *file, int line, int error)
  {
    std::cout << "LREG: GSL Error #" << error << ": "
	      << reason << " in " << file << " at line " << line
	      << std::endl;
  }
}

bool
LogisticRegression::hessian_is_singular() const
{
  const double cutoff (1.0e-20);
  int d (dim());
  if (d == 1)
    return false;
  else
  { gsl_matrix *xtx = gsl_matrix_alloc(d,d);
    gsl_vector *tau = gsl_vector_alloc(d);
    gsl_matrix_const_view hess = gsl_matrix_const_submatrix(mHessGSL,0,0,d,d);
    gsl_matrix_memcpy (xtx, &hess.matrix);  // (dest, src)
    gsl_error_handler_t* default_handler = gsl_set_error_handler(&error_handler);  
    int status =  gsl_linalg_QR_decomp(xtx, tau);
    if (status) std::cout << "LREG: Non-zero status " << status << " returned by QR decomp." << std::endl;
    bool result =( fabs(gsl_matrix_get(xtx,d-1,d-1)/gsl_matrix_get(mHessGSL,d-1,d-1)) < cutoff);
    if (result)
      std::cout << "LREG: Hessian singularity details " << gsl_matrix_get(xtx, d-1,d-1)
		<< "/" << gsl_matrix_get(mHessGSL,d-1,d-1) << " < " << cutoff << std::endl;
    gsl_set_error_handler (default_handler);
    gsl_matrix_free(xtx);
    gsl_vector_free(tau);
    return result;
  }
}

// helper friends
int logistic_gradient   (const gsl_vector *x, void *pM, gsl_vector *f);
int logistic_hessian    (const gsl_vector *x, void *pM, gsl_matrix *H);
int logistic_components (const gsl_vector *x, void *pM, gsl_vector *g, gsl_matrix *H);

int
LogisticRegression::find_mle()   // convert beta into an mle from current value
{
  int d (dim());
  std::cout << "LREG: Converting beta of dim " << d << " to an MLE.\n";
  gsl_vector *b = gsl_vector_alloc(d);
  for (int j=0; j<d; ++j)
    gsl_vector_set(b,j,mBeta[j]);
  int result = find_mle(b);
  for (int j=0; j<d; ++j)
    mBeta[j] = gsl_vector_get(b,j);
  gsl_vector_free(b);
  return result;
}

int
LogisticRegression::find_mle (gsl_vector *b)
{
  const int maxIterations (40);
  const gsl_multiroot_fdfsolver_type *T;
  T = gsl_multiroot_fdfsolver_hybridsj; // gnewton;
  gsl_multiroot_fdfsolver *solver;

  mUpdateCount = 0; // force hessian revision at start
  update_derivatives();
  if (hessian_is_singular())
  { std::cout << "LREG: Singular hessian; abandoning search for MLE." << std::endl;
    return GSL_EDOM;
  }
  
  int d (dim());
  gsl_multiroot_function_fdf FDF;
  FDF.f      = &logistic_gradient;
  FDF.df     = &logistic_hessian;
  FDF.fdf    = &logistic_components;
  FDF.n      = d;
  FDF.params = (void *) this;
  solver = gsl_multiroot_fdfsolver_alloc(T, d);
  gsl_multiroot_fdfsolver_set (solver, &FDF, b);

  int iter (0), status (0);
  // print_optimizer_state (iter, d, solver);
  do
  { ++iter;
    // std::cout << "LREG: Top of loop at iter = " << iter << std::endl;
    status = gsl_multiroot_fdfsolver_iterate (solver);
    if (status) break;
    status = gsl_multiroot_test_residual (solver->f, 1.0e-2);  // coarse convergence is all we need; 0.01 small on log like scale
  }
  while (status == GSL_CONTINUE && iter < maxIterations);
  print_optimizer_state (iter, d, solver);
  std::clog << "LREG: GSL optimization status ... " << gsl_strerror(status) << std::endl;
  for(int j=0; j<d; ++j)
    gsl_vector_set(b,j, gsl_vector_get(solver->x,j));
  gsl_multiroot_fdfsolver_free(solver);
  return status;
}


///  External friends

int
logistic_gradient (const gsl_vector *x, void *pM, gsl_vector *f)
{
  LogisticRegression *pModel = (LogisticRegression *) pM;
  
  pModel->set_beta_from_gsl_vector(x);
  pModel->update_derivatives();
  pModel->fill_with_gradient(f);
  return 0;
}


int
logistic_hessian (const gsl_vector *x, void *pM, gsl_matrix *H)
{
  LogisticRegression *pModel = (LogisticRegression *) pM;
  
  pModel->set_beta_from_gsl_vector(x);
  pModel->fill_with_hessian(H);
  return 0;
}


int
logistic_components (const gsl_vector *x, void *pM, gsl_vector *g, gsl_matrix *H)
{
  LogisticRegression *pModel = (LogisticRegression *) pM;
  
  pModel->set_beta_from_gsl_vector(x);
  pModel->update_derivatives();
  pModel->fill_with_gradient(g);
  pModel->fill_with_hessian(H);
  return 0;
}

