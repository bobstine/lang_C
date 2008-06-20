#include <gsl/gsl_errno.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_sf_erf.h>

#include "gsl_regr.h"
#include "bennett.h"

#include <iostream>
#include <iomanip>

gslRegression::~gslRegression()
{
  std::clog << "GSLR: deleting regr obj with n = "
	    << mN << ", q = " << mQ << ", maxQ = " << mMaxQ
	    << std::endl;
  if (mB)      delete(mB);
  if (mX)      gsl_matrix_free(mX);
  if (mY)      gsl_vector_free(mY);
  if (mZ)      gsl_vector_free(mZ);
  if (mZFit)   gsl_vector_free(mZFit);
  if (mBeta)   gsl_vector_free(mBeta);
  if (mGammaZ) gsl_vector_free(mGammaZ);
  if (mXtX)    gsl_matrix_free(mXtX);
  if (mXtXinv) gsl_matrix_free(mXtXinv);
  if (mXtXinv) gsl_matrix_free(mXtXinvRoot);
  if (mXtY)    gsl_vector_free(mXtY);
  if (mResids) gsl_vector_free(mResids);
  if (mFit)    gsl_vector_free(mFit);
  if (mZZ)     gsl_matrix_free(mZZ);
  if (mXZ)     gsl_matrix_free(mXZ);
  if (mYZ)     gsl_vector_free(mYZ);
}

  
gslRegression::gslRegression (const gslRegression& regr)
{
  std::clog << "GSLR: copy constructor called"
	    << std::endl;
  mMaxQ = regr.mMaxQ;
  mN = regr.mN;
  mQ = regr.mQ;
  mRSS = regr.mRSS;
  allocate_memory();
  gsl_vector_memcpy(mY, regr.mY);
  gsl_matrix_memcpy(mX, regr.mX);
  gsl_vector_memcpy(mZ, regr.mZ);
  gsl_vector_memcpy(mZ, regr.mZFit);
  gsl_vector_memcpy(mBeta, regr.mBeta);
  gsl_vector_memcpy(mBeta, regr.mGammaZ);
  gsl_matrix_memcpy(mXtX, regr.mXtX);  
  gsl_matrix_memcpy(mXtXinv, regr.mXtXinv);
  gsl_matrix_memcpy(mXtXinvRoot, regr.mXtXinvRoot);
  gsl_vector_memcpy(mXtY, regr.mXtY);
  gsl_vector_memcpy(mResids, regr.mResids);
  gsl_vector_memcpy(mFit, regr.mFit);
}


void
gslRegression::initialize_with_X (int q, const double **X, const double *xBar, const double **XtX, const double *XtY)
{
  mQ = q;
  for (int j=0; j<mQ; ++j)
  { gsl_vector_set(mXBar,j, *xBar);
    ++xBar;
  }
  for(int i=0; i<mN; ++i)
  { for (int j=0; j<mQ; ++j)
    { gsl_matrix_set(mX,i,j, **X);
      ++ *X;
    }
    ++X;
  }
  for(int i=0; i<mQ; ++i)
  { gsl_vector_set(mXtY,i,*XtY);
    ++XtY;
    for (int j=0; j<mQ; ++j)
    { gsl_matrix_set(mXtX,i,j, **XtX);
      ++ *XtX;
    }
    ++XtX;
  }
}


void
gslRegression::allocate_memory()
{
  std::clog << "GSLR: allocating memory with n = " << mN << ", max q = " << mMaxQ
	    << std::endl;
  // X terms
  mX      = gsl_matrix_alloc(mN, mMaxQ);
  mBeta   = gsl_vector_alloc(1+mMaxQ);  // room for intercept
  mGammaZ = gsl_vector_alloc(1+mMaxQ);
  mXBar   = gsl_vector_alloc(mMaxQ);
  mXtX    = gsl_matrix_alloc(mMaxQ,mMaxQ);
  mXtXinv = gsl_matrix_alloc(mMaxQ,mMaxQ);
  mXtY    = gsl_vector_alloc(mMaxQ);
  mXtXinvRoot = gsl_matrix_alloc(mMaxQ,mMaxQ);
  // sweep terms are nil
  mZZ     = NULL;
  mXZ     = NULL;
  mYZ     = NULL;
  // n dependent
  mY      = gsl_vector_alloc(mN);
  mZ      = gsl_vector_alloc(mN);
  mZFit   = gsl_vector_alloc(mN);
  mResids = gsl_vector_alloc(mN);
  mFit    = gsl_vector_alloc(mN);
}

//  --- Check new predictors ---

std::pair<double,double>
gslRegression::ols_predictor_stats(double ZtZ, const double *XtZ, double YtZ) const
{
  double dRSS (added_SS(ZtZ, XtZ, YtZ));
  double s2   ((mRSS-dRSS)/(df_residual()-1));     // include effect of its addition
  std::cout << "GSLR: s2=" << s2 << " from (" << mRSS << " - " << dRSS << ") / "
	    << df_residual()-1 << std::endl;
  return std::make_pair(sqrt(dRSS/s2), dRSS);
}

std::pair<double,double>
gslRegression::Gaussian_evaluation(double ZtZ, const double *XtZ, double YtZ) const
{
  double num (added_SS(ZtZ, XtZ, YtZ));
  std::clog<< "GSLR: ztz " << ZtZ << " zty " << YtZ << " --> dRSS = " << num;
  double den (mRSS-num);
  if (den < 0.0)
  { std::clog << std::endl
	      << "GSLR: Error --- Underflow in den of t-ratio, " << den << " < 0" << std::endl;
    return std::make_pair(num, 1.0);
  }
  else
  { double fRatio = (num/den) * (mN-mQ-2);                           // 2 = constant + this one
    std::clog << " F = " << fRatio << std::endl;
    return std::make_pair(num, 2.0 * gsl_sf_erf_Q(sqrt(fRatio)));    // crude approx to F prob
  }
}

//  --- Add another predictor ---

void
gslRegression::expand_cross_products (const double ZtZ, const double *XtZ, const double ZtY)
{
  gsl_vector_set(mXtY,mQ, ZtY);
  for (int j=0; j<mQ; ++j)
    { gsl_matrix_set(mXtX,j,mQ, *XtZ);
    gsl_matrix_set(mXtX,mQ,j, *XtZ);
    ++XtZ;
    }
  gsl_matrix_set(mXtX,mQ,mQ,ZtZ);
  ++mQ;
  update_XtXinv();
  update_cholesky();
  update_beta();
  update_RSS();
  update_fit_and_residuals();
}




//////////////////////////////  Linear Algebra  /////////////////////////////////


double
gslRegression::added_SS (const double ztz, const double *ztX, const double zty) const
{
  double ZZ(ztz), ZY(zty);
  if (mQ > 0)
  { // view  XtXinv, beta for needed dimensions
    gsl_vector_const_view beta   = gsl_vector_const_subvector(mBeta  ,1,mQ);  // just slopes
    gsl_matrix_const_view xtxinv = gsl_matrix_const_submatrix(mXtXinv,0,0,mQ,mQ);
    // copy cross prod into dense temp space
    gsl_vector *ztXvec = gsl_vector_alloc(mQ);
    for(int i=0; i<mQ; ++i)
    { gsl_vector_set(ztXvec,i,*ztX);
      ++ztX;
    }
    // compute adjusted ss  ZZ = z~'z~   ZY = z~'y~
    double dot(0.0);
    gsl_blas_ddot(&beta.vector, ztXvec, &dot);
    ZY -= dot;
    // quad form
    gsl_vector *temp = gsl_vector_alloc(mQ);
    gsl_blas_dgemv(CblasNoTrans, 1.0, &xtxinv.matrix, ztXvec, 0.0, temp);
    gsl_blas_ddot(temp, ztXvec, &dot);
    ZZ -= dot;
    gsl_vector_free(ztXvec);
    gsl_vector_free(temp);
  }
  if (ZZ <= 0.0)
  { std::clog << "GSLR: Underflow in added SS, " << ZZ << " < 0" << std::endl;
    return 0.0;
  }
  else
    return (ZY*ZY/ZZ);
}
      
void
gslRegression::update_XtXinv ()
{
  // std::clog << " *** XtXinv update *** \n";
  // view XtX, XtXinv for needed dimensions
  gsl_matrix_view xtx    = gsl_matrix_submatrix(mXtX   ,0,0,mQ,mQ);
  gsl_matrix_view xtxinv = gsl_matrix_submatrix(mXtXinv,0,0,mQ,mQ);
  // grab temp space
  gsl_matrix *temp = gsl_matrix_alloc(mQ,mQ);
  // copy cross-prod into temp space
  gsl_matrix_memcpy(temp, &xtx.matrix);  // syntax: dest,src
  // factor, then invert
  gsl_permutation *p = gsl_permutation_alloc (mQ);
  int parity;
  gsl_linalg_LU_decomp(temp, p, &parity);
  gsl_linalg_LU_invert(temp, p, &xtxinv.matrix);
  gsl_permutation_free (p);
  gsl_matrix_free (temp);
}

void
gslRegression::update_cholesky()
{
  // copy xtxinv into cholesky location
  gsl_matrix_view xtxinv = gsl_matrix_submatrix(mXtXinv    ,0,0,mQ,mQ);
  gsl_matrix_view root   = gsl_matrix_submatrix(mXtXinvRoot,0,0,mQ,mQ);
  gsl_matrix_memcpy(&root.matrix, &xtxinv.matrix);
  // factor in place
  int result = gsl_linalg_cholesky_decomp(&root.matrix);
  if (result)
    std::clog << " GSL: Failure in Cholesky; XtX not pos def." << std::endl;
}

  
void
gslRegression::update_beta ()
{
  // view XtY, XtXinv, beta for needed dimensions
  gsl_vector_view beta         = gsl_vector_subvector(mBeta  ,1,mQ); // skip over intercept
  gsl_vector_const_view xty    = gsl_vector_const_subvector(mXtY   ,0,mQ);
  gsl_matrix_const_view xtxinv = gsl_matrix_const_submatrix(mXtXinv,0,0,mQ,mQ);
  // matrix mult for (XtX)inv XtY
  gsl_blas_dgemv(CblasNoTrans, 1.0, &xtxinv.matrix, &xty.vector, 0.0, &beta.vector);
  // prefix with intercept
  double dot;
  gsl_vector_const_view xbar   = gsl_vector_const_subvector(mXBar ,0,mQ);
  gsl_blas_ddot(&beta.vector, &xbar.vector, &dot);
  gsl_vector_set(mBeta,0, mYBar-dot);
}


void
gslRegression::update_RSS ()
{
  // view XtY, XtXinv, beta for needed dimensions
  gsl_vector_const_view beta   = gsl_vector_const_subvector(mBeta  ,1,mQ); // skip intercept
  gsl_vector_const_view xty    = gsl_vector_const_subvector(mXtY   ,0,mQ);
  // matrix mult
  double dot(0.0);
  gsl_blas_ddot(&beta.vector, &xty.vector, &dot);
  double newRSS (mTotalSS - dot);
  if (newRSS >= mRSS)
    std::clog << "GSLR: RSS did not decrease, " << mRSS << " < " << newRSS << std::endl;
  else
    mRSS = newRSS;
}

void
gslRegression::update_fit_and_residuals ()
{
  gsl_matrix_const_view X = gsl_matrix_const_submatrix(mX,0,0,mN,mQ);
  gsl_vector_const_view b = gsl_vector_const_subvector(mBeta,1,mQ);     // skip intercept

  gsl_vector_set_all(mFit,gsl_vector_get(mBeta,0));                    // init to b[0]
  gsl_blas_dgemv(CblasNoTrans, 1.0, &X.matrix, &b.vector, 1.0, mFit);  // add others

  gsl_blas_dcopy(mY, mResids);
  gsl_blas_daxpy(-1.0, mFit, mResids);
}

// SWEEP OPS  SWEEP OPS  SWEEP OPS  SWEEP OPS  SWEEP OPS  SWEEP OPS  SWEEP OPS  SWEEP OPS  SWEEP OPS  

void
gslRegression::allocate_covariances(int p)
{
  if (mZZ) // free old space
  { gsl_matrix_free(mZZ);
    gsl_vector_free(mYZ);
    if (mXZ) gsl_matrix_free(mXZ);
  }
  mZZ   = gsl_matrix_alloc(p,p);
  mYZ   = gsl_vector_alloc(p);
  if (mQ) mXZ = gsl_matrix_alloc(mQ,p);
}

void
gslRegression::load_covariances(int p, const double** ZtZ, const double **XtZ, const double *YtZ)
{
  for (int i=0; i<mQ; ++i)
  { gsl_vector_set(mYZ,0, YtZ[i]);
    const double *zz(ZtZ[i]);
    for (int j=0; j<mQ; ++j)
      gsl_matrix_set(mZZ,i,j,zz[j]);
    const double *xz(XtZ[i]);
    for (int j=0; j<p; ++j)
      gsl_matrix_set(mXZ,i,j,xz[j]);
  }
}

void
gslRegression::sweep_current_predictors_from_covariances (void)
{
  // convert the XtZ array to (XtX)^(-1/2)XtZ
  gsl_matrix_view root   = gsl_matrix_submatrix(mXtXinvRoot,0,0,mQ,mQ);
  gsl_blas_dtrmm (CblasLeft, CblasUpper, CblasNoTrans, CblasNonUnit, 1.0, &root.matrix, mXZ);
  // do the rank q update of ZtZ
  gsl_blas_dsyrk(CblasUpper, CblasTrans, -1.0, mXZ, 1.0, mZZ);
}

namespace {
  inline double abs(double x)
  {
    return (x<0.0) ? -x : x;
  }
  std::pair<double,int>
  find_max_abs (const gsl_vector* v)
  {
    double max(0.0);
    int pos(-1);
    for (unsigned int i=0; i<v->size; ++i)
      if (abs(gsl_vector_get(v,i))>max)
      { max = gsl_vector_get(v,i);
	pos = i;
      }
    return std::make_pair(max,pos);
  }
  const double CORR_THRESHOLD(0.2);
}

std::vector<int>
gslRegression::predictors_from_covariances ()
{
  std::vector<int> iz (0);
  int p (mYZ->size);
  // initialize partial correlations
  gsl_vector *partial = gsl_vector_alloc(p);
  gsl_vector_memcpy(partial, mYZ);
  for (int i=0; i<p; ++i)
    gsl_vector_set(partial,i, gsl_vector_get(partial,i)/gsl_matrix_get(mZZ,i,i));
  // add more so long as partial corr above threshold
  std::pair<double,int> best;
  while ( (best=find_max_abs(partial)).first > CORR_THRESHOLD)
  {
    // double maxCorr = 0.0;
    // threshold partial corr based on F approx
  }    
  return iz;
}



////////////////////////  Bennett things  ////////////////////////////////

void
gslRegression::calc_gamma_Z(double zBar, const double* ZtX)
{
  if (0 == mQ)
    gsl_vector_set(mGammaZ,0, zBar);
  else
  {
    // find slopes
    gsl_vector_view gamma         = gsl_vector_subvector(mGammaZ ,1, mQ);
    gsl_vector_const_view xtz     = gsl_vector_const_view_array(ZtX, mQ);
    gsl_matrix_const_view xtxinv  = gsl_matrix_const_submatrix(mXtXinv,0,0,mQ,mQ);
    // matrix mult
    gsl_blas_dgemv(CblasNoTrans, 1.0, &xtxinv.matrix, &xtz.vector, 0.0, &gamma.vector);
    // prefix with intercept
    double dot;
    gsl_vector_view xbar   = gsl_vector_subvector(mXBar ,0,mQ);
    gsl_blas_ddot(&gamma.vector, &xbar.vector, &dot);
    gsl_vector_set(mGammaZ,0, zBar-dot);
  }
}

void
gslRegression::sweep_X_from_Z()
{
  gsl_vector_set_all(mZFit,gsl_vector_get(mGammaZ,0));                      // init to b[0]
  if(mQ > 0)
  { // add other terms to the fit
    gsl_matrix_const_view  X = gsl_matrix_const_submatrix(mX,0,0,mN,mQ);
    gsl_vector_const_view gz = gsl_vector_const_subvector(mGammaZ,1,mQ);      // skip intercept
    gsl_blas_dgemv(CblasNoTrans, 1.0, &X.matrix, &gz.vector, 1.0, mZFit);
  }
  gsl_blas_daxpy(-1.0, mZFit, mZ);                                          // subract from Z
}

double
gslRegression::RSS_of_Z(double ZtZ, const double *ZtX) const
{
  double zRSS (ZtZ);
  // matrix algebra to get Z's residual SS
  if (mQ > 0)
  { gsl_vector_const_view gz   = gsl_vector_const_subvector(mGammaZ,1,mQ);      // skip intercept
    gsl_vector_const_view ztx  = gsl_vector_const_view_array(ZtX,mQ);
    double dot(0.0);
    gsl_blas_ddot(&gz.vector, &ztx.vector, &dot);
    zRSS = zRSS - dot;
  }
  if (zRSS <= 0.0 || zRSS > ZtZ)
    std::clog << "GSLR: Problems with Z residuals; ZtZ = " << ZtZ
	      << " and zRSS = " << zRSS << std::endl;
  return zRSS;
}

namespace {
  double
  abs_val(double x)
  {
    if (x >= 0.0) return x; else return -x;
  }
  
  double
  max_abs(double x, double y)
  {
    double ax = abs_val(x);
    double ay = abs_val(y);
    if (ax >= ay) return ax; else return ay;
  }
}

std::pair<double,double>
gslRegression::compute_bennett_pair (double ZtZ, const double *ZtX)
{
  // regress Z on X
  calc_gamma_Z(mZBar, ZtX); // leaves slope on mGammaZ
  // replace Z by Z~ and get its rss
  sweep_X_from_Z ();
  double zRSS = RSS_of_Z(ZtZ, ZtX);
  //  std::cout << "GSLR: bennett zRSS = " << zRSS;
  // find beta slope for added predictor
  double betaZ (0.0);
  gsl_blas_ddot(mZ, mResids, &betaZ);
  betaZ = betaZ/zRSS;
  // std::cout << "  beta_z = " << betaZ;
  // find Bennett parameters
  double maxA (0.0);
  double sumB2 (0.0);
  double absZ (0.0);
  double *pZ = gsl_vector_ptr(mZ,0);                       // Z~ at this point
  double *pMu = gsl_vector_ptr(mFit,0);                    // current fit
  for (int i=0; i<mN; ++i)
  {
    double fit = pMu[i];                                   // truly tuned for 0/1 response
    if (fit < 0.0)        fit = 0.000001;
    else if (fit > 1.0)   fit = 0.999999;
    absZ = abs_val(pZ[i]) * max_abs(fit,1.0-fit);          // weighted by fit (x_i-x^_i)(y-y^)
    if (absZ > maxA) maxA = absZ;                          // largest in this column?
    sumB2 = sumB2 + pZ[i] * pZ[i] * fit*(1.0 - fit);       // sum_i z_i^2 V_i  binomial variance
    }
  // scale by Z's residual SS
  double a = maxA / zRSS;
  double b = sqrt(sumB2) / zRSS;
  betaZ = abs_val(betaZ);
  // std::cout << "  a=" << a << "  b=" << b << std::endl;
  double pVal =  bennett_p_value(betaZ, a, b);
  double lowerBound = bennett_bound(betaZ, a, b, .05);
  return std::make_pair(lowerBound, pVal);
}


///////////////////////////////////  Printing  //////////////////////////////////

void
gslRegression::write_header_to (std::ostream &os) const
{
  os << "                 GSLR model ["
     << mN << " of " << mLen << ", q = " << mQ << " with limit " << mMaxQ << "]" << std::endl;
  // Show SS and beta
  os << "        TSS   : " << mTotalSS << std::endl;
  os << "        RSS   : " << mRSS << "   R2 = " << 1.0-mRSS/mTotalSS << std::endl;
  os << "        Beta  : " ;
  for (int j=0; j<=mQ; ++j)
    os << std::setw(10) << gsl_vector_get(mBeta,j) << " ";
  os << std::endl << " - - - - - - - - - - - - - - - - - " << std::endl;
}

void
gslRegression::write_to (std::ostream &os, int depth) const
{
  write_header_to(os);
  // Show cross products if present
  if (mQ)
  {
    os << "    XtY             XtX " << std::endl;
    for (int j=0; j<mQ; ++j)
    { os << std::setw(10) << gsl_vector_get(mXtY,j) << "      ";
      for (int k=0; k<mQ; ++k)
	os << std::setw(10) << gsl_matrix_get(mXtX,j,k) << " ";
      os << std::endl;
    }
    os << " - - - - - - - - - - - - - - - - - " << std::endl;
  }
  os << "Leading residuals: ";
  for (int i=0; i<depth; ++i) os << gsl_vector_get(mResids,i) << "  ";
  os << std::endl;
  // Show first part of data
  os << "Y (avg " << std::setw(6) << mYBar << ")  ";
  for (int j=0; j<mQ; ++j)
    os << "X" << j << "(" << std::setw(6) << gsl_vector_get(mXBar,j) << ") ";
  os << std::endl;
  for (int i=0; i<depth; ++i)
  { os << std::setw(10) << gsl_vector_get(mY,i) << " ";
    for (int j=0; j<mQ; ++j)
      os << std::setw(10) << gsl_matrix_get(mX, i,j) << " ";
    os << std::endl;
  }
}

std::ostream&
operator<<(std::ostream& os, const gslRegression& regr)
{
  const int number_to_print(5);
  os << "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
     << std::endl;
  regr.write_to(os, number_to_print);
  os << "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
     << std::endl << std::endl;
  os.flush();
  return os;
}


// Utilities            These utilities work on dense data

double 
gslRegression::mean (double *v) const                                                   
{
  double s (0.0);
  for (int i=0; i<mN; ++i,++v)
    s += *v;
  return (s/mN);
}  

double 
gslRegression::SS (double *v, double mean) const
{
  double ss (0.0);
  for (int i=0; i<mN; ++i, ++v)
  { double dev (*v-mean);
    ss += dev * dev;
  }
  return (ss);
}
