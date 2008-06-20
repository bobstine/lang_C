// $Id: gsl_regr.h,v 1.6 2003/08/18 00:40:44 bob Exp $

#include <gsl/gsl_matrix.h>
#include <vector>
#include <iostream>

/*
  The GSL regression object acts as an API to the GSL matrix routines.
  All IO from these is as a pointer to double, *except* for the input
  vector which are allowed to be arbitrary iterators.

  20 May 03 ... Start to add sweeping features.
  10 Dec 02 ... Created to support new model code, with added iterators.

*/

class gslRegression
{
  int mN, mQ, mMaxQ;
  
  gsl_matrix *mX;  gsl_vector *mXBar;
  gsl_vector *mY;  double      mYBar;
  gsl_vector *mZ;  double      mZBar; // holds temp space for predictor to consider
  gsl_vector *mZFit;
  
  gsl_vector *mBeta, *mGammaZ; // latter used for sweeping
  double      mTotalSS, mRSS;
  gsl_matrix *mXtX;
  gsl_matrix *mXtXinv;
  gsl_matrix *mXtXinvRoot;
  gsl_vector *mXtY;

  gsl_matrix *mZZ, *mXZ;              // used in sweeping several
  gsl_vector *mYZ;
  
  gsl_vector *mResids;
  gsl_vector *mFit;

 public:
  
  ~gslRegression ();

  //  --- Initialize ---

  gslRegression (const gslRegression& regr);

  template<class Iter>
    gslRegression (const int n, Iter Y, const double yBar, const double ySS)
    { initialize_with_Y(25, n, Y, yBar, ySS); }

  template<class Iter>
    gslRegression (const int maxQ, const int n, Iter Y, const double yBar, const double ySS)
    { initialize_with_Y(maxQ, n, Y, yBar, ySS); }

  gslRegression (const int n, const int q,
	      const double *Y, const double yBar, const double ySS,
	      const double **X, const double *xBar, const double **XtX,
	      const double *XtY)
    { initialize_with_Y(10+q, n, Y,yBar,ySS); initialize_with_X(q, X, xBar, XtX, XtY); }
  
  gslRegression (const int maxQ, const int n, const int q,
	      const double *Y, const double yBar, const double ySS,
	      const double **X, const double *xBar, const double **XtX,
	      const double *XtY)
    { initialize_with_Y(maxQ, n, Y,yBar,ySS); initialize_with_X(q, X,xBar, XtX, XtY); }

  
  //  --- Consider one new predictor (test stat, nominal p-value) ---
  
  std::pair<double,double>
    ols_predictor_stats(double ZtZ, const double *ZtX, double YtZ) const;  // z_beta, dRSS (unadjusted)

  std::pair<double,double>
    Gaussian_evaluation(double ZtZ, const double *ZtX, double YtZ) const;  // dRSS, p-val
  
  template<class Iter>
    std::pair<double,double>
    Bennett_evaluation(Iter Z, double zBar, double ZtZ, const double* ZtX)
    {
      mZBar = zBar;
      std::copy(Z, Z + mN, gsl_vector_ptr(mZ,0));
      return compute_bennett_pair(ZtZ, ZtX);
    }

  //  --- Consider several predictors, offered as a bundle (sweep)
  
  void load_covariances(int p, const double** ZtZ, const double **XtZ, const double *YtZ);

  void sweep_current_predictors_from_covariances (void);

  std::vector<int> predictors_from_covariances ();
    
  
  //  --- Add another predictor (returns new size) ---

  template<class Iter>
    int AddPredictor (Iter Z, const double zBar, const double ZtZ,
		      const double *XtZ, const double ZtY)
    {
      if (mQ == mMaxQ)
      { std::clog << "REGR: q = max q  " << mQ << "==" << mMaxQ << std::endl;
	return 0;
      }
      else
      {	gsl_vector_set(mXBar,mQ,zBar);
	for (int i=0; i<mN; ++i)
	{ gsl_matrix_set(mX,i,mQ,*Z);
	  ++Z;
	}
	expand_cross_products(ZtZ, XtZ, ZtY);
	return mQ;
      }
    }

  //  --- Read features ---

  int N()    const {  return mN; }
  int Q()    const {  return mQ; }
  int MaxQ() const {  return mMaxQ; }

  const gsl_vector* Y() const {  return mY; }
  const gsl_matrix* X() const {  return mX; }
  const double  YBar() const {  return mYBar; }
  const double* XBar() const {  return gsl_vector_ptr(mXBar,0); }
  const double* beta() const { return gsl_vector_ptr(mBeta,0); }        // intercept in b[0]
  const double* gamma() const { return gsl_vector_ptr(mGammaZ,0); }        // intercept in b[0]
  
  double ss_total()      const {  return mTotalSS; }
  double ss_residual()   const {  return mRSS; }
  int    df_fit()        const {  return mQ+1; }
  int    df_residual()   const {  return mN - mQ - 1; }

  template<class Iter>
    void fill_with_se(Iter begin) const
    { double s2 (ss_residual()/df_residual());
      for (int j=0; j<mQ; ++j)
	*begin++ = sqrt(gsl_matrix_get(mXtXinv,j,j) * s2);
    }
  
  const double* residuals()   const {  return gsl_vector_ptr(mResids,0); }
  const double* fit()         const {  return gsl_vector_ptr(mFit,0); }

  //  --- Printing access ---

  void write_header_to (std::ostream &os) const;
  void write_to (std::ostream &os, int depth) const;
  
 private:

  template<class Iter>
  void initialize_with_Y(const int maxQ, const int n,
		  Iter Y, const double yBar, const double ySS)
    {
      mMaxQ = maxQ;  mN = n;  mQ = 0;
      mRSS = mTotalSS = ySS;
      allocate_memory();
      mYBar = yBar;
      gsl_vector_set(mBeta,0,yBar);
      for(int i=0; i<mN; ++i)
	{
	  gsl_vector_set(mFit,i, yBar);
	  gsl_vector_set(mResids,i, *Y-yBar);
	  gsl_vector_set(mY,i, *Y);
	  ++Y;
	}
    }
  
  void initialize_with_X (int q, const double **X, const double *xBar, const double **XtX, const double *XtY);

  void allocate_covariances(int q);
  void allocate_memory();

  double added_SS (const double ZtZ, const double* XtZ, const double ZtY) const;

  void expand_cross_products (const double ZtZ, const double *XtZ, const double ZtY);
  void update_XtXinv ();
  void update_cholesky ();
  void update_beta ();
  void update_RSS ();
  void update_fit_and_residuals();

  std::pair<double,double> compute_bennett_pair (double ZtZ, const double *ZtX);         // returns pair with bennett parms
  void calc_gamma_Z(double zBar, const double* ZtX);      // intercept in b[0]
  void sweep_X_from_Z();
  double RSS_of_Z(double ZtZ, const double *ZtX) const;
  
  gslRegression& operator=(const gslRegression& regr);
};


std::ostream&
operator<<(std::ostream& os, const gslRegression& regr);
