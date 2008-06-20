#include "gsl/gsl_matrix.h"

#include "range_stats.h"

#include <iostream>
#include <iterator>

/*
  The GSL regression object defines an API to the GSL matrix routines.
  All IO from these is as a pointer to double, *except* for the input
  vector which are allowed to be arbitrary iterators.

  In particular, there is *no* range code in this object.  This is real
  low-level stuff, and its all with pointers really.

  10 Dec 02 ... Created to support new model code, with added iterators.
*/

class gslMatrixRowIterator;


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
  gsl_vector *mXtY;
  
  gsl_vector *mResids;
  gsl_vector *mFit;

 public:
  
  ~gslRegression ();

  //  Initialize

  gslRegression (const gslRegression& regr);
  
  template<class Range>
    gslRegression (int maxQ, Range r)
    { initialize_from_range(maxQ, r); }

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

  
  //  --- Consider new predictors (test stat, nominal p-value) ---
  
  std::pair<double,double>
    Gaussian_evaluation(double ZtZ, const double *ZtX, double YtZ) const;
  
  template<class Iter>
    std::pair<double,double>
    Bennett_evaluation(Iter Z, double zBar, double ZtZ, const double* ZtX)
    {
      mZBar = zBar;
      std::copy(Z, Z + mN, gsl_vector_ptr(mZ,0));
      return compute_bennett_pair(ZtZ, ZtX);
    }

  
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

  const gsl_vector* Y() const { return mY; }
  const gsl_matrix* X() const { return mX; }
  const double   YBar() const { return mYBar; }
  const double*  XBar() const { return gsl_vector_ptr(mXBar,0); }
  const double*  beta() const { return gsl_vector_ptr(mBeta,0); }        // intercept in b[0]
  const double* gamma() const { return gsl_vector_ptr(mGammaZ,0); }        // intercept in b[0]
  
  double TotalSS()      const {  return mTotalSS; }
  double ResidualSS()   const {  return mRSS; }
  int    DFFit()        const {  return mQ+1; }
  int    DFResiduals()  const {  return mN - mQ - 1; }

  const double* Residuals()   const {  return gsl_vector_ptr(mResids,0); }
  const double* Fit()         const {  return gsl_vector_ptr(mFit,0); }


  //  --- Printing access ---

  void print_to (std::ostream &os, int depth) const;
  
 private:


  template<class Range>
    void initialize_from_range(int maxQ, Range r)
    {
      mMaxQ    = maxQ;   mQ = 0;
      mN       = range_ops::length(r);
      mYBar    = average(r,mN);
      mTotalSS = ySS = sum_of_squares(r, mYBar);
      mRSS     = mTotalSS;
      allocate_memory();
      gsl_vector_set(mBeta,0,yBar);
      for(int i=0; i<mN; ++i)
	{
	  gsl_vector_set(mFit,i, yBar);
	  gsl_vector_set(mResids,i, *Y-yBar);
	  gsl_vector_set(mY,i, *Y);
	  ++Y;
	}
    }

  template<class Iter>
    void initialize_with_Y(int maxQ, int n, Iter Y, double yBar, double ySS)
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

  void allocate_memory();

  double added_SS (const double ZtZ, const double* XtZ, const double ZtY) const;

  void expand_cross_products (const double ZtZ, const double *XtZ, const double ZtY);
  void update_XtXinv ();
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

class gslMatrixRowIterator : public std::iterator<std::random_access_iterator_tag, const double*>
{
  gsl_matrix mMatrix;
  int mRow;
  const int mCol;
  typedef gslMatrixRowIterator type_of_this;

 public:
  
  gslMatrixRowIterator (const gsl_matrix& m)
    : mMatrix(m),
    mRow(0),
    mCol(5 /*m.Q()*/)
    { }

  gslMatrixRowIterator (const gslMatrixRowIterator& it)
    : mMatrix (it.mMatrix),
    mRow(it.mRow), 
    mCol(it.mCol)
    { }
  
  std::pair<const double*, const double*>
    operator* () 
    {
      const double* begin ((*gsl_matrix_ptr) (&mMatrix, mRow, 0));
      return std::make_pair(begin, begin+mCol);
    }
  
  void operator++ ()    { ++ mRow; }
  void operator-- ()    { -- mRow; }

  gslMatrixRowIterator
    operator+(int n)  const
    {
      gslMatrixRowIterator result(*this);
      result += n;
      return result;
    }
  
  gslMatrixRowIterator
    operator-(int n)  const
    {
      gslMatrixRowIterator result(*this);
      result -= n;
      return result;
  }
  
  gslMatrixRowIterator&
    operator+=(int n) { mRow += n; return *this;}
 
  gslMatrixRowIterator&
    operator-=(int n) { mRow -= n; return *this;}

  std::iterator_traits<gslMatrixRowIterator>::difference_type
    operator-(const type_of_this& it) { return mRow - it.mRow; }

  bool
    operator<(const gslMatrixRowIterator& it) const { return mRow < it.mRow; }
  bool
    operator>(const gslMatrixRowIterator& it) const { return mRow > it.mRow; }
};
