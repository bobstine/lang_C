/* 

 *  Created by Robert Stine on 12/12/05.
 *  Copyright 2005. All rights reserved.

 */

////////////////////////////////////  Extra includes for template.h

#include "debug.h"
#include "bennett.h"
#include "gsl_utils.h"

#include <iomanip>

using namespace debugging;

///////////////////////////////////  Initialize

template<class Data, class Engine>
void
gslRegression<Data, Engine>::initialize()
{
  mN = mpData->n();                                                         // number for estimation, not incld validation
  mQ = 0;   mMaxQ = gslRegression_Max_Q;
  debug("GSLR",0) << "Initializing gsl regression object with n=" << mN << "  max q =" << mMaxQ << std::endl;
  int nBlocks (mN/mBlockSize);
  if (mN != mBlockSize*nBlocks)
  { debug("GSLR",0) << "*** ERROR ***  Invalid block size; " << mBlockSize << " is not a divisor of n = " << mN << std::endl;
    return;
  }
  allocate_memory();
  mXtXinvIsCurrent = false;
  mYBar = center_data_vector(mpData->live_y());                             // centers y to mean zero
  gsl_vector_set_all(mpData->live_Xb(), mYBar);
  mRSS = mTSS = mEngine.sum_of_squares(&gsl_vector_const_subvector(mpData->y(),0,mN).vector);
  mEngine.prepare_vector_for_analysis (mpData->live_e(), mpData->y());      // weights resids if WLS
  /*  peek at the first few y's
      gsl_vector const* y (mpData->y());
      for(int i=0; i<10; ++i)
      std::cout << "gsl regr init y[" << i << "] = " << gsl_vector_get(y,i) << std::endl;
  */
}



//////////////////////////////////  Evaluate predictors, intended to be quick scan of possible variables

template<class Data, class Engine>
template <class Iter>
bool
gslRegression<Data,Engine>::prepare_predictor(std::string const& name, Iter Z) 
{
  // stuff into vector of iterators and pass along
  std::vector< std::pair<std::string, Iter> > vec; 
  vec.push_back( make_pair(name, Z) ); 
  return prepare_predictors(vec); 
}
  
template<class Data, class Engine>
template<class C>
bool
gslRegression<Data,Engine>::prepare_predictors(C predictor_collection)
{ 
  mDimZ = predictor_collection.size();
  for (int j=0; j<mDimZ; ++j)
  { // save centered new predictors into X without increasing mQ
    int qj = mQ + j;                                               // offset from current model predictors
    gsl_vector *Xj(& (gsl_matrix_column(mpData->live_x(),qj)).vector);
    // reads est and validation cases from iterator into data object
    mpData->set_name_of_predictor(predictor_collection[j].first, qj);
    mpData->permuted_copy_from_iterator(predictor_collection[j].second, Xj);
    // centers *all* mLen cases using mean from first n
    gsl_vector_set(mXBar,qj, center_data_vector(Xj));
    // move n rows into Z where data gets weighted as needed
    gsl_vector_view       vZj  (gsl_matrix_column(mZ,j));
    gsl_vector_const_view vXjn (gsl_vector_const_subvector(Xj,0,mN));
    gsl_vector_memcpy (&vZj.vector, &vXjn.vector);
  }
  // update statistics for Z matrix 
  sweep_x_from_z_into_zres();
  mZIsSingular = z_appears_singular();
  if (mZIsSingular) 
    debug("GSLR",2) << " *** Warning ***  Detected singularity when evaluating predictor.\n";
  else {
    compute_cross_products_z();
    compute_partial_coef_z();
  }
  return mZIsSingular;
}


///////////////////////////////////////////////////////////////  Basic internal stat utilities

template <class Data, class Engine>
double 
gslRegression<Data,Engine>::center_data_vector  (gsl_vector *v) const
{
  double avg (mEngine.average(v));  // engine uses only first n cases
  gsl_vector_add_constant(v, -avg);
  return avg;
}


///////////////////////////////////////////////////////////////  Scan, quick eval of new predictors


template <class Data, class Engine>
void
gslRegression<Data,Engine>::sweep_x_from_z_into_zres() 
{
  // copy centered Z into Z res
  gsl_matrix  const*   z  (&gsl_matrix_const_submatrix(mZ, 0,0, mN, mDimZ).matrix);
  gsl_matrix       *zres  (&gsl_matrix_submatrix(mZResids, 0,0, mN, mDimZ).matrix);
  if (mQ == 0) // dont need to sweep anything since only mean in model (weight for WLS)
    mEngine.insert_analysis_matrix (zres, z);
  else
  { // regress each Z on X, leaving coefs in gamma; row j holds coefs of z_j on X
    // Z is centered as is X, so no intercept in gamma = z'x (x'x)i
    gsl_matrix      *   g   (&gsl_matrix_submatrix (mGammaZ, 0,0, mDimZ, mQ).matrix); 
    gsl_vector const* tau   (&gsl_vector_const_subvector(mTau, 0, mQ).vector);
    gsl_matrix const*  qr   (&gsl_matrix_const_submatrix(mQR, 0,0, mN+mQ, mQ).matrix); // pad for shrinkage
    gsl_vector      *  Zj   (gsl_vector_alloc(mN+mQ));                                 //
    gsl_vector      *  e    (gsl_vector_alloc(mN+mQ));                                 //
    for(int j = 0; j<mDimZ; ++j)
    { gsl_vector_view       vZRj (gsl_matrix_column (zres, j));
      gsl_vector_view       vGj  (gsl_matrix_row (g,j));
      // not impl shrinkage  mEngine.prepare_vector_for_analysis(Zj, &vZj.vector);
      gsl_vector_memcpy  (&gsl_vector_subvector(Zj, 0,mN).vector, &gsl_matrix_const_column(z,j).vector);
      gsl_vector_set_all (&gsl_vector_subvector(Zj,mN,mQ).vector, gsl_vector_get(mZBar,j));
      gsl_linalg_QR_lssolve (qr, tau, Zj, &vGj.vector, e);
      gsl_vector_memcpy  (&gsl_matrix_column(zres,j).vector, &gsl_vector_subvector(e,0,mN).vector);
    }
    gsl_vector_free(Zj);
    gsl_vector_free(e);
    /*
    { //  check zres properties: <z,1> = 0   <z,x> = 0    (weighted dot norm)
      double dp (0);
      gsl_blas_ddot(&gsl_matrix_const_column(zres,0).vector, mEngine.sqrt_wts(), &dp);
      debug("GSLR",0) << "TESTING: zres . 1 = " << dp << std::endl;
      gsl_vector *temp (gsl_vector_alloc(mN));
      for(int j=0; j<mQ; ++j) {
	gsl_vector *x    (&gsl_matrix_const_column(mpData->x(),j).vector);
	gsl_vector_memcpy(temp, &gsl_vector_subvector(x,0,mN).vector);
	gsl_vector_mul(temp, mEngine.sqrt_wts());
	gsl_blas_ddot(&gsl_matrix_const_column(zres,0).vector, temp, &dp);
	debug("GSLR",0) << "TESTING: zres . x[" << j << "] = " << dp << std::endl;
      }
      gsl_vector_free(temp);
    }
    */
  }
}


template <class Data, class Engine>
bool
gslRegression<Data,Engine>::z_appears_singular() const
{
  const double nearZero    (1.0e-30);
  const double nearZeroPct (1.0e-5 );
  gsl_matrix_const_view     vZ  (gsl_matrix_const_submatrix(  mZ    , 0,0, mN, mDimZ));
  gsl_matrix_const_view  vZRes  (gsl_matrix_const_submatrix(mZResids, 0,0, mN, mDimZ));
  for(int j=0; j<mDimZ; ++j)
  { // Do we need to check the average squared value of Z when later check the residual variance???    
    gsl_vector_const_view vZj (gsl_matrix_const_column(&vZ.matrix,j));
    gsl_vector    const*   zj (&vZj.vector);
    double ss (0.0);
    gsl_blas_ddot(zj, zj, &ss);
    double avgSS (ss/mN);
    if (avgSS < nearZero) return true;
    // check the amount of residual variation, both in absolute and relative terms
    gsl_vector_const_view vZRj (gsl_matrix_const_column(&vZRes.matrix,j));
    gsl_vector    const* zResj (&vZRj.vector);
    double rss (0.0);
    gsl_blas_ddot(zResj, zResj, &rss);
    double avgRSS (rss/mN);
    if (avgRSS < nearZero) return true;
    if (rss/ss < nearZeroPct) return true;
  }
  return false;
}

template <class Data, class Engine>
void   
gslRegression<Data,Engine>::compute_cross_products_z()
{
  gsl_vector  *ze (&gsl_vector_subvector(mZE,0,mDimZ).vector);
  gsl_matrix  *zz (&gsl_matrix_submatrix(mZZ,0,0,mDimZ,mDimZ).matrix);
  if (mZIsSingular) 
  { // set cross product to zero and ss to 1, then leave
    gsl_vector_set_zero( ze );
    gsl_matrix_set_all ( zz, 1.0);
    return;
  }
  // dot with the current residuals
  gsl_matrix const* z (&gsl_matrix_const_submatrix(mZResids,0,0,mN,mDimZ).matrix);
  gsl_vector const* e (&gsl_vector_const_subvector(mpData->e(), 0, mN).vector);
  gsl_blas_dgemv (CblasTrans, 1.0, z, e, 0.0, ze);  
  gsl_blas_dsyrk(CblasLower, CblasTrans, 1.0, z, 0.0, zz);     // bugged in GSL 1.5; need 1.7
}

template <class Data, class Engine>
void   
gslRegression<Data,Engine>::compute_partial_coef_z()
{
  const gsl_vector      *ze (&gsl_vector_const_subvector(mZE,0,mDimZ).vector);
  const gsl_matrix      *zz (&gsl_matrix_const_submatrix(mZZ,0,0,mDimZ,mDimZ).matrix);
  gsl_vector            *c  (&gsl_vector_subvector(mC,0,mDimZ).vector);
  gsl_vector_set_zero(c);
  if (mZIsSingular)
    return;  
  if (1 == mDimZ)
  { gsl_vector_set(c,0,gsl_vector_get(ze,0)/gsl_matrix_get(zz,0,0));
    return;
  } 
  else
  { gsl_matrix * tzz (mpData->temp_mat(mDimZ, mDimZ));
    gsl_matrix_memcpy (tzz, zz);
    int gslError (0);  
    { gsl_error_handler_t *builtIn (gsl_set_error_handler_off());
      gslError = gsl_linalg_cholesky_decomp(tzz);
      gsl_set_error_handler(builtIn);
    }
    if (gslError) 
      debug("GSLR",3) << "*** Error *** Cholesky decomp of Z'Z is not PSD. Return c = 0.\n";
    else
      gsl_linalg_cholesky_solve (tzz, ze, c);
  }
}
    
template <class Data, class Engine>
void
gslRegression<Data,Engine>::compute_fitted_values(int nUse)
{  
  if (0 == mQ) return;   // nothing to do; Xb() initialized with y-bar
  const gsl_matrix *X   (&gsl_matrix_const_submatrix(mpData->x(),0,0,nUse,mQ).matrix);
  const gsl_vector *b   (&gsl_vector_const_subvector(mBeta,0,mQ).vector); // no intercept
  gsl_vector       *Xb  (&gsl_vector_subvector(mpData->live_Xb(),0,nUse).vector);
  gsl_vector_set_all (Xb, mYBar);                                         // init with mean y since X centered, d<-s
  gsl_blas_dgemv(CblasNoTrans, 1.0, X, b, 1.0, Xb);                       // add others
}


/////////////////////////////////////////////////////////////////////////////////  Added Sums of Squares, dRSS


template <class Data, class Engine>
double
gslRegression<Data,Engine>::white_f_stat()
{
  if (mZIsSingular) return 0.0;
  // find Z'EE Z = Z'DZ
  gsl_matrix             *zdz  (mpData->temp_mat(mDimZ, mDimZ));
  gsl_matrix_set_zero(zdz);
  gsl_matrix       const*   z  (&gsl_matrix_const_submatrix(mZResids, 0,0, mN, mDimZ).matrix);
  if(mBlockSize == 1)
    mEngine.blas_dsyr(z, mpData->e(), zdz);
  else
    mEngine.blas_dsyr(mBlockSize,z,mpData->e(), zdz);
  // scalar case avoids matrices
  if (1 == mDimZ)
  { double zz00   (gsl_matrix_get(mZZ,0,0));
    double zdz00  (gsl_matrix_get(zdz,0,0));
    if (zdz00 < 1.0e-30)
    { debug("GSLR",2) << "Warning; near singular White variance. Setting to zero.\n";
      return 0.0;
    }
    double ze (zz00 * gsl_vector_get(mC,0));
    return ze * ze / zdz00;   // s2 imbedded in zdz
  }
  // invert (Z'DZ)º  with cholesky decomp
  int gslError (0);  
  { gsl_error_handler_t *builtIn (gsl_set_error_handler_off());
    gslError = gsl_linalg_cholesky_decomp(zdz);
    gsl_set_error_handler(builtIn);
  }
  if (gslError) 
  { debug("GSLR",3) << " *** Error ***   Z'DZ not PSD in Cholesky decomp. Return dRSS = 0.\n";
    return 0.0;
  }
  //  e'Z (Z'DZ)º (Z'e)
  gsl_vector  const* ze (&gsl_vector_const_subvector(mZE,0,mDimZ).vector);
  gsl_vector       *tmp (gsl_vector_alloc(mDimZ));
  gsl_linalg_cholesky_solve (zdz, ze, tmp);   // e'Z (ZDZ)º
  double fStat;
  gsl_blas_ddot(ze,tmp,&fStat);               // e'Z (Z'Z) Z'e
  gsl_vector_free (tmp);
  return fStat/mDimZ;
}


template <class Data, class Engine>
double 
gslRegression<Data,Engine>::change_in_rss ()  const 
{
  if (mZIsSingular)
  { debug("GSLR",0) << "Singularity among added predictors\n";
    return 0.0;
  }
  const gsl_matrix *zz;
  zz = &(gsl_matrix_const_submatrix (mZZ,0,0,mDimZ, mDimZ)).matrix;
  if (1 == mDimZ) 
  { double c (gsl_vector_get(mC,0));
    return c * c * gsl_matrix_get(zz,0,0);
  }
  gsl_vector_const_view vC  (gsl_vector_const_subvector (mC,0,mDimZ));
  const gsl_vector      *c  (&vC.vector);
  gsl_vector_view       tmp (gsl_vector_subvector(mpData->temp_vec(0),0,mDimZ));
  gsl_blas_dsymv(CblasLower, 1.0, zz, c, 0.0, &tmp.vector);   // (Z'Z) c
  double ss;
  gsl_blas_ddot(c,&tmp.vector,&ss);                           // c'(Z'Z)c
  return ss;
}


///////////////////////////////////////////////////////////////  Change weights used in GSL engine


template <class Data, class Engine>
void 
gslRegression<Data,Engine>::reweight(gsl_vector const* newWeights)                            // changes the estimates
{
  gsl_vector *weights  (mEngine.weights());
  debug("GSLR",0) << "Reweighting; initial weights are " << gsl_vector_get(newWeights,0) << " " << gsl_vector_get(newWeights,1) << std::endl;
  gsl_vector_memcpy(weights, newWeights);
  mEngine.weights_have_changed();
  mYBar += center_data_vector(mpData->live_y());       // re-center Y
  if (mQ > 0) {            
    for (int j=0; j<mQ; ++j) {                         // re-center X's
      gsl_vector_view vXj (gsl_matrix_column(mpData->live_x(),j));
      double mXj (gsl_vector_get(mXBar,j));
      gsl_vector_set(mXBar,j,mXj + center_data_vector(&vXj.vector));  }
    qr_decomposition(0,mQ);                            // refactors and reweights the world
  }
}

template <class Data, class Engine>
void 
gslRegression<Data,Engine>::reweight(gsl_vector const* newWeights, gsl_vector const* newY)    // also inserts new response
{
  debug("GSLR",0) << "Replacing response in model with = " << mQ << " predictors.\n";
  gsl_vector_memcpy (mpData->live_y(), newY);
  mYBar = 0.0;          // reset in reweighting                                                    
  reweight(newWeights);
}
  

///////////////////////////////////////////////////////////// Add predictors

template <class Data, class Engine>
int 
gslRegression<Data,Engine>::add_current_predictors ()
{
  if (mQ+mDimZ >= mMaxQ)
  { std::clog << "GSLR: Attempt to add too many predictors; q " << mQ+mDimZ << ">=" << mMaxQ << std::endl;
    return 0;
  }
  else  // NOTE: most recent predictors are in mDimZ past column mQ in mX
  { debug("GSLR",1) << "Adding " << mDimZ << " predictors (model has " << mQ << " predictors)... \n";
    int status (0);
    prepare_shrinkage();
    status = qr_decomposition(mQ, mDimZ);   // increments mQ += mDimZ if successful
    status += update_XtXinv();
    if (status)
    { debug("GSLR",3) << " *** Error ***    Cannot invert X'X.\n"; 
      mQ -= mDimZ;
    }
    return mQ;
  }
}

template <class Data, class Engine>
  void
  gslRegression<Data,Engine>::prepare_shrinkage ()
{
  double lambda (0.0);
  gsl_matrix   const  *X (&(gsl_matrix_const_submatrix(mpData->x(),0,0, mN,mQ+mDimZ)).matrix);
  for (int j = mQ; j < mQ + mDimZ; ++j)
  { gsl_vector const* xj (&gsl_matrix_const_column(X,j).vector);
    double ssx (0.0);
    gsl_blas_ddot (xj,xj, &ssx);
    lambda = sqrt(ssx/mWhiteF);
    debug("GSLR",2) << " Column " << j << " [sandwich F=" << mWhiteF << ", ssx=" << ssx
		    << "] --> shrinkage diagonal lambda=" << lambda << std::endl;
    gsl_vector_set(mShrinkage,j,lambda);
  }
}


template <class Data, class Engine>
int
gslRegression<Data,Engine>::qr_decomposition (int firstColumn, int numberColumns)
{
  const int newQ (mQ + mDimZ);
  assert((firstColumn + numberColumns) == newQ);
  // overwrite QR with the data including
  gsl_matrix   const  * X    (&(gsl_matrix_const_submatrix(mpData->x(),0,0, mN,newQ)).matrix);
  gsl_matrix          * QR   (&(gsl_matrix_submatrix      (mQR,        0,0, mN,newQ)).matrix);
  mEngine.insert_analysis_matrix (QR, X);  // dest <- src
  // insert shinkage terms on diagonal after zero out; ??? not right if collinear
  gsl_matrix_set_zero(&(gsl_matrix_submatrix(mQR, mN,0,    newQ,newQ)).matrix);
  QR   =              &(gsl_matrix_submatrix(mQR,  0,0, mN+newQ,newQ)).matrix;
  for(int j=0; j<newQ; ++j)
    gsl_matrix_set(QR,mN+j,j, gsl_vector_get(mShrinkage,j));
  // factor matrix
  int status (0);
  gsl_vector *tau  (&gsl_vector_subvector(mTau, 0, newQ).vector);
  debug("GSLR",0) << "Refactoring matrix of " << newQ << " columns with shrinkage inserted.\n";
  { gsl_error_handler_t *builtIn (gsl_set_error_handler_off());
    status = gsl_linalg_QR_decomp(QR, tau); 
    gsl_set_error_handler(builtIn);
  }
  if (status)   debug("GSLR",0) << "Warning. Status of QR decomp is " << status << std::endl;
  else          mQ = newQ;                                                          // reset Q to have new variables
  /*
    This block of code controlled building QR when no shrinkage
  // copy new portions of X into QR
  gsl_matrix   const  * X    (&(gsl_matrix_const_submatrix(mpData->x(),0,firstColumn, mN,numberColumns)).matrix);
  gsl_matrix          * QR   (&(gsl_matrix_submatrix      (mQR,        0,firstColumn, mN,numberColumns)).matrix);
  mEngine.insert_analysis_matrix (QR, X);  // dest <- src, with weighting as needed
  // update QR for the new columns
  int status (0);
  const int newQ (firstColumn + numberColumns);
  QR = &gsl_matrix_submatrix(mQR, 0,0, mN, newQ).matrix;
  gsl_vector_view  vTau  (gsl_vector_subvector(mTau, 0, newQ));
  if (0 == firstColumn)
  { debug("GSLR",0) << "Refactoring matrix of " << mQ << " columns.\n";
    gsl_error_handler_t *builtIn (gsl_set_error_handler_off());
    status = gsl_linalg_QR_decomp(QR, &vTau.vector); 
    gsl_set_error_handler(builtIn);
  }
  else 
    status = gsl_linalg_partial_QR_decomp (QR, &vTau.vector, firstColumn);
  */
  /*
   { // debugging code that prints full QR decomposition
     gsl_matrix *q (gsl_matrix_alloc(n,n));
     gsl_matrix *r (gsl_matrix_alloc(n,newQ));
     gsl_linalg_QR_unpack (&vQR.matrix, &vTau.vector, q, r);
     gsl_matrix_const_view vQ (gsl_matrix_const_submatrix(q,0,0,n,newQ));
     gsl_matrix_const_view vR (gsl_matrix_const_submatrix(r,0,0,newQ,newQ));
     debug("GSLR",0)<< " ********  Q " << &vQ.matrix;
     debug("GSLR",0) << " ********  R " << &vR.matrix;
     gsl_matrix_free(q);
     gsl_matrix_free(r);
   }
  */
  gsl_vector const* Y   (&gsl_vector_const_subvector(mpData->y(),0,mN).vector);          // store beta and residuals
  gsl_vector    *beta   (&gsl_vector_subvector(mBeta,0,mQ).vector);
  gsl_vector     *res   (&gsl_vector_subvector(mpData->live_e(),0,mN).vector);
  { gsl_vector  *e (gsl_vector_alloc(mN+mQ));
    gsl_vector  *y (gsl_vector_alloc(mN+mQ));                                           // longer vectors for shrinkage
    gsl_vector_memcpy  (&gsl_vector_subvector(y,  0, mN).vector, Y);
    gsl_vector_set_all (&gsl_vector_subvector(y, mN, newQ).vector, mYBar);
    // not imp shrinkage
    // mEngine.prepare_vector_for_analysis (y, &vY.vector);                               // resulting resids weighted by W^.5 if WLS
    gsl_error_handler_t *builtIn (gsl_set_error_handler_off());
    status = gsl_linalg_QR_lssolve (QR, tau, y, beta, e);
    gsl_set_error_handler(builtIn);
    gsl_vector_memcpy(res, &gsl_vector_subvector(e,0,mN).vector);
    gsl_vector_free(y);
    gsl_vector_free(e);
  }
  if (status)
    debug("GSLR",2) << "Warning. Status of QR lssolve is " << status << std::endl;
  else
  { gsl_blas_ddot(res, res, &mRSS);
    compute_fitted_values(len());
  }
  mXtXinvIsCurrent = false;
  return status;
}

      
template <class Data, class Engine>
template <class Iter>
void
gslRegression<Data,Engine>::fill_with_fitted_values(Iter fit) const
{  
  mpData->permuted_copy_to_iterator(mpData->Xb(), fit, len());
}

template <class Data, class Engine>
std::pair<double,double>
gslRegression<Data,Engine>::sums_of_squares () const
{
  gsl_vector const* y (mpData->y());
  gsl_vector const* f (mpData->Xb());
  // test that matches built-in residual SS computed in QR
  /*  for(int i=0; i<mN; ++i)
      { diff = mYBar + gsl_vector_get(y,i)-gsl_vector_get(f,i);
        rssIn += diff * diff;
      }
      debug(0) << "GSLR: Built in RSS=" << mRSS << " ; calculated RSS=" << rssIn << std::endl;
  */
  double rssOut (0.0);
  for(int i=mN; i<len(); ++i)                                      // note that it starts at mN thru len (validation cases)
  { double diff = mYBar + gsl_vector_get(y,i)-gsl_vector_get(f,i); // add mean back to uncenter y
    rssOut += diff * diff;
  }
  return std::make_pair(mRSS, rssOut);
}

template <class Data, class Engine>
template <class Iter> 
void 
gslRegression<Data,Engine>::fill_with_beta(Iter begin) const
{
  *begin = intercept();
  for (int j=0; j<q(); ++j, ++begin)
    *begin = gsl_vector_get(mBeta,j);
}

template<class Data, class Engine>
template<class Iter>      
void 
gslRegression<Data,Engine>::fill_with_diagonal_XtXinv(Iter begin, double scale) const
{
  for (int j=0; j<mQ; ++j, ++begin)
    *begin = scale * gsl_matrix_get(mXtXinv,j,j);
}


template <class Data, class Engine>
int
gslRegression<Data,Engine>::update_XtXinv ()
{
  if (mXtXinvIsCurrent) { return 0;  }
  int status (0);
  gsl_matrix  const* R    (&gsl_matrix_const_submatrix(mQR, 0,0, mQ,mQ).matrix);
  // first find the inverse of R
  gsl_matrix       * Rinv (gsl_matrix_alloc(mQ,mQ));
  for (int j=0; j<mQ; ++j)
  { gsl_vector *RIj (&gsl_matrix_row(Rinv, j).vector);
    gsl_vector_set_basis (RIj,j);
    status += gsl_linalg_QR_Rsvx (R, RIj);
  }
  // square it  
  gsl_matrix *   XtXinv  (&gsl_matrix_submatrix(mXtXinv, 0,0, mQ,mQ).matrix);
  status += gsl_blas_dsyrk(CblasLower, CblasTrans, 1.0, Rinv, 0.0, XtXinv); 
  gsl_matrix_free(Rinv);
  if(0==status) mXtXinvIsCurrent = true;
  return status;
}

template <class Data, class Engine>
double
gslRegression<Data,Engine>::intercept() const
{
  double b0 (0.0);
  if (mQ > 0)
  { gsl_vector_const_view vB  (gsl_vector_const_subvector(mBeta, 0, mQ));
    gsl_vector_const_view vXB (gsl_vector_const_subvector(mXBar, 0, mQ));
    gsl_blas_ddot(&vB.vector, &vXB.vector, &b0);
  }
  return (mYBar - b0);
}
    


////////////////////////////////////////////////////////  Bennett


namespace {
  double
  abs_val(double x)
  {
    return (x >= 0.0) ? x : -x;
  }
  
  double
  max_abs(double x, double y)
  {
    double ax = abs_val(x);
    double ay = abs_val(y);
    if (ax >= ay) return ax; else return ay;
  }
  
  double 
    binomialVar(double, double p) { return p * (1.0 - p); }
  
  double    
    whiteVar(double e, double) { return e * e; }
}



template <class Data, class Engine>
  std::pair<double,double>
  gslRegression<Data,Engine>::Bennett_evaluation (double m, double M)
{
  const gsl_vector * z0 (&gsl_matrix_const_column(mZResids,0).vector);// include weights^(1/2) as needed
  const double     * pZ (gsl_vector_const_ptr(z0,0));
  const double     * pY (gsl_vector_const_ptr(mpData->y(),0)); 
  const double     *pMu (gsl_vector_const_ptr(mpData->Xb(),0));       // current fit
  return Bennett_evaluation(pZ, pY, pMu, m, M);
}

template <class Data, class Engine>
  std::pair<double,double>
  gslRegression<Data,Engine>::Bennett_evaluation (double const* z, double const* y, double const* mu, double m, double M)
{
  if (mDimZ != 1)
    debug("GSLR",3) << "Warning. Bennett evaluation for first Z only. \n";
  
  // pick function that computes variance
  double (*var) (double,double) (((0 == m) && (1 == M)) ? binomialVar : whiteVar);

  double num   (0.0);
  double maxA  (0.0);
  double sumB2 (0.0);
  double absZ  (0.0);
  const double epsilon (1.0E-10);
  for (int i=0; i<mN; ++i, ++z, ++y, ++mu) {
    double fit = *mu;
    if (fit <= m)         fit = m + epsilon;          // handle values outside boundary
    else if (fit >= M)    fit = M - epsilon;
    double dev = (*y - *mu);
    num += *z * dev;
    absZ = abs_val(*z) * max_abs(fit-m, M-fit);       // largest possible error   
    if (absZ > maxA) maxA = absZ;                     // largest in this column?
    sumB2 = sumB2 + (*z)*(*z) * var(dev,fit);         // squared z residual times variance of fit
  }
  double rootZDZ (sqrt(sumB2));                       // sum B2 \approx gsl_matrix_get(mZZ,0,0)
  double Mz      (maxA/rootZDZ);
  double tz      (abs_val(num)/rootZDZ);              // num is gsl_vector_get(mZE,0) in usual case
  return std::make_pair(tz, bennett_p_value(tz,Mz));
}


///////////////////////////////////  Printing  //////////////////////////////////

template <class Data, class Engine>
void
gslRegression<Data,Engine>::print_header_to (std::ostream &os) const
{
  os << "     GSLR model [" ; 
  mEngine.print_header_to(os);
  os << " n = " << mpData->n() << ", q = " << mQ << " with limit " << mMaxQ << "]" << std::endl;  // Show SS and beta
  // os << "    Total SS " << mTSS << std::endl;
  // os << " Residual SS " << mRSS << " R2 = " << 1.0-mRSS/mTSS << std::endl;
  os << "        Beta  : " << intercept() << " ";
  for (int j=0; j<mQ; ++j)
    os << std::setw(10) << gsl_vector_get(mBeta,j) << " ";
  os << std::endl;
  os << "     Shrinkage terms: " << &gsl_vector_const_subvector(mShrinkage,0,mQ).vector << std::endl;
}

template <class Data, class Engine>
void
gslRegression<Data,Engine>::print_to (std::ostream &os, int depth) const
{
  print_header_to(os);
  if (0 == depth) return;
  os << "\n - - - - - - - - - - - - - - - - - \n      Leading residuals: ";
  for (int i=0; i<depth; ++i) 
    os << gsl_vector_get(mpData->e(),i) << "  ";
  os << std::endl;
  // Show first part of data 
  os << "   P     Y (avg " << std::setw(6) << mYBar << ")  ";
  for (int j=0; j<mQ; ++j)
    os << "X" << j << "(" << std::setw(6) << gsl_vector_get(mXBar,j) << ") ";
  os << std::endl;
  for (int i=0; i<depth; ++i) {
    os << std::setw(6) << gsl_vector_get(mpData->y(),i) << " ";
    for (int j=0; j<mQ; ++j)
      os << std::setw(6) << gsl_matrix_get(mpData->x(),i,j) << " ";  
    os << std::endl;
  }
}

template <class Data, class Engine>
void
gslRegression<Data,Engine>::write_data_to (std::ostream &os) const
{
  int len (mpData->length());
  std::clog << "GSLR: Writing " << len << " rows to output with " << mQ << " columns of X.\n";
  // header information in output file for reading into spreadsheet; names of first 4 columns are
  os << "Included,Error,Fit,Y";
  for (int j=0; j<mQ; ++j) os << "," << mpData->x_names()[j];
  os << std::endl;
  // need to restore the data to the order read in initially
  const int* permute (mpData->permutation());
  gsl_vector const* y (mpData->y());
  gsl_matrix const* X (mpData->x());
  gsl_vector const* Xb(mpData->Xb());
  for(int i=0; i<len; ++i)
  { int row = permute[i];
    if (row < mN)
      os << " in, ";  // estimation sample
    else
      os << "out, ";  // validation
    double yHat (gsl_vector_get(Xb,row));
    double yObs (gsl_vector_get(y,row) + mYBar);
    os << (yObs - yHat) << "," << yHat << "," << yObs;
    for(int j=0; j<mQ; ++j)
      os << "," << gsl_matrix_get(X,row,j) + gsl_vector_get(mXBar,j);   // add back the mean
    os << std::endl;
  }
}


template <class Data, class Engine>
void
gslRegression<Data,Engine>::restore_state(gslRegressionState const& state)
{
  debug("GSLR",0) << "Restoring state from external object.\n";
  // can only restore to a smaller model with equal number cases
  if ((state.q() > mQ) || (state.n() != mN))
  { debug("GSLR",3) << "  *** Error ***   Cannot restore from object; its dimensions(" 
	  << state.n() << "," << state.q() << ") do not conform.\n";
	  return; }
  mQ = state.q();
  mYBar = state.yBar();
  mRSS = state.rss();
  gsl_vector_view view (gsl_vector_subvector (mpData->live_y(),0,mN));
  gsl_vector_memcpy (&view.vector, state.y());
  view = gsl_vector_subvector(mpData->live_e(),0,mN);
  gsl_vector_memcpy (&view.vector, state.res());
  view = gsl_vector_subvector(mpData->live_w(),0,mN);
  gsl_vector_memcpy (&view.vector, state.wts());
  if (mQ > 0)
  { view = gsl_vector_subvector(mBeta,0,mQ);
    gsl_vector_memcpy (&view.vector, state.beta());
    view = gsl_vector_subvector(mXBar,0,mQ);
    gsl_vector_memcpy (&view.vector, state.xBar());
  }
  mXtXinvIsCurrent = false;
}

/////////////////////////////////////////////////////////////  Allocating memory

template <class Data, class Engine>
void
gslRegression<Data,Engine>::allocate_memory()
{
  debug("GSLR",0) << "Allocating memory with n = " << mN << ", max q = " << mMaxQ << std::endl;
  // regr terms
  mBeta    = gsl_vector_alloc(1+mMaxQ);                    // room for intercept
  mC       = gsl_vector_alloc(mMaxQ);
  mXBar    = gsl_vector_alloc(mMaxQ);
  mTau     = gsl_vector_alloc(mMaxQ);
  mShrinkage=gsl_vector_alloc(mMaxQ);
  mXtXinv  = gsl_matrix_alloc(mMaxQ,mMaxQ);
  mQR      = gsl_matrix_alloc(mN + mMaxQ, mMaxQ);         // padded with identity for shrinkage
  mZ       = gsl_matrix_alloc(mN, gslRegression_Max_P);
  mZResids = gsl_matrix_alloc(mN, gslRegression_Max_P);
  mZBar    = gsl_vector_alloc(gslRegression_Max_P);

  // covar terms
  mZE      = gsl_vector_alloc(gslRegression_Max_P);
  mZZ      = gsl_matrix_alloc(gslRegression_Max_P, gslRegression_Max_P);
  mZX      = gsl_matrix_alloc(gslRegression_Max_P, mMaxQ);
  mGammaZ  = gsl_matrix_alloc(gslRegression_Max_P, mMaxQ); // no intercept
}

template <class Data, class Engine>
gslRegression<Data,Engine>::~gslRegression()
{
  std::clog << "GSLR: Freeing gslRegression with n = " << mN 
            << ", q = " << mQ << ", maxQ = " << mMaxQ << std::endl;
  if (mBeta)    gsl_vector_free(mBeta);
  if (mC)       gsl_vector_free(mC);
  if (mXBar)    gsl_vector_free(mXBar);
  if (mTau)     gsl_vector_free(mTau);
  if(mShrinkage)gsl_vector_free(mShrinkage);
  if (mXtXinv)  gsl_matrix_free(mXtXinv);
  if (mQR)      gsl_matrix_free(mQR);
  if (mZBar)    gsl_vector_free(mZBar);
  if (mZ)       gsl_matrix_free(mZ);
  if (mZResids) gsl_matrix_free(mZResids);
  if (mZE)      gsl_vector_free(mZE);
  if (mZZ)      gsl_matrix_free(mZZ);
  if (mZX)      gsl_matrix_free(mZX);
  if (mGammaZ)  gsl_matrix_free(mGammaZ);
}
/*
template <class Data, class Engine>
gslRegression<Data, Engine>::gslRegression (const gslRegression<Data, Engine>& regr)
{
  std::clog << "GSLR: copy constructor called"  << std::endl;
  mMaxQ = regr.mMaxQ;
  mLen = regr.mLen;
  mQ = regr.mQ;
  mRSS = regr.mRSS;
  allocate_memory();
  std::copy(regr.mPermute,   regr.mPermute+mLen, mPermute);
  gsl_vector_memcpy(mY,      regr.mY);
  gsl_matrix_memcpy(mX,      regr.mX);
  gsl_matrix_memcpy(mZ,      regr.mZ);
  gsl_matrix_memcpy(mZResids,regr.mZResids);
  gsl_vector_memcpy(mZBar,   regr.mZBar);
  gsl_matrix_memcpy(mZZ,     regr.mZZ);
  gsl_matrix_memcpy(mZX,     regr.mZX);
  gsl_vector_memcpy(mZE,     regr.mZE);
  gsl_vector_memcpy(mBeta,   regr.mBeta);
  gsl_matrix_memcpy(mGammaZ, regr.mGammaZ);
  gsl_matrix_memcpy(mXtXinv, regr.mXtXinv);
  gsl_matrix_memcpy(mQR,     regr.mQR);  
  gsl_vector_memcpy(mTau,    regr.mTau);
  gsl_vector_memcpy(mResids, regr.mResids);
  gsl_vector_memcpy(mXb,     regr.mXb);
}

*/


