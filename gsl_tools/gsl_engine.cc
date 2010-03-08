// $Id: gsl_engine.cc,v 1.16 2008/01/16 03:28:08 bob Exp $

#include "gsl_engine.h"
#include "gsl_utils.h"

// sqrt
#include <math.h>

// smoothing
#include "smoothing_spline.h"

#include <gsl/gsl_matrix.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_linalg.h>


//   Ordinary GSL Engine      Ordinary GSL Engine      Ordinary GSL Engine      Ordinary GSL Engine      

void 
olsEngine::configure (gslData *data)
{ 
  std::cout << "OLSE: Configuring OLS engine with n = " << mN << " with gslData pointer " << data << std::endl;
}


double 
olsEngine::average (gsl_vector const* v) const                                                   
{
  double s (0.0);

  for (int i=0; i<mN; ++i)
    s += gsl_vector_get(v,i);
  return (s/mN);
}  


double 
olsEngine::sum_of_squares (gsl_vector const* v, double mean) const
{
  double ss (0.0);
  
  for (int i=0; i<mN; ++i)
  { double dev (gsl_vector_get(v,i)-mean);
    ss += dev * dev;
  }
  return (ss);
}


double 
olsEngine::sum_of_squares (gsl_vector const* v) const
{
  double ss (0.0);
  gsl_blas_ddot(v,v, &ss);
  return ss;
}

void
olsEngine::blas_ddot (gsl_vector const* x, gsl_vector const* y, double *dp) const
{ 
  gsl_blas_ddot (x, y, dp);
}


void
olsEngine::blas_dsyr(gsl_matrix const* z, gsl_vector const*e, gsl_matrix *zdz) const
{
  for (int i=0; i<mN; ++i) // accum e_i^2 z_i z_i'
  { double                 ei (gsl_vector_get(e,i));
    gsl_vector_const_view  zi (gsl_matrix_const_row(z, i));
    gsl_blas_dsyr(CblasLower, ei*ei, &zi.vector, zdz);
  }
}

double
olsEngine::smooth (int df, gsl_vector const*x, gsl_vector const* y, gsl_vector *smth) const
{
  SmoothingSpline ss(df, x->data, y->data, mN); 
  std::cout << "########### Smoothing spline initialized with vector length " << mN << " out of " << x->size << std::endl;
  ss.fill_with_smooth(df, smth->data);
  return sum_of_squares(smth, average(smth));
}


//   Weighted GSL Engine      Weighted GSL Engine      Weighted GSL Engine      Weighted GSL Engine      


void 
wlsEngine::configure(gslData *data) 
{ 
  std::cout << "WLSE: Configuring WLS engine with n = " << mN << std::endl;
  mWeights = data->live_w();
  // allocate weights if we had none
  if (!mSqrtW) mSqrtW = gsl_vector_alloc(mN);
  // put sqrt of weights into local vector
  weights_have_changed();
}

void         
wlsEngine::weights_have_changed ()
{
  for (int i=0; i<mN; ++i)
  { double wi (gsl_vector_get(mWeights,i));
    if (wi <= 0.0)
    { std::cout << "WLSE:   Warning. Case " << i << " assigned weight 0. Ought to exclude.\n";
      wi = 0.000000001; 
    }
    gsl_vector_set(mSqrtW,i,sqrt(wi));
  }
}



double 
wlsEngine::average (gsl_vector const* v) const
{
  double num  (0.0);
  double den  (0.0);
  for (int i=0; i<mN; ++i)
  { double w (gsl_vector_get(mWeights,i));
    num += gsl_vector_get(v,i) * w;
    den += w;
  }
  return (num/den);  
}



double 
wlsEngine::sum_of_squares (gsl_vector const* v, double mean) const
{
  double ss (0.0);
  for (int i=0; i<mN; ++i)
  { double dev (gsl_vector_get(v,i)-mean);
    double w   (gsl_vector_get(mWeights,i));
    ss += dev * dev * w;
  }
  return (ss);
}


double 
wlsEngine::sum_of_squares (gsl_vector const* v) const
{
  double ss (0.0);
  for (int i=0; i<mN; ++i)
  { double dev (gsl_vector_get(v,i));
    double w   (gsl_vector_get(mWeights,i));
    ss += dev * dev * w;
  }
  return (ss);
}


double
wlsEngine::smooth (int df, gsl_vector const*x, gsl_vector const* y, gsl_vector *smth) const
{
  SmoothingSpline ss(df, x->data, y->data, mWeights->data, (int) x->size);
  ss.fill_with_smooth(df, smth->data);
  return sum_of_squares(smth, average(smth));
}


  
void   
wlsEngine::insert_analysis_matrix (gsl_matrix *dest, gsl_matrix const* src) const
{
  gsl_matrix_memcpy(dest, src);
  // apply weights to the columns of dest
  for (size_t j=0; j<dest->size2; ++j)
  { gsl_vector_view dj (gsl_matrix_column(dest,j));
    gsl_vector_mul(&dj.vector, mSqrtW);
  }
}

void   
wlsEngine::prepare_vector_for_analysis (gsl_vector *dest, gsl_vector const* src) const
{ 
  gsl_vector_memcpy(dest, src);
  gsl_vector_mul(&(gsl_vector_subvector(dest,0,mN).vector), mSqrtW);
}

void
wlsEngine::unweight (gsl_vector *vec) const
{
  gsl_vector_div(&(gsl_vector_subvector(vec,0,mN).vector), mSqrtW);
}

void 
wlsEngine::blas_ddot(gsl_vector const* x, gsl_vector const* y, double *dp) const
{
  blas_ddot(x,y,mWeights,dp);
}


void 
wlsEngine::blas_ddot(gsl_vector const* x, gsl_vector const* y, gsl_vector const* w, double *dp) const
{ 
  *dp = 0.0;  
  for(size_t i=0; i<x->size; ++i)
    *dp += gsl_vector_get(x,i) * gsl_vector_get(y,i) * gsl_vector_get(w,i);
}


void
wlsEngine::blas_dsyr(gsl_matrix const* z, gsl_vector const*e, gsl_matrix *zdz) const
{
  for (int i=0; i<mN; ++i) // accum e_i^2 z_i z_i'
  { double                 ei (gsl_vector_get(e,i));
    gsl_vector_const_view  zi (gsl_matrix_const_row(z, i));
    gsl_blas_dsyr(CblasLower, ei*ei, &zi.vector, zdz);
  }
}



