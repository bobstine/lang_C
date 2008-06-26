// $Id: gsl_regr.cc,v 1.39 2007/12/16 17:59:27 bob Exp $

#include <gsl/gsl_errno.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_linalg.h>

#include "gsl_regr.h"


namespace {
  
  size_t 
  min_size(size_t x, size_t y) { if (x < y) return x; return y; }
  
}

//   mods of any GSL routines shared by engines


int
gsl_linalg_partial_QR_decomp (gsl_matrix * A, gsl_vector * tau, size_t start)
{
  const size_t M = A->size1;
  const size_t N = A->size2;
  if (tau->size != min_size(M, N))
    GSL_ERROR ("size of tau must be MIN(M,N)", GSL_EBADLEN);
  else
  { size_t i;
    /* Apply prior transformation to the remaining columns and update the norms */
    for(i=0; i<start; ++i)
    { gsl_vector_view c_full (gsl_matrix_column (A, i));
      gsl_vector_view c      (gsl_vector_subvector (&(c_full.vector), i, M-i));    // rest of ith column
      gsl_matrix_view m      (gsl_matrix_submatrix (A, i, start, M - i, N-start)); // block from rest of matrix
      gsl_linalg_householder_hm (gsl_vector_get(tau,i), &(c.vector), &(m.matrix));
    }
    /* Convert the new columns into form */
    for (i = start; i < min_size(M, N); i++)
    {
      /* Compute the Householder transformation to reduce the j-th
      column of the matrix to a multiple of the j-th unit vector */
      gsl_vector_view c_full (gsl_matrix_column (A, i));
      gsl_vector_view c      (gsl_vector_subvector (&(c_full.vector), i, M-i));
      double tau_i = gsl_linalg_householder_transform (&(c.vector));
      gsl_vector_set (tau, i, tau_i);
      /* Apply the transformation to the remaining columns and update the norms */
      if (i + 1 < N)
      { gsl_matrix_view m = gsl_matrix_submatrix (A, i, i + 1, M - i, N - (i + 1));
        gsl_linalg_householder_hm (tau_i, &(c.vector), &(m.matrix));
      }
    }
    return GSL_SUCCESS;
  }    
}


//////////////////  GSL Regression State

void
gslRegressionState::initialize(gsl_vector const* beta, gslData const* data)
{
  mY    = copy_vector(data->y(),mN);
  mRes  = copy_vector(data->e(),mN);
  mWts  = copy_vector(data->w(),mN);
  mBeta = (mQ == 0) ? 0 : copy_vector(beta,     mQ);
}

gsl_vector* 
gslRegressionState::copy_vector (gsl_vector const* v, int n)
{
  gsl_vector *copy (gsl_vector_alloc(n));
  gsl_vector_const_view vV (gsl_vector_const_subvector(v, 0, n));
  gsl_vector_memcpy(copy, &vV.vector);
  return copy;
}

void 
gslRegressionState::free()
{
  std::cout << "GSLR: Freeing state object.\n";
  if (mY) gsl_vector_free (mY);
  if (mRes) gsl_vector_free (mRes);
  if (mWts) gsl_vector_free (mWts);
  if (mBeta) gsl_vector_free (mBeta);
};




