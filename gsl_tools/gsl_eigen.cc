/*
 *  gsl_eigen.cc
 *
 *  Created by Robert Stine on 1/28/08.
 *  Copyright 2008. All rights reserved.
 *
 */

#include "gsl_eigen.h"
#include "debug.h"


#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_eigen.h>

#include <vector>
#include <string>

#include <iostream>
#include <math.h>
#include <assert.h>


unsigned int
max(unsigned int x, unsigned int y) { return (x>y)? x : y; }

unsigned int
min(unsigned int x, unsigned int y) { return (x<y)? x : y; }

void
gsl_eigen::principal_components (gsl_matrix* gram, gsl_vector* eVals, gsl_matrix* eVecs)
{
  const unsigned int nRows (gram->size1);
  assert (nRows == gram->size2);
  assert (nRows == eVals->size);
  debugging::debug("GSLE",2) << "Gram matrix is " << gram->size1 << "x" << gram->size2 << std::endl;
  gsl_eigen_symmv_workspace *scratch (gsl_eigen_symmv_alloc(nRows));
  gsl_eigen_symmv(gram, eVals, eVecs, scratch);
  gsl_eigen_symmv_sort(eVals, eVecs, GSL_EIGEN_SORT_VAL_DESC); 
  debugging::debug("GSLE",2) << "Leading eigenvalues are:\n";
  gsl_vector_print_head (debugging::debug("GSLE",2), eVals, min(10, eVals->size));
  gsl_eigen_symmv_free(scratch);
}


void
gsl_eigen::construct_principal_component (gsl_matrix const* data, int col, gsl_matrix const* eVecs, gsl_vector const* pMeans, gsl_vector const* pSD, gsl_matrix *pc)
{ 
  assert (pMeans->size == data->size2);
  
  const int   n           (data->size1);
  const int   p           (data->size2);
  const bool  standardize (pSD->size > 0);
  gsl_vector* coef        (&gsl_matrix_const_column(eVecs,col).vector);
  if (standardize)
  { 
    for (int i = 0; i<n; ++i)
    { double pci (0.0);
      for (int j=0; j<p; ++j)
        pci += gsl_vector_get(coef,j)*(gsl_matrix_get(data,i,j)-gsl_vector_get(pMeans, j))/gsl_vector_get(pSD,j);
      gsl_matrix_set(pc,i,col, pci);
    }
  }
  else 
  {
    for (int i = 0; i<n; ++i)
    { double pci (0.0);
      for (int j=0; j<p; ++j)
        pci += gsl_vector_get(coef,j)*(gsl_matrix_get(data,i,j)-gsl_vector_get(pMeans, j));
      gsl_matrix_set(pc,i,col,pci);
    }
  }
}



gslPrincipalComponents::result_type
gslPrincipalComponents::operator()(gsl_matrix const* data)   const 
{
  int nCols (data->size2);
  gsl_matrix* covMat (gsl_matrix_alloc(nCols,nCols));
  gsl_vector* pMean  (gsl_vector_alloc(nCols));
  gsl_vector* pSD    (gsl_vector_alloc(mStandardize ? nCols:0));
  gsl_matrix_covariance_matrix(data, covMat, pMean, pSD);
  // find the eigenvector decomposition, with evals sorted in diminishing order
  gsl_vector *eVals (gsl_vector_alloc(nCols));
  gsl_matrix *eVecs (gsl_matrix_alloc(nCols,nCols));
  gsl_eigen::principal_components(covMat, eVals, eVecs);
  // build the principal components; mNum 0 means to choose
  int nPC (mNumComponents);
  if (nPC==0)
  { double ei  (gsl_vector_get(eVals,0));
    if (!gsl_isnan(ei))
    { double ei1 (gsl_vector_get(eVals,1));                                                   // assume at least two
      while (nPC < (int)eVals->size-1 && !gsl_isnan(ei1) && (ei > 1.0) && (ei > 1.25*ei1))    // at least 25% more than next
      { ++nPC;
	ei = ei1;
	ei1 = gsl_vector_get(eVals,nPC+1);
      }
    }
  }
  gsl_matrix *pc (0);
  if (0 == nPC)
  { debugging::debug("GSLE", 0) << " *** Error ***  Found no principal components; returns a one-column array of zero.\n";
    pc = gsl_matrix_alloc(data->size1,1);
    for (int i=0; i<(int)data->size1; ++i)
      gsl_matrix_set(pc,i,0,  0.0);
  }
  else
  { pc = gsl_matrix_alloc(data->size1,nPC);
    for(int j=0; j<nPC; ++j)
      gsl_eigen::construct_principal_component (data, j, eVecs, pMean, pSD, pc);
  }
  // free space
  gsl_matrix_free(eVecs);
  gsl_vector_free(eVals);
  gsl_vector_free(pSD);
  gsl_vector_free(pMean);
  gsl_matrix_free(covMat);
  return pc;                     // somebody else needs to free this  ??? (done in adapter if anyone)
}

/* GSL documentation for these functions

— Function: int gsl_blas_dsyrk (CBLAS_UPLO_t Uplo, CBLAS_TRANSPOSE_t Trans, double alpha, const gsl_matrix * A, double beta, gsl_matrix * C)
These functions compute a rank-k update of the symmetric matrix C, C = \alpha A A^T + \beta C when 
Trans is CblasNoTrans and C = \alpha A^T A + \beta C when Trans is CblasTrans. 
Since the matrix C is symmetric only its upper half or lower half need to be stored. 
When Uplo is CblasUpper then the upper triangle and diagonal of C are used, and when Uplo is CblasLower 
then the lower triangle and diagonal of C are used.

— Function: gsl_eigen_symmv_workspace * gsl_eigen_symmv_alloc (const size_t n)
— Function: void gsl_eigen_symmv_free (gsl_eigen_symmv_workspace * w)
This function allocates a workspace for computing eigenvalues and eigenvectors of n-by-n real symmetric matrices. 
The size of the workspace is O(4n).

— Function: int gsl_eigen_symmv (gsl_matrix * A, gsl_vector * eval, gsl_matrix * evec, gsl_eigen_symmv_workspace * w)
This function computes the eigenvalues and eigenvectors of the real symmetric matrix A. 
Additional workspace of the appropriate size must be provided in w. The diagonal and lower triangular part of 
A are destroyed during the computation, but the strict upper triangular part is not referenced. 
The eigenvalues are stored in the vector eval and are unordered. The corresponding eigenvectors are stored 
in the columns of the matrix evec. For example, the eigenvector in the first column corresponds to the first
eigenvalue. The eigenvectors are guaranteed to be mutually orthogonal and normalised to unit magnitude.


— Function: int gsl_eigen_symmv_sort (gsl_vector * eval, gsl_matrix * evec, gsl_eigen_sort_t sort_type)
This function simultaneously sorts the eigenvalues stored in the vector eval and the corresponding real 
eigenvectors stored in the columns of the matrix evec into ascending or descending order according to the 
value of the parameter sort_type,

GSL_EIGEN_SORT_VAL_ASC
ascending order in numerical value 
GSL_EIGEN_SORT_VAL_DESC
descending order in numerical value 
GSL_EIGEN_SORT_ABS_ASC
ascending order in magnitude 
GSL_EIGEN_SORT_ABS_DESC
descending order in magnitude


Example
 
gsl_vector *eval = gsl_vector_alloc (4);
gsl_matrix *evec = gsl_matrix_alloc (4, 4);
gsl_eigen_symmv_workspace * w = gsl_eigen_symmv_alloc (4);
gsl_eigen_symmv (&m.matrix, eval, evec, w);
gsl_eigen_symmv_free (w);
gsl_eigen_symmv_sort (eval, evec, GSL_EIGEN_SORT_ABS_ASC);

*/


// RKHS    RKHS   RKHS   RKHS   RKHS   RKHS   RKHS   RKHS   RKHS   RKHS   RKHS   RKHS   RKHS   RKHS  
 
std::string RadialKernel::classname          ("Radial Kernel");
std::string WeightedRadialKernel::classname  ("Wtd Radial Kernel");
std::string QuadraticKernel::classname       ("Quadratic Kernel");
std::string L2Kernel::classname              ("L2 Kernel");


double
L2Kernel::operator()(gsl_vector const*a, gsl_vector const* b) const
{
  register double dp (0.0);
  double const* x (gsl_vector_const_ptr(a,0));                      // DANGER: better be laid out in row-major
  double const* y (gsl_vector_const_ptr(b,0));
  for (int i=0; i<(int)a->size; ++i)
    dp += *x++ * *y++;
  return dp;
}


double
QuadraticKernel::operator()(gsl_vector const*a, gsl_vector const* b) const
{
  double l2 (1.0 + L2Kernel()(a,b));
  return l2*l2;
}


double
RadialKernel::operator()(gsl_vector const*a, gsl_vector const* b) const
{
  register double diff;
  register double total (0.0);
  double const* x (gsl_vector_const_ptr(a,0));
  double const* y (gsl_vector_const_ptr(b,0));
  for (int i=0; i<(int)a->size; ++i)
  { diff = *x++ - *y++;
    total += diff * diff;
  }
  return exp(-total);
}


double
WeightedRadialKernel::operator()(gsl_vector const*a, gsl_vector const* b) const
{
  register double diff;
  register double total (0.0);
  double const* x (gsl_vector_const_ptr(a,0));
  double const* y (gsl_vector_const_ptr(b,0));
  double const* w (gsl_vector_const_ptr(mWts,0));
  for (int i=0; i<(int)a->size; ++i)
  { diff = (*x++ - *y++) / *w++;
    total += diff * diff;
  }
  return exp(-total);
}
