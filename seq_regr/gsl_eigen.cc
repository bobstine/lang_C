/*
 *  gsl_eigen.cc
 *  seq_regr
 *
 *  Created by Robert Stine on 1/28/08.
 *  Copyright 2008. All rights reserved.
 *
 */

#include "gsl_eigen.h"
#include <gsl/gsl_vector.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_eigen.h>

#include <vector>
#include <string>
// sqrt, exp
#include <math.h>
#include <assert.h>

// debug
// #include <iostream>
// #include "gsl_utils.h"

void
gsl_eigen::principal_components (gsl_matrix* gram, gsl_vector* eVals, gsl_matrix* eVecs)
{
  const int nRows (gram->size1);
  assert (nRows == gram->size2);
  assert (nRows == eVals->size);
  gsl_eigen_symmv_workspace *scratch (gsl_eigen_symmv_alloc(nRows));
  gsl_eigen_symmv(gram, eVals, eVecs, scratch);
  gsl_eigen_symmv_sort(eVals, eVecs, GSL_EIGEN_SORT_VAL_DESC); 
  std::cout << "GSLE: Leading eigenvalues are ";
  gsl_vector_print_head (std::cout, eVals, 4);
  gsl_eigen_symmv_free(scratch);
}
  
gsl_vector*
gsl_eigen::construct_principal_component (gsl_matrix const* data, int col, gsl_matrix const* eVecs, gsl_vector const* pMeans, gsl_vector const* pSD)
{ 
  assert (pMeans->size == data->size2);
  
  const int   n           (data->size1);
  const int   p           (data->size2);
  const bool  standardize (pSD->size > 0);
  gsl_vector* coef        (&gsl_matrix_const_column(eVecs,col).vector);
  gsl_vector* pc          (gsl_vector_alloc(n));
  if (standardize)
  { 
    for (int i = 0; i<n; ++i)
    { double pci (0.0);
      for (int j=0; j<p; ++j)
        pci += gsl_vector_get(coef,j)*(gsl_matrix_get(data,i,j)-gsl_vector_get(pMeans, j))/gsl_vector_get(pSD,j);
      gsl_vector_set(pc,i,pci);
    }
  }
  else 
  {
    for (int i = 0; i<n; ++i)
    { double pci (0.0);
      for (int j=0; j<p; ++j)
        pci += gsl_vector_get(coef,j)*(gsl_matrix_get(data,i,j)-gsl_vector_get(pMeans, j));
      gsl_vector_set(pc,i,pci);
    }
  }
  return pc;
}
 





gslPrincipalComponents::result_type
gslPrincipalComponents::operator()(gsl_matrix const* data)   const 
{
  int n     (data->size1);
  int nCols (data->size2);
  gsl_matrix* covMat (gsl_matrix_alloc(nCols,nCols));
  gsl_vector* pMean  (gsl_vector_alloc(nCols));
  gsl_vector* pSD    (gsl_vector_alloc(mStandardize ? nCols:0));
  gsl_matrix_covariance_matrix(data, covMat, pMean, pSD);
  // find the eigenvector decomposition, with evals sorted in diminishing order
  gsl_vector *eVals (gsl_vector_alloc(nCols));
  gsl_matrix *eVecs (gsl_matrix_alloc(nCols,nCols));
  gsl_eigen::principal_components(covMat, eVals, eVecs);
  // build the principal components
  std::vector<gsl_vector*> pc;
  double evalBound (1.0);
  if (mNumComponents>0) evalBound = gsl_vector_get(eVals,mNumComponents);
  for (int j=0; gsl_vector_get(eVals,j)>evalBound; ++j)
    pc.push_back(gsl_eigen::construct_principal_component (data, j, eVecs, pMean, pSD));
  // free space
  gsl_matrix_free(eVecs);
  gsl_vector_free(pSD);
  gsl_vector_free(pMean);
  gsl_matrix_free(covMat);
  return std::make_pair(eVals,pc);                // somebody else needs to free these items
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
  for (int i=0; i<a->size; ++i)
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
  for (int i=0; i<a->size; ++i)
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
  for (int i=0; i<a->size; ++i)
  { diff = (*x++ - *y++) / *w++;
    total += diff * diff;
  }
  return exp(-total);
}
