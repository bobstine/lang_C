/* 
 *  gsl_eigen.template.h
 *
 *  Created by Robert Stine on 2/4/08.
 *  Copyright 2008. All rights reserved.
 *
 */

#include "gsl_utils.h"
#include <gsl/gsl_eigen.h>
#include <gsl/gsl_blas.h>

/* use to test in place of sampling
void
gsl_matrix_pick_rows(gsl_matrix *dest, gsl_matrix const* src)
{
  for (int i=0;i<dest->size1;++i)
    gsl_vector_memcpy(&gsl_matrix_row(dest,i).vector, &gsl_matrix_const_row(src,i).vector);
}
*/

template<class Kernel>
gsl_matrix*
gslRKHS<Kernel>::operator()(gsl_matrix * srcData) const
{
  int n (srcData->size1);
  int p (srcData->size2);
  const int maxRows = 400;
  // standardize by shrinking toward mean
  if (mStandardize)
    gsl_matrix_standardize_columns(srcData);
  // sample data has nRows 
  int nRows   ((n < maxRows) ? n : maxRows);
  gsl_matrix *sampleData (gsl_matrix_alloc (nRows,p));
  gsl_matrix_sample_rows(sampleData, srcData);
  // compute the distance matrix, column at a time, filling lower triangular portion  
  gsl_matrix *dist (gsl_matrix_alloc(n,nRows));   // extra rows used below
  for (int j=0; j<nRows; ++j)
  { gsl_vector const* row_j = &gsl_matrix_const_row(sampleData,j).vector;
    for (int i=j; i<nRows; ++i)
      gsl_matrix_set(dist, i,j, Kernel()(row_j, &gsl_matrix_const_row(sampleData,i).vector));
  }
  // find the eigenvector decomposition, with evals sorted in diminishing order
  gsl_vector *eVals (gsl_vector_alloc(nRows));
  gsl_matrix *eVecs (gsl_matrix_alloc(nRows,nRows));
  gsl_eigen::principal_components(&gsl_matrix_submatrix(dist, 0, 0, nRows, nRows).matrix, eVals, eVecs);
  // expand the distance matrix, filling in all elements
  for (int i=0; i<n; ++i)
  { gsl_vector const* row_i = &gsl_matrix_const_row(srcData,i).vector;
    for (int j=0; j<nRows; ++j)
      gsl_matrix_set(dist, i,j, Kernel()(row_i, &gsl_matrix_const_row(sampleData,j).vector));
  }
  // count e-values larger than 1
  int nPC (mNumComponents);
  if(nPC == 0) 
    for(int i=0; i<(int)eVals->size; ++i)
      if (gsl_vector_get(eVals,i) > 1.0)
	++nPC;
      else break;
  nPC = nPC > 2 ? nPC : 3;  // at least 3
  // build the principal components in feature space
  gsl_matrix *pc (gsl_matrix_alloc(n,nPC));
  std::cout << "GSLE: SD of RKHS basis elements are ";
  for (int j=0; j < nPC; ++j)
  { gsl_vector* coefj (&gsl_matrix_const_column(eVecs,j).vector);
    gsl_vector* pcj   (&(gsl_matrix_column(pc,j).vector));
    gsl_blas_dgemv(CblasNoTrans, 1.0, dist, coefj, 0.0, pcj);
    std::cout << gsl_vector_standard_deviation(pcj) << " ";
  }
  std::cout << std::endl;
  gsl_matrix_free(eVecs);
  gsl_vector_free(eVals);
  gsl_matrix_free(dist);
  gsl_matrix_free(sampleData);
  return pc;
}
