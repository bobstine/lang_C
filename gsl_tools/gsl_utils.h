// $Id: gsl_utils.h,v 1.9 2008/03/05 22:37:32 bob Exp $
#ifndef _GSL_UTILS_H_
#define _GSL_UTILS_H_
 
///

#include "range_ops.h"
#include "gsl_iterator.h"

#include <gsl/gsl_matrix.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_linalg.h>

#include <assert.h>

// make_pair
#include <utility>
#include <iostream>

/////////////////////  Printing a gsl vector or matrix  //////////////////////

std::ostream&
operator<<(std::ostream& os, gsl_vector const* v);

std::ostream&
operator<<(std::ostream& os, gsl_matrix const* m);

void
gsl_vector_print_head (std::ostream& os, gsl_vector const* v, int nPrint);

void
gsl_matrix_write_to_file (char* fileName, gsl_matrix const* m);


////////////////  Copy a collection of ranges into a matrix or vector ///////////

template<class Collection>
std::pair<int,int>
gsl_matrix_fill_from_ranges (Collection c, gsl_matrix *matrix)
{
  int j = 0;  
  for(typename Collection::const_iterator it = Ranges::begin(c); it != Ranges::end(c); ++it)
  { // item in collection must possess range method
    if ((int)matrix->size2 == j) return std::pair<int,int>(0,j); 
    range_ops::copy(it->range(), GSL::begin(&gsl_matrix_column(matrix,j).vector));
    ++j;
  }
  return std::make_pair(matrix->size1, matrix->size2);
}


template<class Range>
int
gsl_vector_fill_from_range (Range r, gsl_vector *vector)
{
  range_ops::copy(r, GSL::begin(vector));
  return vector->size;
}

/////////////////////////////  Sample Rows  ////////////////////////////////

void
gsl_matrix_sample_rows(gsl_matrix *dest, gsl_matrix const* src);


///////////////////////////  Summary Statistics  ////////////////////////////

double
gsl_vector_mean (gsl_vector const* v);


double
gsl_vector_standard_deviation (gsl_vector const* v, double mean);

double
gsl_vector_standard_deviation (gsl_vector const* v);


void
gsl_vector_center_vector (gsl_vector *v);

void 
gsl_vector_standardize_vector (gsl_vector *v);

void
gsl_matrix_standardize_columns (gsl_matrix *m);


// computes correlations if pSD is allocated; finds means and SDs 
void
gsl_matrix_covariance_matrix (gsl_matrix const* data, gsl_matrix *covMat, gsl_vector *pMeans, gsl_vector *pSD); 


///////////////////// Matrix operations  ////////////////////////

inline
void
gsl_matrix_multiply_Ab(gsl_matrix const* A, gsl_vector const* v, gsl_vector *Av)
{
  gsl_blas_dgemv(CblasNoTrans, 1.0, A, v, 0.0, Av); 
}

inline
void
gsl_matrix_multiply_ABt(gsl_matrix const* A, gsl_matrix const* B, gsl_matrix *ABt) //  A B'
{
  gsl_blas_dgemm(CblasNoTrans, CblasTrans, 1.0, A, B, 0.0, ABt);   
}

inline
void
gsl_matrix_multiply_AtB(gsl_matrix const* A, gsl_matrix const* B, gsl_matrix *AtB) //  A' B
{
  gsl_blas_dgemm(CblasTrans, CblasNoTrans, 1.0, A, B, 0.0, AtB);   
}


inline
void
gsl_matrix_multiply_AS(gsl_matrix const* A, gsl_matrix const* S, gsl_matrix* AS)  // A S , with S symmetric
{
  gsl_blas_dsymm(CblasRight, CblasLower, 1.0, S, A, 0.0, AS);      
}

inline
void
gsl_matrix_multiply_SA(gsl_matrix const* S, gsl_matrix const* A, gsl_matrix* SA)  // S A , with S symmetric
{
  gsl_blas_dsymm(CblasLeft, CblasLower, 1.0, S, A, 0.0, SA);      
}


void
gsl_matrix_inverse(gsl_matrix const* M, gsl_matrix *Mi);

// Matrix is assumed to be lower triagular

double
gsl_weighted_l2_norm_squared (gsl_vector const* x, gsl_matrix const* M); // x'Mx 

double
gsl_weighted_l2_dist_squared (gsl_vector const* x, gsl_vector const* y, gsl_matrix const* M);  // (x-y)'M(x-y)

#endif
