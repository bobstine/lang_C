/* $Id: gsl_utils.cc,v 1.11 2008/03/05 22:37:32 bob Exp $ */

#include "gsl_utils.h"
#include "gsl_iterator.h"

// squared deviation
#include "function_utils.h"

#include "range_stats.h"

#include <gsl/gsl_blas.h>

#include <math.h>
#include <assert.h>
#include <iomanip>
#include <fstream>


std::ostream&
operator<<(std::ostream& os, gsl_vector const* v)
{
  os << "[" << v->size << "] ";
  for(size_t i=0; i<v->size; ++i)
    os << std::setw(6) << gsl_vector_get(v,i) << " " ;
  os << std::endl;
  os.flush();
  return os;
}

std::ostream&
operator<<(std::ostream& os, gsl_matrix const* m)
{
  os << "[" << m->size1 << "," << m->size2 << "]: " << std::endl;
  for (size_t i=0; i<m->size1; ++i)
  { 
    for (size_t j=0; j<m->size2; ++j)
      os << std::setw(6) << gsl_matrix_get(m,i,j) << " ";
    os << std::endl;
  }
  os.flush();
  return os;
}

void
gsl_vector_print_head (std::ostream& os, gsl_vector const* v, int n)
{
  const int nPrint ((n<(int)v->size) ? n : v->size);
  for (int i=0; i < nPrint; ++i)
  { os << gsl_vector_get(v,i);
    if (nPrint-i>1) os << ", " ;
  }
  os << std::endl;
}


void
gsl_matrix_write_to_file (char *fileName, gsl_matrix const* m)
{
  const int n (m->size1);
  const int p (m->size2);
  std::ofstream output (fileName);
  for (int i=0; i<n; ++i)
  { for (int j=0; j<p; ++j)
    output << gsl_matrix_get(m,i,j) << " ";
    output << std::endl;
  }
  output.close();
}




/////////  Sample

void
gsl_matrix_sample_rows(gsl_matrix *dest, gsl_matrix const* src)
{
  assert(dest->size2 == src->size2); // same number of columns
  int destRows  (dest->size1);
  int slotsLeft (destRows);
  int rowsLeft  (src->size1);
  while (rowsLeft && slotsLeft)
  {
    if ( ((double)(rand())/RAND_MAX) <  (((double)slotsLeft) / rowsLeft)  )
    {
      gsl_vector_memcpy(&gsl_matrix_row(dest,destRows - slotsLeft).vector,
                        &gsl_matrix_const_row(src ,rowsLeft-1).vector);
      --slotsLeft;
    }
    --rowsLeft;
  }
}


/////////  Stats


double
gsl_vector_mean (gsl_vector const* v)
{
  return range_stats::average(make_range(begin(v), end(v)), v->size);
}


void
gsl_vector_center_vector (gsl_vector * v)
{
  gsl_vector_add_constant(v,-gsl_vector_mean(v));
}


double
gsl_vector_standard_deviation (gsl_vector const* v, double mean)
{
  return range_stats::standard_deviation(make_range(begin(v), end(v)), mean, v->size-1);
}


double
gsl_vector_standard_deviation (gsl_vector const* v)
{
  return gsl_vector_standard_deviation(v,gsl_vector_mean(v)); 
}



void 
gsl_vector_standardize_vector (gsl_vector *v)
{
  double m (gsl_vector_mean(v));
  double s (gsl_vector_standard_deviation(v,m));
  assert (s > 0);
  gsl_vector_add_constant(v,-m);
  gsl_vector_scale(v,1/s);
}

void
gsl_matrix_standardize_columns (gsl_matrix *m)
{
  for (int j=0; j<(int)m->size2; ++j)
    gsl_vector_standardize_vector(&gsl_matrix_column(m,j).vector);
}

void
gsl_matrix_covariance_matrix (gsl_matrix const* data, gsl_matrix *covMat, gsl_vector* pM, gsl_vector* pSD)
{
  const int n (data->size1);
  const int p (data->size2);
  for (int j=0; j<p; ++j)
    gsl_vector_set(pM, j, gsl_vector_mean(&gsl_matrix_const_column(data,j).vector));
  // only fill diagonal and lower triagular portion
  for (int k=0; k<p; ++k)
  { gsl_vector const* pXk (&gsl_matrix_const_column(data,k).vector);
    double Mk (gsl_vector_get (pM, k));
    for (int j=0; j<=k; ++j)
    { gsl_vector const* pXj (&gsl_matrix_const_column(data,j).vector);
      double Mj (gsl_vector_get(pM,j));
      double dot (0.0);
      for (int i=0; i<n; ++i)
        dot += (gsl_vector_get(pXk,i)-Mk)*(gsl_vector_get(pXj,i)-Mj);
      gsl_matrix_set(covMat,k,j,dot/(n-1));
    }
  }
  if (pSD->size > 0)  // standardize by dividing by sd
  { 
    for (int k=0; k<p; ++k) 
    { double sigma = sqrt(gsl_matrix_get(covMat,k,k));
      gsl_vector_set (pSD,k,sigma);
      gsl_matrix_set(covMat,k,k,1.0);
      for(int j=0; j<k; ++j)
        gsl_matrix_set(covMat,k,j, gsl_matrix_get(covMat,k,j)/sigma);
      for(int j=k+1; j<p; ++j)
        gsl_matrix_set(covMat,j,k, gsl_matrix_get(covMat,j,k)/sigma); 
    }
  }
}
  

/////////  Matrix operations, norm

void
gsl_matrix_inverse(gsl_matrix const* M, gsl_matrix *Mi)
{
  gsl_matrix *chol (gsl_matrix_alloc(M->size1, M->size1));
  gsl_matrix_memcpy(chol, M);
  gsl_linalg_cholesky_decomp(chol);
  gsl_matrix_set_identity(Mi);
  for(int j=0; j<(int)M->size1; ++j) 
    gsl_linalg_cholesky_svx(chol, &gsl_matrix_row(Mi,j).vector);
  gsl_matrix_free(chol);
}

double
gsl_weighted_l2_norm_squared (gsl_vector const* x, gsl_matrix const* M) // x'Mx 
{
  gsl_vector *temp (gsl_vector_alloc(x->size));
  gsl_blas_dsymv(CblasLower, 1.0, M, x, 0.0, temp);
  double norm2 (0.0);
  gsl_blas_ddot(temp, x, &norm2);
  gsl_vector_free(temp);
  return (norm2);
}

double
gsl_weighted_l2_dist_squared (gsl_vector const* x, gsl_vector const* y, gsl_matrix const* M)// (x-y)'M(x-y)
{
  gsl_vector *diff (gsl_vector_alloc(x->size));
  gsl_vector_memcpy(diff, x);
  gsl_vector_sub(diff,y);
  double dist (gsl_weighted_l2_norm_squared(diff, M));
  gsl_vector_free(diff);
  return dist;
}


