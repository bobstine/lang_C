// $Id: svd_sample.cc,v 3.0 2004/11/19 18:58:36 foster Exp $

#include <gsl/gsl_matrix.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_eigen.h>
#include <vector>
#include <math.h>
#include <iostream>
#include <assert.h>

// printing routines
std::ostream& operator<<(std::ostream& o, const gsl_matrix& m);
std::ostream& operator<<(std::ostream& o, const gsl_vector& m);

gsl_matrix*
load_data()
{
  int dim1 = 30;
  int dim2 = 2;
  gsl_matrix* p_result = gsl_matrix_alloc(dim1,dim2);  
  for(int i = 0; i < dim1; ++i)
    {
      gsl_matrix_set(p_result,i,0, (i % 5) - 2.0);
      gsl_matrix_set(p_result,i,1, (i % 3) - 1.0);
    }
  return p_result;
}

gsl_matrix*
load_covariance()
{
  gsl_matrix* p_data = load_data();
  
  int n = p_data->size1;
  
  gsl_matrix* p_result = gsl_matrix_alloc(n,n);

  // gsl_blas_sgemm(opt_a,opt_b,alpha, A,B, beta, C) is interpreted as:
  //
  //       C = alpha * opt_a(A) * opt_b(B) + beta * C
  //
  //       opt_a = CblasNoTrans
  //       opt_b = CblasTrans
  //       alpha = 1
  //       A = data
  //       B = data
  //       beta = 0
  //       C = result
  //
  // p_result = 1 * data * data' + 0 * p_result
  
  gsl_blas_dgemm(CblasNoTrans, CblasTrans, 1.0, p_data, p_data, 0.0, p_result);
  return p_result;
}

gsl_matrix*
rbf_covariance(gsl_matrix* p_left,gsl_matrix* p_right)
{
  assert(p_left->size2 == p_right->size2);
  int n_left = p_left->size1;
  int n_right = p_right->size1;
  
  gsl_matrix* p_result = gsl_matrix_alloc(n_left,n_right);

  for(int i = 0; i < n_left; ++i)
    for(int j = 0; j < n_right; ++j)
      {
	double ss = 0;
	for(int k = 0; k < p_left->size2; ++k)
	  {
	    double diff = gsl_matrix_get(p_left,i,k) - gsl_matrix_get(p_right,j,k);
	    ss += diff*diff;
	  }
	double value = exp(- .1 * ss);
	gsl_matrix_set(p_result,i,j, value);
      }
  return p_result;
}

gsl_matrix*
sample_rows(gsl_matrix* p_data)
{
  int n = p_data->size1;
  int n_target = 10;
  
  gsl_matrix* p_result = gsl_matrix_alloc(n_target,p_data->size2);

  int slots_left = n_target;
  for(int i = 0; i < n; ++i)
    {
      if(double(rand())/RAND_MAX < slots_left / (n - i))
	{
	  gsl_vector_memcpy(&gsl_matrix_row(p_result,n_target - slots_left).vector,
			    &gsl_matrix_row(p_data,i).vector);
	  --slots_left;
	}
    }
  return p_result;
}

int
main()
{

  { // SAMPLING VERSION (experimental)
    std::cout << "\n\n* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * \n\n" << std::endl;
    gsl_matrix* p_data = load_data();
    gsl_matrix* p_sampled = sample_rows(p_data);
    std::cout << *p_sampled << "\n\n";
    gsl_matrix* p_input_matrix = rbf_covariance(p_sampled,p_sampled);
    int dim2 = p_input_matrix -> size2;

    gsl_eigen_symm_workspace * ws = gsl_eigen_symm_alloc (dim2);

    gsl_matrix* p_EVEC = gsl_matrix_alloc(dim2,dim2);
    gsl_vector* p_EVAL = gsl_vector_alloc(dim2);  // the diagonal "matrix"
  
    gsl_eigen_symmv_workspace* p_WORK = gsl_eigen_symmv_alloc(dim2);  // tempary work space

    int error_code =  gsl_eigen_symmv(p_input_matrix, p_EVAL,p_EVEC,p_WORK);
    if(error_code != 0)
      std::cout << "error code: " << error_code << std::endl;

    error_code = gsl_eigen_symmv_sort (p_EVAL, p_EVEC, GSL_EIGEN_SORT_VAL_DESC);

    std::cout << *p_EVAL << std::endl;

    //    std::cout << gsl_matrix_submatrix(p_EVEC,0,1,dim2,3).matrix << std::endl;

    {
      std::cout << "Getting ALL of the sampled eigen vectors printed at once." << std::endl;
      gsl_matrix* p_result = gsl_matrix_alloc(p_data->size1,p_sampled->size1);
      gsl_matrix* p_full_covariance = rbf_covariance(p_data,p_sampled);
      int error = gsl_blas_dgemm (CblasNoTrans,CblasNoTrans, 1.0, p_full_covariance, p_EVEC, 0, p_result);
      std::cout << gsl_matrix_submatrix(p_result,0,0,p_result->size1,3).matrix << std::endl;
    }
    {
      std::cout << "Getting one sampled eigen vectors at a time." << std::endl;
      gsl_vector* p_result = gsl_vector_alloc(p_data->size1);
      gsl_matrix* p_full_covariance = rbf_covariance(p_data,p_sampled);
      gsl_blas_dgemv (CblasNoTrans, 1.0, p_full_covariance, &gsl_matrix_column(p_EVEC,1).vector, 0, p_result);
      std::cout << *p_result << std::endl;
      gsl_blas_dgemv (CblasNoTrans, 1.0, p_full_covariance, &gsl_matrix_column(p_EVEC,2).vector, 0, p_result);
      std::cout << *p_result << std::endl;
    }

  }
  { // METHOD CURRENTLY USED IN PRODUCTION CODE
    std::cout << "\n\n* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * \n\n" << std::endl;
    gsl_matrix* p_data = load_data();
    std::cout << *p_data << "\n\n";
    gsl_matrix* p_input_matrix = rbf_covariance(p_data,p_data);
    int dim2 = p_input_matrix -> size2;

    gsl_matrix U(*p_input_matrix);

    gsl_eigen_symm_workspace * ws = gsl_eigen_symm_alloc (dim2);

    gsl_matrix* p_EVEC = gsl_matrix_alloc(dim2,dim2);
    gsl_vector* p_EVAL = gsl_vector_alloc(dim2);  // the diagonal "matrix"
  
    gsl_eigen_symmv_workspace* p_WORK = gsl_eigen_symmv_alloc(dim2);  // tempary work space

    int error_code =  gsl_eigen_symmv(&U, p_EVAL,p_EVEC,p_WORK);
    if(error_code != 0)
      std::cout << "error code: " << error_code << std::endl;

    error_code = gsl_eigen_symmv_sort (p_EVAL, p_EVEC, GSL_EIGEN_SORT_VAL_DESC);

    std::cout << *p_EVAL << std::endl;
    std::cout << gsl_matrix_submatrix(p_EVEC,0,1,dim2,3).matrix << std::endl;

    //  std::cout << *p_V << std::endl;
  }

  { // GOLD STANDARD SOLUTION
    std::cout << "\n\n* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * \n\n" << std::endl;
    gsl_matrix* p_data = load_data();
    std::cout << *p_data << "\n\n";
    gsl_matrix* p_input_matrix = rbf_covariance(p_data,p_data);
    int dim2 = p_input_matrix -> size2;

    std::cout << "Input matrix is: \n\n" << *p_input_matrix << "\n\n";
    gsl_matrix U(*p_input_matrix);
    gsl_matrix* p_V = gsl_matrix_alloc(dim2,dim2);
    gsl_vector* p_S = gsl_vector_alloc(dim2);  // the diagonal "matrix"
  
    gsl_vector* p_WORK = gsl_vector_alloc(dim2);  // tempary work space
    int error_code = gsl_linalg_SV_decomp (&U,p_V,p_S,p_WORK);
    if(error_code != 0)
      std::cout << "error code: " << error_code << std::endl;

    std::cout << *p_S;
    std::cout << gsl_matrix_submatrix(&U,0,0,dim2,5).matrix << std::endl;
    std::cout << gsl_matrix_submatrix(p_V,0,0,dim2,5).matrix << std::endl;
    //  std::cout << *p_V << std::endl;
  }
};


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

double rounding(double mu,double s, double scaling);
double round_to_4_digits(double mu);

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

double
rounding(double mu,double s)
{
  double scaling = 1;

  if(fabs(mu) < 1e-9)
    return 0.0;

  if(mu == 0)
    return mu;

  if(s < 0)
    s = - s;
  
  double log10 = log(s)/log(10);

  log10 = floor(log10);
  double d = exp(log10 * log(10));
   s = (rint(s*scaling/d))*(d/scaling);
   mu = (rint(mu*scaling/d))*(d/scaling);
   // the following two lines remove -0's from outputs
   if(mu == 0)
     mu = 0;
  if(s == 0)
    s = 0;
  return mu;
};

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

double
round_to_4_digits(double mu)
{
  return rounding(mu,mu/10.);
}
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

std::ostream&
operator<<(std::ostream& o, const gsl_matrix& m)
{
  for(unsigned int i = 0; i < m.size1; ++i)
    {
      o << "\t";
      for(unsigned int j = 0; j < m.size2; ++j)
	o << round_to_4_digits(gsl_matrix_get(&m,i,j)) <<  "\t";
      o << std::endl;
    };
  return o;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */


std::ostream&
operator<<(std::ostream& o, const gsl_vector& m)
{
  o << "\t[ ";
  for(unsigned int i = 0; i < m.size; ++i)
    {
      o << round_to_4_digits(gsl_vector_get(&m,i));
      if(i != m.size-1)
	o <<  ", ";
    };
  o << " ]" << std::endl;
  return o;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

