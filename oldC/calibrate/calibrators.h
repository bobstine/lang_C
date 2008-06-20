// $Id: calibrators.h,v 1.2 2004/08/06 17:57:54 bob Exp $

#ifndef _CALIBRATORS_H_
#define _CALIBRATORS_H_

/*
  Any object that is created with a message of the form

        calibrator(int df, Iterator y, Iterator yHat, int n)

  and is a function object that returns a value for any input on [0,1]
  is a calibrator.

   5 Aug 04 ... Created after problems with fitting splines to large data sets.
*/

#include <iostream>
#include <gsl/gsl_linalg.h>


template<class Iter1, class Iter2>
class BiweightCalibrator : public std::unary_function<double,double>
{
  int            mN;      // number of input values
  int            mDF;     // number of df in fit
  gsl_vector    *mBeta;
  
 public:

  ~BiweightCalibrator() { if (mBeta) gsl_vector_free(mBeta); }

  BiweightCalibrator (BiweightCalibrator const& bc)
    : mN(bc.mN), mDF(bc.mDF), mBeta(0)
    {
      mBeta = gsl_vector_alloc(mDF+1);
      gsl_vector_memcpy(mBeta, bc.mBeta);
    }
     
  BiweightCalibrator (int df, Iter1 x, Iter2 y, int n)
    : mN(n), mDF(df), mBeta(0)
    {
      // std::cout << "BCAL: Copying.\n";
      mBeta = gsl_vector_alloc(mDF+1);
      compute_coefs(x,y);
    }

  double
    operator()(double x) const
    { double z    (2*x-1.0);
      double temp (1.0 - z*z);
      double zj   (temp*temp);  // (1-z^2)^2
      double fit  (mBeta->data[0]);
      for (int j=1; j <= mDF; ++j)
      {	fit += gsl_vector_get(mBeta,j) * zj;  // leaves coefs in mXY
	zj *= 2.0 * z;
      }
      return fit;
    }

  void print_to (std::ostream& os) const
    { os << "Biweight calibrator with df=" << mDF << " and coefs ";
      for (int j=0; j<mDF+1; ++j) os << gsl_vector_get(mBeta,j) << " ";
    }
  
 private:

  void solve_system(gsl_vector *beta, gsl_matrix *xx)
    {
      int problem (gsl_linalg_cholesky_decomp(xx));
      if (problem)
      { std::cout << "BCAL: Cannot compute cholesky factor.\n";
	return;
      }
      problem = gsl_linalg_cholesky_svx(xx, beta);  // inplace calc
      if (problem)
	std::cout << "BCAL: Cannot solve for coefs of biweight calibrator.\n";
    }
  
  void compute_coefs(Iter1 x, Iter2 y)
    {
      int dim (mDF+1);
      gsl_matrix *XX (gsl_matrix_calloc(dim,dim));  // init to zero for accumulate
      gsl_vector_set_zero(mBeta);
      double *xx = new double[dim];
      for (int i=0; i<mN; ++i)
      { register double z    (2 * (*x) - 1.0);      // scale to (-1,1)
	double          temp (1.0 - z * z);
	xx[0] = 1.0;
	xx[1] = temp*temp;                          // (1-z^2)^2
	for (int j=2; j<dim; ++j)
	  xx[j] = 2.0 * z * xx[j-1];
	for(int j=0; j<dim; ++j)
	{ // gsl_vector_set(mXY, j, xx[j] * *y + gsl_vector_get(mXY,j));
	  mBeta->data[j*mBeta->stride] += xx[j] * *y;
	  // xx diagonal
	  XX->data[j * XX->tda + j] += xx[j] * xx[j];
	  for(int k=j+1; k<dim; ++k)
	  { double cp (xx[j] * xx[k]);
	    // gsl_matrix_set(mXX,j,k,cp + gsl_matrix_get(mXX,j,k));
	    XX->data[j * XX->tda + k] += cp;
	    // gsl_matrix_set(mXX,k,j,gsl_matrix_get(mXX,j,k));
	    XX->data[k * XX->tda + j] += cp;
	  }
	}
	++y;
	++x;
      }
      solve_system(mBeta, XX);
      if(xx) delete xx;
      if(XX) gsl_matrix_free(XX);
    }
  /* Use this code to print arrays
      for (int j=0; j<mDF+1; ++j)
      {
	std::cout << gsl_vector_get(mBeta,j) << "   -->     ";
	for (int k=0; k<mDF+1; ++k)
	  std::cout << gsl_matrix_get(XX,j,k) << "  ";
	std::cout << std::endl;
      }
  */
};


template<class I1, class I2>
std::ostream&
operator<<(std::ostream& os, BiweightCalibrator<I1,I2> const& bc)
{
  bc.print_to(os);
  return os;
}

#endif
