/*	Linpack.c								*/
/*		Fake the calls to linpack blas		*/
/*		routines used in some applications.	*/
/*		Linpack routines allow varying inc, */
/*		as for use in matrix routines.  I   */
/*		just use a fixed increment of 1.	*/

#include "linpack.h"
#include "math.h"

/*	subroutine daxpy(n,da,dx,incx,dy,incy)	*/
/*        returns ax+y back in y			*/

void   daxpy  (int n, double *a, double *x, double *y)
{
	int i;
	
	for (i=0; i<n; ++i)
		y[i] += (*a) * x[i];
}


/*	double precision function ddot(n,dx,incx,dy,incy)	*/
/*		dot product of x and y							*/
double ddot(int n, double *x, double *y)
{
	int i;
	double result = 0.0;
	
	for (i=0; i<n; ++i)
		result += x[i] * y[i];
	return(result);
}

/*  Returns x having the same sign as y */
double d_sign(double *x, double *y)
{
  if (*y >= 0)
    return (*x >= 0) ? (*x) :  (-*x);
  else
    return (*x <  0) ? (*x) : (-*x);
}

double pow_dd(double *x, double *y)
{
  return( (double) pow(*x,*y));
}
