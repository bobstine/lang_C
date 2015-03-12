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

void
Linpack::daxpy  (int n, Linpack::Scalar *a, Linpack::Scalar *x, Linpack::Scalar *y)
{
	int i;
	
	for (i=0; i<n; ++i)
		y[i] += (*a) * x[i];
}


/*	was fixed double precision function ddot(n,dx,incx,dy,incy)	*/
/*		dot product of x and y							*/
Linpack::Scalar
Linpack::ddot(int n, Linpack::Scalar *x, Linpack::Scalar *y)
{
	int i;
	Linpack::Scalar result = 0.0;
	
	for (i=0; i<n; ++i)
		result += x[i] * y[i];
	return(result);
}

/*  Returns x having the same sign as y */
Linpack::Scalar
Linpack::d_sign(Linpack::Scalar *x, Linpack::Scalar *y)
{
  if (*y >= 0)
    return (*x >= 0) ? (*x) :  (-*x);
  else
    return (*x <  0) ? (*x) : (-*x);
}

Linpack::Scalar
Linpack::pow_dd(Linpack::Scalar *x, Linpack::Scalar *y)
{
  return( (Scalar) pow(*x,*y));
}
