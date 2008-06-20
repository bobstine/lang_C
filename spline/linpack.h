// $Id: linpack.h,v 1.1.1.1 2004/04/26 21:12:28 bob Exp $

#ifndef _LINPACK_H_
#define _LINPACK_H_

void   daxpy  (int n, double *a, double *x, double *y);
double ddot   (int n, double *x, double *y);
double d_sign (double *x, double *y);
double pow_dd (double *x, double *y);

#endif

