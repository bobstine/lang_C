// $Id: linpack.h,v 1.1.1.1 2004/04/26 21:12:28 bob Exp $

#ifndef _LINPACK_H_
#define _LINPACK_H_

namespace Linpack {

  typedef float Scalar;
  
  void   daxpy  (int n, Scalar *a, Scalar *x, Scalar *y);
  Scalar ddot   (int n, Scalar *x, Scalar *y);
  Scalar d_sign (Scalar *x, Scalar *y);
  Scalar pow_dd (Scalar *x, Scalar *y);

}
#endif

