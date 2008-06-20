// $Id: linpack.test.cc,v 1.1.1.1 2004/04/26 21:12:28 bob Exp $

#include "linpack.h"

#include <iostream>

int
main ()
{
  double a = 1.0, b = 2.0, c = -2.0;
  double x[5] = {1.0, 1.0, 1.0, 1.0, 1.0};
  double y[5] = {1.0, 2.0, 3.0, 4.0, 5.0};
  
  daxpy(5, &b, x, y);
  std::cout << "TEST: y -> " << y[0] << " " << y[1] << " " << y[4] << std::endl;
  
  std::cout << "TEST: ddot is " << ddot (5,x,y) << std::endl;
  
  std::cout << "d_sign(a,b) = " << d_sign(&a,&b) << std::endl;
  std::cout << "d_sign(a,c) = " << d_sign(&a,&c) << std::endl;

  return 0;
}
	
