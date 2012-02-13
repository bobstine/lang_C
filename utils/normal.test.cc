// $Id: normal.test.cc,v 1.4 2007/11/09 22:20:53 bob Exp $


#include "normal.h"

#include <math.h>
#include <iostream>


int main()
{
  std::cout 
			<< "  erf(1.0)  = " << erf(1.0)
	    << "  erfc(1.0) = " << erfc(1.0)
	    << std::endl;

  std::cout 
			<< "  phi(1.0)  = " << normal_density(1.0)
	    << "  phi(-1.0) = " << normal_density(-1.0)
	    << std::endl
	    << "  Phi(1.0)  = " << normal_cdf(1.0)
	    << "  Phi(3.0)  = " << normal_cdf(3.0)
	    << "  Phi(-2)   = " << normal_cdf(-2.0)
	    << "  Phi(-1.96)= " << normal_cdf(-1.96)
	    << std::endl;

  std::cout << "  Phi-1(0.5) = "       << normal_quantile(0.5)
	    << "  Phi-1(0.01) = "      << normal_quantile(0.01)
	    << "  Phi-1(0.0001) = "    << normal_quantile(0.0001)
	    << "  Phi-1(0.9999999) = " << normal_quantile(0.9999999)
	    << std::endl;

  { double p = 1.0;
    for (int i=0; i < 1000; ++i)
    { p *= .25;
      std::cout << i << "      p = " << p << "   Q(p) = " << normal_quantile(p) << std::endl;
    }
  }
  
  std::cout 
  << "  erf(1.0)  = " << erf(1.0)
  << "  erfc(1.0) = " << erfc(1.0)
  << std::endl;
  
  std::cout 
  << "  phi(1.0)  = " << normal_density(1.0)
  << "  phi(-1.0) = " << normal_density(-1.0)
  << std::endl
  << "  Phi(1.0)  = " << normal_cdf(1.0)
  << "  Phi(3.0)  = " << normal_cdf(3.0)
  << "  Phi(-2)   = " << normal_cdf(-2.0)
  << "  Phi(-1.96)= " << normal_cdf(-1.96)
  << std::endl;
  
  std::cout << "  Phi-1(0.5) = "       << normal_quantile(0.5)
  << "  Phi-1(0.01) = "      << normal_quantile(0.01)
  << "  Phi-1(0.0001) = "    << normal_quantile(0.0001)
  << "  Phi-1(0.9999999) = " << normal_quantile(0.9999999)
  << std::endl << std::endl;


  std::cout 
  << "  psi(1.0)  = " << cauchy_density(1.0)
  << "  psi(-0.5) = " << cauchy_density(-0.5)
  << std::endl
  
  << "  Psi(1.0)  = " << cauchy_cdf(1.0)
  << "  Psi(3.0)  = " << cauchy_cdf(3.0)
  << "  Psi(-2)   = " << cauchy_cdf(-2.0)
  << "  Psi(-1.96)= " << cauchy_cdf(-1.96)
  << std::endl;
  
  std::cout 
  << "  Phi-1(0.5) = "       << cauchy_quantile(0.5)
  << "  Psi-1(0.01) = "      << cauchy_quantile(0.01)
  << "  Psi-1(0.0001) = "    << cauchy_quantile(0.0001)
  << "  Psi-1(0.9999999) = " << cauchy_quantile(0.9999999)
  << std::endl;
  
}
    
