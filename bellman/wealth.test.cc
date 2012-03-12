#include "wealth.h"

#include <iostream>

#include <ctime>
#include <Eigen/Core>
 

int  main()
{
  
  // test the probability function from wealth
  if (true)
  { 
    double total (0.0);
    int count = 100000;
    for(int k=0; k<count; ++k) total += universal(k);
    std::cout << "TEST: Total of beta(*,0.05) for " << count << " terms = " << total << std::endl;
  } 

  // test bracket function from wealth 
  if (true)
  {
    double omega (0.05);
    int    steps ( 16 );  // need at least 6 above iZero.
    int    iZero ( 10 );
    std::cout << "TEST: Initializing the wealth array." << std::endl;

    WealthArray wealth(" Test ", steps, omega, iZero, universal);
    std::cout << "TEST: wealth array  \n" << wealth << std::endl;

    { int i = 3;  // boundary
      double bid = wealth.bid(i);
      std::pair<int,double>  kk (wealth.new_wealth_position(i,0.05-bid));
      std::cout << "TEST:  increment W[" << i << "]= " << wealth[i] << " by " << 0.05-bid << " to " << 0.05+wealth[i]-bid
		<< " bracketed by " << wealth[kk.first] << " * (" << kk.second << ")  +  ";
      if(kk.first < steps-1)
	std::cout << wealth[kk.first+1] << " * (" << 1-kk.second << ")" << std::endl;
      else
	std::cout << wealth[kk.first] << " * (" << 1-kk.second << ")" << std::endl;
    }
  }

  return 0;
}
