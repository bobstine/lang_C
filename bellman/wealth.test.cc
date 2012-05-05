#include "wealth.h"

#include <iostream>

#include <ctime>
#include <Eigen/Core>
 

int  main()
{
  
  // test the probability function from wealth.h
  if (true)
  {
    UniversalDist univ;
    double total (0.0);
    int count = 100000;
    for(int k=0; k<count; ++k) total += univ(k);
    std::cout << "TEST: Total of universal(*,0.05) for " << count << " terms = " << total << std::endl;

    GeometricDist geo(0.9);
    total = 0.0;
    count = 10000;
    std::cout << "TEST: initial 10 geometric rates (" << geo.identifier() << ")  ";
    for(int k=0; k<10; ++k) std::cout << geo(k) << " ";
    std::cout << std::endl;
    for(int k=0; k<count; ++k) total += geo(k);
    std::cout << "TEST: Total of geometric for " << count << " terms = " << total << std::endl;

  } 

  // test bracket function from wealth 
  if (true)
  {
    double omega (0.05);
    int  nRounds (5);
    int    iZero ( nRounds + 1 ) ;
    int    steps ( iZero + 6 );  // need at least 6 above iZero.
    std::cout << "TEST: Initializing the wealth array." << std::endl;

    ProbDist *p;
    UniversalDist univ;
    p = &univ;
    WealthArray wealth(" Test ", steps, omega, iZero, *p);
    std::cout << "TEST: wealth array  \n" << wealth << std::endl;
    std::cout << "TEST:  bids are " ;
    for (int r=1; r <= nRounds; ++r) std::cout << wealth.bid(r) << "  ";
    std::cout << std::endl;
    
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
