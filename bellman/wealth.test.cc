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
    std::cout << "TEST: initial 20 universal rates (" << univ.identifier() << ")  ";
    for(int k=0; k<20; ++k) std::cout << univ(k) << " "; std::cout << std::endl;
    for(int k=0; k<count; ++k) total += univ(k);
    std::cout << "TEST: Total of universal(*,0.05) for " << count << " terms = " << total << std::endl;

    GeometricDist geo(0.005);
    total = 0.0;
    count = 10000;
    std::cout << "TEST: initial 20 geometric rates (" << geo.identifier() << ")  ";
    for(int k=0; k<20; ++k) std::cout << geo(k) << " "; std::cout << std::endl;
    for(int k=0; k<count; ++k) total += geo(k);
    std::cout << "TEST: Total of geometric for " << count << " terms = " << total << std::endl;

  } 

  // test bracket function from wealth 
  if (true)
  {
    double omega (0.05);
    int  nRounds (250);
    int    iZero ( nRounds + 1 ) ;
    int    steps ( iZero + 6 );  // need at least 6 above iZero.
    std::cout << "TEST: Initializing the wealth array." << std::endl;

    ProbDist *p;
    UniversalDist univ;
    GeometricDist geo(0.005);
    p = &univ;
    WealthArray uWealth(" Univ ", steps, omega, iZero, *p);
    p = &geo;
    WealthArray gWealth(" Geom ", steps, omega, iZero, *p);
    std::cout << "TEST: wealth array  \n" << uWealth << std::endl;
    std::cout << "TEST: wealth array  \n" << gWealth << std::endl;
    std::cout << "TEST:  bids are " ;
    
    for (int r=1; r <= nRounds; ++r) std::cout << gWealth.bid(r) << "  " << uWealth.bid(r) << "     ";
    std::cout << std::endl;
    
    { int i = 3;  // boundary
      double bid = uWealth.bid(i);
      std::pair<int,double>  kk (uWealth.new_wealth_position(i,0.05-bid));
      std::cout << "TEST:  increment W[" << i << "]= " << uWealth[i] << " by " << 0.05-bid << " to " << 0.05+uWealth[i]-bid
		<< " bracketed by " << uWealth[kk.first] << " * (" << kk.second << ")  +  ";
      if(kk.first < steps-1)
	std::cout << uWealth[kk.first+1] << " * (" << 1-kk.second << ")" << std::endl;
      else
	std::cout << uWealth[kk.first] << " * (" << 1-kk.second << ")" << std::endl;
    }
  }

  return 0;
}
