#include "wealth.h"

#include <iostream>

#include <ctime>
#include <Eigen/Core>
 

int  main()
{

  const int univStart (1);
  
  // test the probability function from wealth.h
  if (true)
  {
    UniversalDist univ(univStart);
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

  // test extremes in geometric wealth table for underflows
  if (true)
  {
    double psi(0.01);
    UniversalDist univ(univStart);
    GeometricDist geo(psi);
    
    double omega ( 0.05 );
    int    iZero ( 500  ) ;
 
    //    WealthArray gWealth(" Geom ", omega, iZero, geo );   // not numerically stable for long trials
    WealthArray uWealth(" Univ ", omega, iZero, univ);
    WealthArray gWealth(omega, iZero, psi );                   // better geometric
    std::cout << "TEST: geometric name for psi=" << psi << " is " << gWealth.name() << std::endl;

    std::cout << "TEST: wealth at  0 is " << uWealth[ 0] << " " << gWealth[ 0] << std::endl << std::endl;
    std::cout << "TEST: wealth at  1 is " << uWealth[ 1] << " " << gWealth[ 1] << std::endl << std::endl;
    std::cout << "TEST: wealth at omega is " << uWealth[ iZero ] << " " << gWealth[ iZero ] << std::endl << std::endl;
    std::cout << "TEST: Low bids " << uWealth.bid(0) << " " << gWealth.bid(0) << std::endl << std::endl;

    std::cout << "TEST: Bid comparisons, geometric(0.01) and universal...\n";
    for (int j=1; j<10; ++j)
      std::cout << "[" << j << "]  " << gWealth.bid(iZero-j) << "   " << uWealth.bid(iZero-j) << std::endl;
    
    std::cout << "TEST: wealth array  \n" << uWealth << std::endl;
    std::cout << "TEST: wealth array  \n" << gWealth << std::endl;
  } 


  // test bracket function from wealth 
  if (false)
  {
    double omega (0.05);
    int  nRounds (250);
    int    iZero ( nRounds + 1 ) ;
    int    steps ( iZero + 6 );  // need at least 6 above iZero.
    std::cout << "TEST: Initializing the wealth array." << std::endl;

    ProbDist *p;
    UniversalDist univ(univStart);
    GeometricDist geo(0.005);
    p = &univ;
    WealthArray uWealth(" Univ ", omega, iZero, *p);
    p = &geo;
    WealthArray gWealth(" Geom ", omega, iZero, *p);
    std::cout << "TEST: wealth array  \n" << uWealth << std::endl;
    std::cout << "TEST: wealth array  \n" << gWealth << std::endl;
    std::cout << "TEST:  bids are " ;
    
    for (int r=1; r <= nRounds; ++r) std::cout << gWealth.bid(r) << "  " << uWealth.bid(r) << "     ";
    std::cout << std::endl;
    
    { int i = 3;  // boundary
      double bid = uWealth.bid(i);
      std::pair<int,double>  kk (uWealth.wealth_position(i));
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
