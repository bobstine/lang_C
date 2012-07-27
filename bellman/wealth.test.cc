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

    total = 0.0;
    count = 10000;
    GeometricDist geo(0.005);
    std::cout << "TEST: initial 20 geometric rates (" << geo.identifier() << ")  ";
    for(int k=0; k<20; ++k) std::cout << geo(k) << " "; std::cout << std::endl;
    for(int k=0; k<count; ++k) total += geo(k);
    std::cout << "TEST: Total of geometric for " << count << " terms = " << total << std::endl;

    total = 0.0;
    count = 100;
    UniformDist uni(count);
    std::cout << "TEST: initial 20 uniform rates (" << uni.identifier() << ")  ";
    for(int k=0; k<20; ++k) std::cout << uni(k) << " "; std::cout << std::endl;
    for(int k=0; k<count; ++k) total += uni(k);
    std::cout << "TEST: Total of uniform for " << count << " terms = " << total << std::endl;
  } 

  // test extremes in geometric wealth table for underflows
  if (true)
  {
    std::cout << "\n\n TEST wealth functions based on spending distributions." << std::endl;
    
    double omega ( 0.05 );
    int    iZero ( 500  );
    double psi(0.01);

    UniversalDist univ(univStart);
    GeometricDist geo(psi);
    UniformDist uni(1+iZero);  // add one to cover 0th position
 
    //    WealthArray gWealth(" Geom ", omega, iZero, geo );   // not numerically stable for long trials
    std::cout << "TEST: init universal wealths \n" ;  WealthArray uWealth(omega, iZero, univ);
    std::cout << "TEST: init geometric wealths \n" ;  WealthArray uniformWealth(omega, iZero, uni);
    std::cout << "TEST: init uniform   wealths \n" ;  WealthArray gWealth(omega, iZero, psi );                   // better geometric
    
    std::cout << "TEST: geometric name for psi=" << psi << " is " << gWealth.name() << std::endl;

    // wealth at 0 should be 1 step from zero for uniform
    std::cout << "TEST: wealth at  0 is (univ) " << uWealth[ 0]      << "   (geo) " << gWealth[ 0]      << "   (unif) " << uniformWealth[0]     << std::endl;
    std::cout << "TEST: wealth at  1 is        " << uWealth[ 1]      << "         " << gWealth[ 1]      << "          " << uniformWealth[1]     << std::endl;
    std::cout << "TEST: wealth at iZero is     " << uWealth[ iZero ] << "         " << gWealth[ iZero ] << "          " << uniformWealth[iZero] << std::endl;
    std::cout << "TEST: bid(0)                 " << uWealth.bid(0)   << "         " << gWealth.bid(0)   << "          " << uniformWealth.bid(0) << std::endl;

    std::cout << "TEST: Bid comparisons, geometric(0.01) and universal...\n";
    for (int j=1; j<10; ++j)
      std::cout << "[" << j << "]  " << gWealth.bid(iZero-j) << "   " << uWealth.bid(iZero-j) << "   " << uniformWealth.bid(iZero-j) << std::endl;
    
    std::cout << "TEST: universal wealth array  \n" << uWealth << std::endl;
    std::cout << "TEST: geometric wealth array  \n" << gWealth << std::endl;
    std::cout << "TEST:   uniform wealth array  \n" << uniformWealth << std::endl;
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
    WealthArray uWealth(omega, iZero, *p);
    p = &geo;
    WealthArray gWealth(omega, iZero, *p);
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
