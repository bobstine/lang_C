#include "line_search.Template.h"
#include "normal.h"
#include "bellman.h"

#include <functional>
#include <iostream>
#include <math.h>

#include <ctime>
#include <Eigen/Core>
 

int  main()
{
  // std::cout << "\n\nMAIN: Bellman recursion for competitive ratio of universal bidder. \n\n\n";
  
  // test the probability function
  if (false)
  { 
    double total (0.0);
    int count = 100000;
    for(int k=0; k<count; ++k) total += universal(k);
    std::cout << "MAIN: Total of beta(*,0.05) for " << count << " terms = " << total << std::endl;
  } 

  // test bracket search function
  if (false)
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

  // test utility objects 
  if (true)
  {
    double gamma (2.0 );
    double omega (0.05);
    int    steps ( 16 );  // need at least 6 above iZero.
    int    iZero ( 10 );
    std::cout << "TEST: Preparing to test the utility object." << std::endl;
    
    WealthArray wealth(" Test ", steps, omega, iZero, universal);
    
    RejectUtility rejectU (gamma, wealth);  // omega implicit in wealth
    std::cout << "TEST: reject util at mu=0 " << rejectU(0) << "   and at mu=1 " << rejectU(1) << std::endl;

    RiskUtility riskU (gamma, wealth);  // omega implicit in wealth
    std::cout << "TEST: risk at mu=0 " << riskU.risk(0,0.05) << "   and at mu=1 " << riskU.risk(1,0.05) << std::endl;
}


  // test maximization object
  if (false)
  { double gamma (2.5);
    double omega (0.05);
    int    size  (15);
    int    iZero (10);
    WealthArray wealth("bidder", size, omega, iZero, universal);

    RejectUtility utility (gamma, wealth);  // omega implicit in wealth
    
    double gridSize (0.25);
    int    maxIt (100);
    Line_Search::GoldenSection search(.0001, std::make_pair(1.5,4.0), gridSize, maxIt);
    
    double v0   = 0;
    double vkp1 = v0;
    std::cout << "TEST: Initial value is " << v0 << std::endl;
    
    { clock_t time = clock();
      int k (-1);
      std::pair<double,double> maxPair;
      utility.set_constants(wealth.bid(k), v0, vkp1);
      double atZero = utility(0.0);
      maxPair = search.find_maximum(utility);
      std::cout << "    k=" << k << "   @ mu=0, f=" << atZero << "     @mu=" << maxPair.first << " max=" << maxPair.second << std::endl;
      std::cout << "Calculation required " << clock() - time << " tics.\n";
    }
  }

  if (true)
  { double gamma        ( 2.5 );
    double omega        ( 0.05);
    int    nSteps       (10   );
    bool   writeDetails ( true);

    std::cout << "TEST: Solve the bellman reject equation... " << std::endl;
    
    solve_reject_equation (gamma, omega, nSteps, universal, writeDetails);
  }

  return 0;
}


