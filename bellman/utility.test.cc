#include "line_search.Template.h"
#include "utility.h"
#include "wealth.h"

#include <iostream>

#include <ctime>
#include <Eigen/Core>
 

int  main()
{

  if (true) // basic test
  {
    std::cout << "TEST: Preparing to test the utility object." << std::endl;    double gamma (2.0 );

    double omega (0.05);
    double alpha (0.025);
    double beta  (0.0125);
    { // vector
      RejectVectorUtility rejectU (gamma, omega);
      std::cout << "TEST: reject util at mu=0 " << rejectU(0) << "   and at mu=1 " << rejectU(1) << std::endl;    
      std::cout << "TEST: risk at mu=0 " << risk(0,0.05) << "   and at mu=1 " << risk(1,0.05) << std::endl;
      // check additive
      double mu (1.8);
      RiskVectorUtility riskU (gamma, omega); 
      riskU.set_constants(alpha, beta, 10., 3.);
      std::cout << "TEST: additivity...   net " << riskU(mu) << " = " << riskU.oracle_utility(mu, 10,3) << " - " << gamma << "*" << riskU.bidder_utility(mu,10,3) << std::endl;
    }
      
    {  // matrix
      RejectMatrixUtility rejectU (gamma, omega);  
      std::cout << "TEST: reject util at mu=0 " << rejectU(0) << "   and at mu=1 " << rejectU(1) << std::endl;
      // check additive
      double mu (1.8);
      RiskMatrixUtility riskU (gamma, omega); 
      riskU.set_constants(alpha, beta, 3, 5, 7, 11);
      std::cout << "TEST: risk   util at mu=0 " << riskU(0) << "   and at mu=1 " << riskU(1) << std::endl;
      std::cout << "TEST: additivity...   net " << riskU(mu) << " = " << riskU.oracle_utility(mu, 3,5,7,11) << " - " << gamma << "*" << riskU.bidder_utility(mu,3,5,7,11) << std::endl;
    }
  }
  
  if(false)  // test maximize
  { double gamma (2.5);
    double omega (0.05);
    int    size  (15);
    int    iZero (10);
    WealthArray wealth("bidder", size, omega, iZero, universal);

    RejectVectorUtility utility (gamma, omega);
    
    double gridSize (0.25);
    int    maxIt (100);
    Line_Search::GoldenSection search(.0001, std::make_pair(1.5,4.0), gridSize, maxIt);
    
    double v0   = 0;
    double v1 = v0;
    std::cout << "TEST: Initial value is " << v0 << std::endl;
    
    { clock_t time = clock();
      int k (-1);
      std::pair<double,double> maxPair;
      utility.set_constants(wealth.bid(k), v0, v1);
      double atZero = utility(0.0);
      maxPair = search.find_maximum(utility);
      std::cout << "    k=" << k << "   @ mu=0, f=" << atZero << "     @mu=" << maxPair.first << " max=" << maxPair.second << std::endl;
      std::cout << "Calculation required " << clock() - time << " tics.\n";
    }
  }

  return 0;
}


