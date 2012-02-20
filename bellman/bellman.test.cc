#include "line_search.Template.h"
#include "normal.h"
#include "bellman.h"

#include <functional>
#include <iostream>
#include <math.h>

#include <ctime>
#include <Eigen/Core>


const double rate = 0.95;

double
negW(int k)
{ double w=1.0;
  for(int i=0;i<k;++i) w *= 1/rate;
  return w;
}

double
posW(int k)
{
  double w=1.0;
  for(int i=0;i<k;++i) w *=  rate ;
  return w;
}


int  main()
{
  // std::cout << "\n\nMAIN: Bellman recursion for competitive ratio of universal bidder. \n\n\n";
  
  // test the probability function
  if (false)
  { 
    double total (0.0);
    int count = 100000;
    for(int k=0; k<count; ++k) total += universal(k,0);
    std::cout << "MAIN: Total of beta(*,0.05) for " << count << " terms = " << total << std::endl;
  } 

  // test time for optimization of gain
  if (false)
  {
    double gamma (2.5);
    double omega (0.05);
    double gridSize (0.25);
    int    maxIt (100);
    clock_t time = clock();
    Line_Search::GoldenSection search(.0001, std::make_pair(1.5,4.0), gridSize, maxIt);
    double spendPct (0.5);
    ExpertCompetitiveGain compRatio (gamma, omega, universal, spendPct);
    
    double v0   = omega - gamma * omega;
    double vkp1 = v0;
    std::cout << "TEST: Initial value is " << v0 << std::endl;
    
    std::pair<double,double> maxPair;
    for (int k=0; k<1000; ++k)
      { compRatio.set_k(k, 1000-k, v0, vkp1);
      double atZero = compRatio(0.0);
      maxPair = search.find_maximum(compRatio);
      if (maxPair.second < atZero)
	maxPair = std::make_pair(0.0,atZero);
      // std::cout << "    k=" << k << "   @ 0 =" << atZero << "     @" << maxPair.first << "  " << maxPair.second << std::endl;
    }
    std::cout << "Calculation required " << clock() - time << " tics.\n";
  }

  // test tracking function
  {
    double omega (0.05);
    int     max   (100);
    std::cout << "TEST: Initializing the wealth array." << std::endl;

    std::cout << "TEST: Neg values for 1, 2, and 3: " << negW(1) << " " << negW(2) << " " << negW(3) << std::endl;
    
    WealthArray wealth("Test", omega, max, negW, posW);
    std::cout << "TEST: wealth array  \n" << wealth << std::endl;

    std::pair< std::pair<int,double>, std::pair<int,double> > kk (wealth.new_position(5,0.05));
    std::cout << "TEST:  increment W[5]= " << wealth[5] << " by 0.05 to " << 0.05+wealth[5] << " bracketed by "
	      << wealth[ kk.first.first] << "*(" << kk.first.second << ") --- "
	      << wealth[kk.second.first] << "*(" << kk.second.second << ")" << std::endl;

    kk = wealth.new_position(15,0.05);
    std::cout << "TEST:  increment W[15]= " << wealth[15] << " by 0.05 to " << 0.05+wealth[15] << " bracketed by "
	      << wealth[ kk.first.first] << "*(" << kk.first.second << ") --- "
	      << wealth[kk.second.first] << "*(" << kk.second.second << ")" << std::endl;


    kk = wealth.new_position(-3,0.05);
    std::cout << "TEST:  increment W[-3]= " << wealth[-3] << " by 0.05 to " << 0.05+wealth[-3] << " bracketed by "
	      << wealth[ kk.first.first] << "*(" << kk.first.second << ") --- "
	      << wealth[kk.second.first] << "*(" << kk.second.second << ")" << std::endl;

  }
  
  return 0;
}


