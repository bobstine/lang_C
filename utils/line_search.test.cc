#include "line_search.Template.h"

#include <vector>
#include <utility>   // pair
#include <math.h>
#include <iostream>
#include <ctime>


class F: public std::unary_function<double,double>
{
public: double operator()(double x) const { return -(x-2)*(x-2); }
};

double f(double x)
{
  return 2.0*x;
}
  
int  main()
{
  std::cout << "\n\nTEST: Test program is starting... \n\n\n";

  double searchTolerance = 0.0001;

  if (true)
  { std::cout << "\n\nTEST: invert for position in vector" << std::endl;
    std::vector<double> v(20);
    for (unsigned int i=0;i<v.size(); ++i) v[i] = i;
    std::cout << "TEST: Increasing, to find y=15. then x=" << Line_Search::invert_monotone(15.0,0,v) << std::endl;
    std::cout << "TEST:             to find y=5.2 then x=" << Line_Search::invert_monotone( 5.2,0,v) << std::endl;
    for (unsigned int i=0; i<v.size(); ++i) v[i] = -v[i];
     std::cout << "TEST: Decreasing, to find y=-15. then x=" << Line_Search::invert_monotone(-15.0,0,v) << std::endl;
     std::cout << "TEST:             to find y=-5.2 then x=" << Line_Search::invert_monotone(- 5.2,0,v) << std::endl;
  }    
      
  if (true)  // bisection search for zero
  { std::cout << "\nTEST: bisection\n" ;
    std::pair<double,double> interval (std::make_pair(0.1,4));
    std::cout << "TEST: Zero of  x^2-2 on [0.1,4]: "
	      << Line_Search::Bisection(searchTolerance,interval)([](double x){ return x*x-2; }) << std::endl;
    std::cout << "TEST: Zero of -x^2+2 on [0.1,4]: "
	      << Line_Search::Bisection(searchTolerance,interval)([](double x){ return -x*x+2; }) << std::endl;
    std::cout << "TEST: Zero of log(x)-1 on [0.1,4]: "
	      << Line_Search::Bisection(searchTolerance,interval)([](double x){ return log(x)-1; }) << std::endl;
    std::cout << "TEST: Zero of log(x)-2 on [0.1,4]: [Should cause error message.] "
	      << Line_Search::Bisection(searchTolerance,interval)([](double x){ return log(x)-2; }) << std::endl;
    std::cout << "TEST: Zero of  1/x-1/2   on [0.1,4]: "
	      << Line_Search::Bisection(searchTolerance,interval)([](double x){ return 1/x-0.5; }) << std::endl;
  }
    
  if (true)  // golden section test code; parameters used to create line search
  { std::cout << "\n\n\nTEST: Golden section\n" ;
    std::pair<double, double> searchInterval = std::make_pair(-2.0,5.0);
    std::cout << "TEST: Search interval is [" << searchInterval.first << ", " << searchInterval.second << "]\n";
    double gridSpacing = 0.5;
    int maxIterations = 100;
    // optimizer returns (x,g(x)) pair
    std::pair<double, double> optPair;
    // create with starting parameters
    Line_Search::GoldenSection searcher (searchTolerance, searchInterval, gridSpacing, maxIterations);
    // call ... regular c function; optimum at ends of the search interval
    optPair = searcher.find_minimum(f);
    std::cout << "TEST: Minimum of f=2x   f(" << optPair.first << ") = " << optPair.second << std::endl;
    optPair = searcher.find_maximum(f);
    std::cout << "TEST: Maximum of f=2x   f(" << optPair.first << ") = " << optPair.second << std::endl;
    //     ... function object
    optPair = searcher.find_maximum(F());
    std::cout << "TEST: Maximum f=-(x-2)^2  f(" << optPair.first << ") = " << optPair.second << std::endl;
    //     ... anonymous function
    optPair = searcher.find_minimum([](double x){ return (x-2) * (x-2); });
    std::cout << "TEST: Minimum of (x-2)^2  f(" << optPair.first << ") = " << optPair.second << std::endl;
    //     ... cubic function
    optPair = searcher.find_maximum([](double x) { return x * (x-1.0) * (x-4.0); });
    std::cout << "TEST: Maximum of cubic   f(" << optPair.first << ") = " << optPair.second << std::endl;
    optPair = searcher.find_minimum([](double x) { return x * (x-1.0) * (x-4.0); });
    std::cout << "TEST: Minimum of cubic   f(" << optPair.first << ") = " << optPair.second << std::endl;  } 
  std::cout << "\n\nTEST: Done.\n";
}
