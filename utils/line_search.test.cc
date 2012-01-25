#include "line_search.Template.h"

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

  // parameters used to create line search
  double searchTolerance = 0.0001;
  std::pair<double, double> searchInterval = std::make_pair(0.0,10.0);
  int maxIterations = 100;
  
  // optimizer returns (x,g(x)) pair
  std::pair<double, double> optPair;

  // create with starting parameters
  Line_Search::GoldenSection searcher (searchTolerance, searchInterval, maxIterations);

  
  // call ... regular c function
  optPair = searcher.find_minimum(f);
  std::cout << "TEST: Min result is " << optPair.first << "," << optPair.second << std::endl;

  //     ... function object
  optPair = searcher.find_maximum(F());
  std::cout << "TEST: Max result is " << optPair.first << "," << optPair.second << std::endl;

  //     ... anonymous function
  optPair = searcher.find_minimum([](double x){ return (x-2) * (x-2); });
  std::cout << "TEST: Min result is " << optPair.first << "," << optPair.second << std::endl;
  
  std::cout << "\n\nTEST: Done.\n";
}
