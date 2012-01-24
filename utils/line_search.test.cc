#include "line_search.h"
#include <math.h>
#include <iostream>
#include <ctime>


  
double f(double x)
{
  return 2.0*x;
}

  
int  main()
{
  std::cout << "\n\nTEST: Test program is starting... \n\n\n";

  std::pair<double, double> searchInterval = std::make_pair(0.0,10.0);
  std::pair<double, double> maxPair;
  
  maxPair = Line_Search::GoldenSection()(&f, searchInterval);
  std::cout << "TEST: Search result is " << maxPair.first << "," << maxPair.second << std::endl;
  
  maxPair = Line_Search::GoldenSection()([](double x){ return (x-2) * (x-2); },searchInterval);
  std::cout << "TEST: Search result is " << maxPair.first << "," << maxPair.second << std::endl;

  maxPair = Line_Search::GoldenSection()([](double x){ return (7-x) * (x-7); }, searchInterval);
  std::cout << "TEST: Search result is " << maxPair.first << "," << maxPair.second << std::endl;
  
  std::cout << "\n\nTEST: Done.\n";

}
