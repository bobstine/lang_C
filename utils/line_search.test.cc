#include "line_search.h"
#include "normal.h"
#include <math.h>

#include <iostream>
#include <ctime>


double univ (int k)
{
  const int start = 20;
  const double normConst = 0.3346;
  double ll = log(k+1+start);
  return 1.0/( (k+start) * ll * ll * normConst);
}

double beta (int k, double omega)
{
  return omega * .5 * univ(k);
}

double alpha (double mu, double omega)
{
  double z = (mu * mu + 2 * log(1.0/omega))/(2 * mu);
  return 1.0 - normal_cdf(z);
}

double reject(double mu, double level)
{
  return normal_cdf(mu-normal_quantile(1-level));
}

double univBidder(double mu)
{
  double gamma = 2.5;
  double omega = 0.05;
  int k = 3;
  double v0  = 0.0;
  double vk1 = 1.0;
  
  double a = alpha(mu,omega);
  double b = beta(k, omega);

  double r = reject(mu,b);
  return (omega * reject(mu,a) - a) - gamma * (omega*r - b) + r * v0 + (1-r) * vk1;
}

  
double f(double x)
{
  return 2.0*x;
}

  
int  main()
{
  std::cout << "\n\nTEST: Test program is starting... \n\n\n";

  std::pair<double, double> searchInterval = std::make_pair(0.0,10.0);
  
  std::pair<double, double> maxPair;


  
  maxPair = Line_Search::GoldenSection()(&f,
					 searchInterval);
  std::cout << "TEST: Search result is " << maxPair.first << "," << maxPair.second << std::endl;

  
  maxPair = Line_Search::GoldenSection()([](double x){ return (x-2) * (x-2); },
					 searchInterval);
  std::cout << "TEST: Search result is " << maxPair.first << "," << maxPair.second << std::endl;

    

  {
    clock_t time = clock();

    maxPair = Line_Search::GoldenSection()([](double x){ return (7-x) * (x-7); },
					   searchInterval);
    std::cout << "TEST: Search result is " << maxPair.first << "," << maxPair.second << std::endl;
    std::cout << "TEST: Took " << clock()-time << " ticks." << std::endl;
  }
  
  std::cout << "\n\nTEST: Done.\n";

}
