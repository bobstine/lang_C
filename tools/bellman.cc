#include "line_search.h"
#include "normal.h"
#include <math.h>

#include <functional>
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

 
class ExpertCompetitiveGain: public std::unary_function<double,double>
{
private:
  const double mGamma;
  const double mOmega;
  int mK;
  double mV0, mVkp1;
  
public:
  ExpertCompetitiveGain(double gamma, double omega)
    : mGamma(gamma), mOmega(omega) {}

  void set_k (int k, double v0, double vkp1)
    { mK = k; mV0 = v0; mVkp1 = vkp1;  }
  
  double operator()(double mu) const
    {
      double a = alpha(mu,mOmega);
      double b = beta(mK, mOmega);
      double r = reject(mu,b);
      return (mOmega * reject(mu,a) - a) - mGamma * (mOmega * r - b) + r * mV0 + (1-r) * mVkp1;
    }
  
private: 
  inline double reject(double mu, double level) const
    {
      return normal_cdf(mu-normal_quantile(1-level));
    }

};

  
int  main()
{
  std::cout << "\n\nMAIN: Bellman recursion for competitive ratio of universal bidder. \n\n\n";

  // parameters for all functions
  const double gamma = 2.5;
  const double omega = 0.05;

  
  // test the probability function
  {
    double total (0.0);
    for(int k=0; k<100000; ++k) total += beta(k,0.05);
    std::cout << "MAIN: Total of beta(*,0.05) for 100 000 terms = " << total << std::endl;
  } 

  // test time for optimization of gain
  {
    clock_t time = clock();
    Line_Search::GoldenSection maximizer(.0001, std::make_pair(1.5,4.0), 100);
    ExpertCompetitiveGain compRatio (gamma, omega);
    
    std::pair<double,double> maxPair;
    double v0   = 0.05;  // as in initialization
    double vkp1 = 0.05;
    for (int k=0; k<100; ++k)
    { compRatio.set_k(k, v0, vkp1);
      maxPair = maximizer(compRatio);
      std::cout << maxPair.second << " ";
    }
    std::cout << "\nTEST: Search result is " << maxPair.first << "," << maxPair.second << std::endl;
    std::cout << "TEST: Took " << clock()-time << " ticks.\n";
  }

  
  std::cout << "\n\n MAIN: Bellman done.\n";

}
