#include "line_search.Template.h"
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
      double b = beta(mK, mOmega);
      if(mu < 0.00001)
      { // std::cout << "For mu = 0 reject prob r = b = " << b << " and V0 = " << mV0 << " and V_k+1 = " << mVkp1 << std::endl;
	return mGamma * b * (1.0-mOmega) + b*mV0 + (1-b)*mVkp1;
      }
      else
      {
	double rb = reject(mu,b);
	double a = alpha(mu);
	double ra = reject(mu,a);
	double gain = (mOmega * ra - a) - mGamma * (mOmega * rb - b) + rb * mV0 + (1-rb) * mVkp1;
	// std::cout << "For mu=" << mu << " reject prob rb=" << rb << " ra=" << ra  << " a=" << a << " b=" << b << " gain=" << gain << std::endl;
	return gain;
      }
    }
  
private: 
  inline double alpha (double mu) const
    {
      double z = (mu * mu + 2 * log(1.0/mOmega))/(2 * mu);
      return 1.0 - normal_cdf(z);
    }
  
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
  if (false)
  {
    double total (0.0);
    int count = 100000;
    for(int k=0; k<count; ++k) total += beta(k,0.05);
    std::cout << "MAIN: Total of beta(*,0.05) for " << count << " terms = " << total << std::endl;
  } 

  // test time for optimization of gain
  {
    clock_t time = clock();
    Line_Search::GoldenSection search(.0001, std::make_pair(1.5,4.0), 100);
    ExpertCompetitiveGain compRatio (gamma, omega);
    
    double v0   = omega - gamma * omega;
    double vkp1 = v0;
    std::cout << "TEST: Initial value is " << v0 << std::endl;
    
    std::pair<double,double> maxPair;
    for (int k=0; k<1000; ++k)
    { compRatio.set_k(k, v0, vkp1);
      double atZero = compRatio(0.0);
      maxPair = search.find_maximum(compRatio);
      if (maxPair.second < atZero)
	maxPair = std::make_pair(0.0,atZero);
      // std::cout << "    k=" << k << "   @ 0 =" << atZero << "     @" << maxPair.first << "  " << maxPair.second << std::endl;
    }

    std::cout << "TEST: Took " << clock()-time << " ticks.\n";
  }
  
  std::cout << "\n\n MAIN: Bellman done.\n";
}
