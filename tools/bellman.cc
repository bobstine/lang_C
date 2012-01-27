#include <Eigen/Core>

#include "line_search.Template.h"
#include "normal.h"
#include <math.h>

#include <functional>
#include <iostream>

#include <fstream>
#include <sstream>
#include <getopt.h>
#include "read_utils.h"     

#include <ctime>



void
parse_arguments(int argc, char** argv,     double &gamma, int &nRounds, bool &writeTable);




double univ (int k)
{
  const int start = 20;                 // where to start the code
  const double normConst = 0.3346;      // so sums to 1 (computed in MMa)
  double ll = log(k+1+start);
  return 1.0/( (k+start) * ll * ll * normConst);
}

double beta (int k, double omega)
{
  const double spendingPct = 0.5;
  return omega * spendingPct * univ(k); 
}


 
class ExpertCompetitiveGain: public std::unary_function<double,double>
{
private:
  const double mGamma;
  const double mOmega;
  
  int    mK;
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

void
solve_bellman_equation (double gamma, double omega, Eigen::MatrixXf &gain, Eigen::MatrixXf &mean)
{
  Line_Search::GoldenSection search(.0001, std::make_pair(1.5,4.0), 100);
  ExpertCompetitiveGain compRatio (gamma, omega);
    
  // initial boundary condition
  double v0   = omega - gamma * omega;
  //  std::cout << "BELL: Initial value is " << v0 << std::endl;  
  int maxRow = gain.rows();
  for (int j=0; j<maxRow; ++j)
  { gain(maxRow-1,j) = v0;
    mean(maxRow-1,j) = 0.0;
  }
  // fill rows in triangular array
  for (int row = maxRow-2; row > -1; --row)
  { v0 = gain(row+1,0);
    for (int col=0; col<=row; ++col)
    { std::pair<double,double> maxPair;
      compRatio.set_k(col, v0, gain(row+1,col+1));
      double atZero = compRatio(0.0);
      maxPair = search.find_maximum(compRatio);
      if (maxPair.second < atZero)
	maxPair = std::make_pair(0.0,atZero);
      mean(row,col) = maxPair.first;
      gain(row,col) = maxPair.second;
      // std::cout << "    k=" << k << "   @ 0 =" << atZero << "     @" << maxPair.first << "  " << maxPair.second << std::endl;
    }
  }
}

int
write_matrix_to_file (std::string fileName, Eigen::MatrixXf const& x)
{
  std::ofstream output (fileName.c_str());
  if (! output)
  { std::cerr << "ERROR: Cannot open output text file for writing " << fileName << std::endl;
    return 1;
  }
  for (int i=0; i<x.rows(); ++i)
  { for (int j=0; j<x.cols(); ++j)
      output << x(i,j) << " ";
    output << std::endl;
  }
  return 0;
}

int  main(int argc, char** argv)
{
  // std::cout << "\n\nMAIN: Bellman recursion for competitive ratio of universal bidder. \n\n\n";
  
  // test the probability function
  if (false)
  { 
    double total (0.0);
    int count = 100000;
    for(int k=0; k<count; ++k) total += beta(k,0.05);
    std::cout << "MAIN: Total of beta(*,0.05) for " << count << " terms = " << total << std::endl;
  } 

  // test time for optimization of gain
  if (false)
  {
    double gamma (2.5);
    double omega (0.05);
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
    std::cout << "Calculation required " << clock() - time << " tics.\n";
  }

  // fill matrix solution for finite horizon
  {
    // parameters for all functions
    const double omega  = 0.05;

    double     gamma  = 2.5;
    int       maxRow  = 100;
    bool   writeTable = false;    // if false, only find final value
    
    parse_arguments(argc, argv, gamma, maxRow, writeTable);

    Eigen::MatrixXf gain(maxRow+1,maxRow+1);
    Eigen::MatrixXf mean(maxRow+1,maxRow+1);
    
    solve_bellman_equation (gamma, omega, gain, mean);
    
    // print first 10 rows and columns of gain and mean
    if (false)
    { 
      for (int i=0; i<10; ++i)
      { std::cout << "\n[" << i << "] ";
	for (int j=0; j<10; ++j)
	  std::cout << gain(i,j) << " ";
      }
      std::cout << std::endl;
      for (int i=0; i<10; ++i)
      { std::cout << "\n[" << i << "] ";
	for (int j=0; j<10; ++j)
	  std::cout << mean(i,j) << " ";
      }
    }
    
    // write solution (without boundary row) to file
    if(writeTable)
    { int gammaInt (trunc(10 *gamma));
      std::ostringstream ss;
      ss << "bellman.g" << gammaInt << ".n" << maxRow;
      std::string fileName  (ss.str() + ".mu");
      write_matrix_to_file(fileName, mean.topLeftCorner(mean.rows()-1, mean.rows()-1));  // omit boundary row
      fileName  = ss.str() + ".gain";
      write_matrix_to_file(fileName, mean.topLeftCorner(mean.rows()-1, gain.rows()-1));
    }

    // write the final value to std io
    std::cout << gain(0,0) << std::endl;
  } 
  return 0;
}





void
parse_arguments(int argc, char** argv,
		double &gamma,
		int &nRounds,
		bool &writeTable)
{
  int key;
  while (1)                                  // read until empty key causes break
    {
      int option_index = 0;
      static struct option long_options[] = {
	  {"gamma",             1, 0, 'g'},  // has arg,
	  {"rounds",            1, 0, 'r'},  // has arg,
	  {"write",             0, 0, 'w'},  // no arg, switch
	  {0, 0, 0, 0}                       // terminator 
	};
	key = getopt_long (argc, argv, "grw", long_options, &option_index);
	if (key == -1)
	  break;
	std::cout << "Option key " << char(key) << " with option_index " << option_index << " and arg " << optarg << std::endl;
	switch (key)
	  {
	  case 'g' : 
	    {
	      gamma = read_utils::lexical_cast<double>(optarg);
	      break;
	    }
	  case 'r' :
	    {
	      nRounds = read_utils::lexical_cast<int>(optarg);
	      break;
	    }
	  case 'w' : 
	    {
	      writeTable=true ;
	      break;
	    }
	  }
    }
}

