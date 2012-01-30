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


/***********************************************************************************

  Bellman backward recursion for comptitive alpha-investing using
  Rissanen type universal prior, with spending percentage.

***********************************************************************************/

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
  typedef std::pair<double,double> DoublePair;
  
private:
  const double mGamma;
  const double mOmega;
  
  double mBetaK;
  double mV0, mVkp1;
  
public:
  ExpertCompetitiveGain(double gamma, double omega)
    : mGamma(gamma), mOmega(omega) {}

  void set_k (int k, double v0, double vkp1)
    { mBetaK = beta(k,mOmega); mV0 = v0; mVkp1 = vkp1;  }

  double operator()(double mu) const
    {
      if(mu < 0.00001)
      { // std::cout << "For mu = 0 reject prob r = b = " << b << " and V0 = " << mV0 << " and V_k+1 = " << mVkp1 << std::endl;
	return mGamma * mBetaK * (1.0-mOmega) + mBetaK*mV0 + (1-mBetaK)*mVkp1;
      }
      else
      {
	double rb = reject(mu,mBetaK);
	double a = alpha(mu);
	double ra = reject(mu,a);
	double gain = (mOmega * ra - a) - mGamma * (mOmega * rb - mBetaK) + rb * mV0 + (1-rb) * mVkp1;
	return gain;
      }
    }
  
  double value_to_oracle(double mu, double o0, double okp1) const    // expert 
    {
      double value, rb;
      if (mu < 0.00001)
      { value = 0.0;
	rb = mBetaK;
      }
      else
      { double a = alpha(mu);
	double ra = reject(mu,a);
	value = mOmega * ra - a ;
	rb = reject(mu, mBetaK);
      }
      return value + rb * o0 + (1-rb) * okp1;
    }
  
  double value_to_bidder (double mu, double b0, double bkp1) const    // universal bidder
    {
      double rb = (mu < 0.00001) ? mBetaK : reject(mu,mBetaK);
      return mOmega * rb - mBetaK + rb * b0 + (1-rb) * bkp1;
    }    
  
private: 
  inline double alpha (double mu) const
    { double z = (mu * mu + 2 * log(1.0/mOmega))/(2 * mu);
      return 1.0 - normal_cdf(z);
    }
  
  inline double reject(double mu, double level) const
    { return normal_cdf(mu-normal_quantile(1-level));
    }

};


void
solve_bellman_equation (double gamma, double omega, Eigen::MatrixXf &gain, Eigen::MatrixXf &mean, Eigen::MatrixXf &oracle, Eigen::MatrixXf &bidder)
{
  const int maxIterations (100);
  const double tolerance  (0.0001);
  Line_Search::GoldenSection search(tolerance, std::make_pair(1.5,4.0), maxIterations);
  ExpertCompetitiveGain compRatio (gamma, omega);
    
  // initial boundary condition goes into extra row
  double v0   = omega - gamma * omega;
  int maxRow = gain.rows();
  for (int j=0; j<maxRow; ++j)
  { gain(maxRow-1,j)  = v0;
    oracle(maxRow-1,j)=omega;
    bidder(maxRow-1,j)=omega;
  }
  // fill rows in triangular array
  for (int row = maxRow-2; row > -1; --row)
  { v0 = gain(row+1,0);
    double b0 = bidder(row+1,0);
    double o0 = oracle(row+1,0);
    for (int col=0; col<=row; ++col)
    { std::pair<double,double> maxPair;
      compRatio.set_k(col, v0, gain(row+1,col+1));
      double atZero = compRatio(0.0);
      maxPair = search.find_maximum(compRatio);
      if (maxPair.second < atZero)
	maxPair = std::make_pair(0.0,atZero);
      mean  (row,col) = maxPair.first;
      gain  (row,col) = maxPair.second;
      oracle(row,col) = compRatio.value_to_oracle(maxPair.first, o0, oracle(row+1,col+1));
      bidder(row,col) = compRatio.value_to_bidder(maxPair.first, b0, bidder(row+1,col+1));
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


int
write_vector_to_file (std::string fileName, Eigen::VectorXf const& x)
{
  std::ofstream output (fileName.c_str());
  if (! output)
  { std::cerr << "ERROR: Cannot open output text file for writing " << fileName << std::endl;
    return 1;
  }
  for (int i=0; i<x.size(); ++i)
      output << x(i) << " ";
  output << std::endl;
  return 0;
}


int  main(int argc, char** argv)
{
  const double omega  = 0.05;

  // default arguments
  double     gamma  = 2.5;
  int       maxRow  = 100;
  bool   writeTable = false;    // if false, only return final value

  parse_arguments(argc, argv, gamma, maxRow, writeTable);
  std::cout << gamma << " " << maxRow << " " << writeTable << " ";
  
  Eigen::MatrixXf gain  (maxRow+1, maxRow+1);   // extra row for boundary condition
  Eigen::MatrixXf oracle(maxRow+1, maxRow+1);
  Eigen::MatrixXf bidder(maxRow+1, maxRow+1);
  Eigen::MatrixXf mean  (maxRow,   maxRow);

  solve_bellman_equation (gamma, omega, gain, mean, oracle, bidder);
  
  // write solution (without boundary row) to file
  if(writeTable)
  { int gammaInt (trunc(10 *gamma));
    std::ostringstream ss;
    ss << "bellman.g" << gammaInt << ".n" << maxRow << ".";
    std::string fileName  (ss.str() + "mu");
    write_matrix_to_file(fileName, mean.topLeftCorner(mean.rows(), mean.rows()));
    fileName  = ss.str() + "gain";
    write_matrix_to_file(fileName, gain.topLeftCorner(gain.rows()-1, gain.rows()-1));  // omit boundary row
    fileName = ss.str() + "oracle";
    write_matrix_to_file(fileName, oracle.topLeftCorner(oracle.rows()-1,oracle.rows()-1));
    fileName = ss.str() + "bidder";
    write_matrix_to_file(fileName, bidder.topLeftCorner(bidder.rows()-1, bidder.rows()-1));
  }
  
  // write the final values to std io
  std::cout << gain(0,0) << " " << oracle(0,0) << " " << bidder(0,0) << std::endl;
  
  return 0;
}



void
parse_arguments(int argc, char** argv,		double &gamma, int &nRounds, bool &writeTable)
{
  static struct option long_options[] = {
    {"gamma",   required_argument, 0, 'g'},  // has arg,
    {"rounds",  required_argument, 0, 'n'},  // has arg,
    {"write",         no_argument, 0, 'w'},  // no arg, switch
    {0, 0, 0, 0}                             // terminator 
  };
  int key;
  int option_index = 0;
  while (-1 !=(key = getopt_long (argc, argv, "g:n:w", long_options, &option_index))) // do not access optarg no_argument (eg. writea)
  {
    // std::cout << "Option key " << char(key) << " for option " << long_options[option_index].name << ", option_index=" << option_index << std::endl;
    switch (key)
    {
    case 'g' : 
      {
	gamma = read_utils::lexical_cast<double>(optarg);
	break;
      }
    case 'n' :
      {
	nRounds = read_utils::lexical_cast<int>(optarg);
	break;
      }
    case 'w' : 
      {
	writeTable=true ;
	break;
      }
    default:
      {
	std::cout << "PARSE: Option not recognized; returning.\n";
      }
    } // switch
  } // while
}

