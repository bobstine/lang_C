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



typedef double (*ProbDist)(int,int);

void
parse_arguments(int argc, char** argv,     double &gamma, int &nRounds, char &probChar, double &spendPct, bool &writeTable);


int
write_matrix_to_file (std::string fileName, Eigen::MatrixXf const& x);
int
write_vector_to_file (std::string fileName, Eigen::VectorXf const& x);



/***********************************************************************************

  Bellman backward recursion for comptitive alpha-investing using
  several possible probability distributions, with spending percentage.

***********************************************************************************/

double univ (int k, int)
{
  const int start = 20;                 // where to start the code
  const double normConst = 0.3346;      // so sums to 1 (computed in MMa)
  double ll = log(k+1+start);
  return 1.0/( (k+start) * ll * ll * normConst);
}

double geometric (int k, int)
{
  const double rate = 0.5;
  double p=1;
  for (int i=0; i<=k; ++i) p *= rate;
  return p;
}

double equal (int k, int left)   // equal spread over possible locations
{
  return 1.0/(double)(k + left);
}


//  This guy does the optimization to find the best mu at given state
class ExpertCompetitiveGain: public std::unary_function<double,double>
{
  typedef std::pair<double,double> DoublePair;
  
private:
  const double mGamma;
  const double mOmega;
  ProbDist mProb;
  const double mSpendPct;
  double mBetaK;
  double mV0, mVkp1;

  
public:
  ExpertCompetitiveGain(double gamma, double omega, ProbDist f, double spendPct)
    : mGamma(gamma), mOmega(omega), mProb(f), mSpendPct(spendPct), mBetaK(0.0) {}

  double beta_k (void) const { return mBetaK; }

  void set_k (int k, int left, double v0, double vkp1)
    {
      mBetaK = mOmega * mSpendPct * mProb(k,left);
      mV0 = v0; mVkp1 = vkp1;  }

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
};  // class


void
solve_bellman_equation (double gamma, double omega, ProbDist f, double spendPct,
			Eigen::MatrixXf &gain, Eigen::MatrixXf &mean, Eigen::MatrixXf &oracle, Eigen::MatrixXf &bidder)  // eek references!
{
  const int maxIterations (100);
  const double tolerance  (0.0001);
  const std::pair<double,double> searchInterval = std::make_pair(1.5,6.5);
  Line_Search::GoldenSection search(tolerance, searchInterval, maxIterations);
  ExpertCompetitiveGain compRatio (gamma, omega, f, spendPct);
    
  // boundary condition goes into extra row
  double v0   = omega - gamma * omega;
  int maxRow = gain.rows();
  for (int j=0; j<maxRow; ++j)
  { gain(maxRow-1,j)  = v0;
    oracle(maxRow-1,j)=omega;
    bidder(maxRow-1,j)=omega;
  }

  // use matrix to capture further intermediate results
  // Eigen::MatrixXf prob = Eigen::MatrixXf::Zero(maxRow,maxRow);
  
  // stores intermediates in rows of triangular array
  for (int row = maxRow-2; row > -1; --row)
  { v0 = gain(row+1,0);
    double b0 = bidder(row+1,0);
    double o0 = oracle(row+1,0);
    for (int col=0; col<=row; ++col)
    { std::pair<double,double> maxPair;
      compRatio.set_k(col, maxRow-1-row, v0, gain(row+1,col+1)); 
      double atZero = compRatio(0.0);
      maxPair = search.find_maximum(compRatio);
      if (maxPair.second < atZero)
	maxPair = std::make_pair(0.0,atZero);
      mean  (row,col) = maxPair.first;
      gain  (row,col) = maxPair.second;
      oracle(row,col) = compRatio.value_to_oracle(maxPair.first, o0, oracle(row+1,col+1));
      bidder(row,col) = compRatio.value_to_bidder(maxPair.first, b0, bidder(row+1,col+1));
      // prob (row,col) = compRatio.beta_k();
    }
  }
  // write_matrix_to_file("/Users/bob/C/tools/probmatrix.txt", prob);
}


int  main(int argc, char** argv)
{
  const double omega  = 0.05;

  // default arguments
  double     gamma  = 2.5;
  int       maxRow  = 100;
  double  spendPct  = 0.5;
  char    probChar  = 'u';
  bool   writeTable = false;    // if false, only return final value

  parse_arguments(argc, argv, gamma, maxRow, probChar, spendPct, writeTable);
  std::cout << gamma << " " << maxRow << " " << writeTable << " " << probChar << " " << spendPct << " ";
  
  Eigen::MatrixXf gain  (maxRow+1, maxRow+1);   // extra row for boundary condition
  Eigen::MatrixXf oracle(maxRow+1, maxRow+1);
  Eigen::MatrixXf bidder(maxRow+1, maxRow+1);
  Eigen::MatrixXf mean  (maxRow,   maxRow);

  // select function for spending down probability
  ProbDist p;
  switch (probChar)
  {
  case 'u': { p = univ;      break; }
  case 'g': { p = geometric; break; }
  case 'e': { p = equal;     break; }
  default: { std::cerr << "ERROR: Unrecognized probablity distribution " << probChar << " chosen.\n"; return -1; }
  }

  // solve the recursion
  solve_bellman_equation (gamma, omega, p, spendPct,   gain, mean, oracle, bidder);
  
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
parse_arguments(int argc, char** argv,		double &gamma, int &nRounds, char &probChar, double &spendPct, bool &writeTable)
{
  static struct option long_options[] = {
    {"gamma",   required_argument, 0, 'g'},
    {"rounds",  required_argument, 0, 'n'},
    {"prob",    required_argument, 0, 'p'},
    {"spend",   required_argument, 0, 's'},
    {"write",         no_argument, 0, 'w'},
    {0, 0, 0, 0}                             // terminator 
  };
  int key;
  int option_index = 0;
  while (-1 !=(key = getopt_long (argc, argv, "g:n:p:s:w", long_options, &option_index))) // colon means has argument
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
    case 'p' :
      {
	probChar = read_utils::lexical_cast<char>(optarg);
	break;
      }
    case 's' :
      {
	spendPct = read_utils::lexical_cast<double>(optarg);
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

