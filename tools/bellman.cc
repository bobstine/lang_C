#include "bellman.h"
#include "line_search.Template.h"
#include "normal.h"

#include <Eigen/Core>
#include <math.h>
#include <functional>
#include <iostream>
#include <fstream>


typedef Eigen::MatrixXf Matrix;

///////////////////////////////////////  Write data to file  /////////////////////////////

int
write_matrix_to_file (std::string fileName, Matrix const& x)
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

////////////////////////////////////  Utility functions  /////////////////////////////////////////

double
reject_prob(double mu, double level)
{
  return normal_cdf(mu-normal_quantile(1-level));
}  

  
double
optimal_alpha (double mu, double omega) 
{ double z = (mu * mu + 2 * log(1.0/omega))/(2 * mu);
  return 1.0 - normal_cdf(z);
}



/////////////////////////////////////////  Distributions  ////////////////////////////////////////

double universal (int k, int)
{
  const int start = 20;                 // where to start the code
  const double normConst = 0.3346;      // so sums to 1 (computed in MMa)
  double ll = log(k+1+start);
  return 1.0/( (k+start) * ll * ll * normConst);
}

double geometric (int k, int)
{
  double p=1;
  for (int i=0; i<=k; ++i) p *= 0.99;
  return p/99.0;
}

double equal (int k, int left)         // equal spread over possible locations
{
  return 1.0/(double)(k + left);
}



////////////////////////////////  Constrained Solver  ////////////////////////////////////////
//
// this version handles state-dependent expert, returning the grid of expert and bidder values
// needed for the next round of the recursion.
//
/////////////////////////////////////////////////////////////////////////////////////////////

void
solve_constrained_bellman_equation (double gamma, double omega, int nRounds, double spendPct, ProbDist oracleProb, ProbDist bidderProb)
{
  const int maxIterations (100);
  const double tolerance  (0.0001);
  const std::pair<double,double> searchInterval = std::make_pair(0,6.5);
  Line_Search::GoldenSection search(tolerance, searchInterval, maxIterations);
  ConstrainedExpertCompetitiveGain compRatio (gamma, omega, spendPct, oracleProb, bidderProb);
    
  // space for holding intermediate results; extra row for boundary condition
  // code flips between these on read and write
  Matrix gain0   = Matrix::Constant (nRounds+1, nRounds+1, omega - gamma * omega);
  Matrix oracle0 = Matrix::Constant (nRounds+1, nRounds+1, omega);
  Matrix bidder0 = Matrix::Constant (nRounds+1, nRounds+1, omega);
  Matrix gain1   = Matrix::Zero (nRounds+1, nRounds+1);
  Matrix oracle1 = Matrix::Zero (nRounds+1, nRounds+1);
  Matrix bidder1 = Matrix::Zero (nRounds+1, nRounds+1);
  Matrix mean    = Matrix::Zero (nRounds, nRounds);
  bool use0 = true;

  Matrix* pGainSrc;   Matrix* pGainDest = NULL;
  Matrix* pOracleSrc; Matrix* pOracleDest = NULL;
  Matrix* pBidderSrc; Matrix* pBidderDest = NULL;
  for (int round = nRounds; round > -1; --round)
  { if (use0)
    { pGainSrc    = &gain0;
      pOracleSrc  = &oracle0;
      pBidderSrc  = &bidder0;
      pGainDest   = &gain1;
      pOracleDest = &oracle1;
      pBidderDest = &bidder1;
    } else
    { pGainSrc    = &gain1;
      pOracleSrc  = &oracle1;
      pBidderSrc  = &bidder1;
      pGainDest   = &gain0;
      pOracleDest = &oracle0;
      pBidderDest = &bidder0;
    }
    use0 = !use0;
    std::cout << " --------------- " << round << " --------------------- " << std::endl << (*pGainSrc) << std::endl;
    for (int i=0; i<round; ++i)        // next round status of expert
    { for (int j=0; j<round; ++j)      //                      bidder
      { std::pair<double,double> maxPair;
	compRatio.set_delay (i, j, round, nRounds, (*pGainSrc)(0,0), (*pGainSrc)(i+1,0), (*pGainSrc)(0,j+1), (*pGainSrc)(i+1,j+1));
	// ??? should not need this, but not unimodal; need preliminary grid search
	double atZero = compRatio(0.0);
	maxPair = search.find_maximum(compRatio);
	if (maxPair.second < atZero)
	  maxPair = std::make_pair(0.0,atZero);
	mean(i,j) = maxPair.first;
	(*pGainDest)(i,j) = maxPair.second;
	(*pOracleDest)(i,j) = compRatio.value_to_oracle(maxPair.first, (*pOracleSrc)(0,0), (*pOracleSrc)(i+1,0), (*pOracleSrc)(0,j+1), (*pOracleSrc)(i+1,j+1));
	(*pBidderDest)(i,j) = compRatio.value_to_bidder(maxPair.first, (*pBidderSrc)(0,0), (*pBidderSrc)(i+1,0), (*pBidderSrc)(0,j+1), (*pBidderSrc)(i+1,j+1));
      }
    }
    std::cout << " ---------------   MEAN  --------------------- " << std::endl << mean << std::endl;
    pGainSrc->setZero();
  }
  // write the final values to std io
  std::cout << (*pGainDest)(0,0) << " " << (*pOracleDest)(0,0) << " " << (*pBidderDest)(0,0) << std::endl;
}




////////////////////////////////  Unconstrained Solver  ////////////////////////////////////////

void
solve_bellman_equation (double gamma, double omega, int nRounds, double spendPct, ProbDist f, bool writeDetails)
{
  const int maxIterations (100);
  const double tolerance  (0.0001);
  const std::pair<double,double> searchInterval = std::make_pair(1.5,6.5);
  Line_Search::GoldenSection search(tolerance, searchInterval, maxIterations);
  ExpertCompetitiveGain compRatio (gamma, omega, f, spendPct);
    
  // space for holding intermediate results
  Matrix gain  (nRounds+1, nRounds+1);   // extra row for boundary condition
  Matrix oracle(nRounds+1, nRounds+1);
  Matrix bidder(nRounds+1, nRounds+1);
  Matrix mean = Matrix::Zero(nRounds,nRounds);
  // capture further results
  //  Matrix prob = Matrix::Zero(nRounds,nRounds);
  
  //  initialize boundary conditions
  double v0   = omega - gamma * omega;
  for (int j=0; j<nRounds+1; ++j)
  { gain(nRounds,j)  = v0;
    oracle(nRounds,j)=omega;
    bidder(nRounds,j)=omega;
  }
  
  // stores intermediates in rows of triangular array
  for (int row = nRounds-1; row > -1; --row)
  { v0 = gain(row+1,0);
    double b0 = bidder(row+1,0);
    double o0 = oracle(row+1,0);
    for (int col=0; col<=row; ++col)
    { std::pair<double,double> maxPair;
      compRatio.set_k(col, nRounds-1-row, v0, gain(row+1,col+1)); 
      double atZero = compRatio(0.0);
      maxPair = search.find_maximum(compRatio);
      if (maxPair.second < atZero)
	maxPair = std::make_pair(0.0,atZero);
      gain  (row,col) = maxPair.second;
      oracle(row,col) = compRatio.value_to_oracle(maxPair.first, o0, oracle(row+1,col+1));
      bidder(row,col) = compRatio.value_to_bidder(maxPair.first, b0, bidder(row+1,col+1));
      mean (row,col) = maxPair.first;
      //      prob (row,col) = compRatio.beta_k();
    }
  }
  
  // write solution (without boundary row) to file
  if(writeDetails)
  { std::string fileName;
    std::ostringstream ss;
    int gammaInt (trunc(10 *gamma));
    ss << "bellman.g" << gammaInt << ".n" << nRounds << ".";
    fileName  = ss.str() + "gain";
    write_matrix_to_file(fileName, gain.topLeftCorner(gain.rows()-1, gain.rows()-1));  // omit boundary row
    fileName = ss.str() + "oracle";
    write_matrix_to_file(fileName, oracle.topLeftCorner(oracle.rows()-1,oracle.rows()-1));
    fileName = ss.str() + "bidder";
    write_matrix_to_file(fileName, bidder.topLeftCorner(bidder.rows()-1, bidder.rows()-1));
    fileName = ss.str() + "mu";
    write_matrix_to_file(fileName, mean.topLeftCorner(mean.rows(), mean.rows()));
    //    write_matrix_to_file("/Users/bob/C/tools/probmatrix.txt", prob);
  }
  // write the final values to std io
  std::cout << gain(0,0) << " " << oracle(0,0) << " " << bidder(0,0) << std::endl;
}






/////////////////////////////////  Unconstrained Expert  ///////////////////////////////////////


double
ExpertCompetitiveGain::operator()(double mu) const
{
  if(mu < 0.00001)
  { // std::cout << "For mu = 0 reject prob r = b = " << b << " and V0 = " << mV0 << " and V_k+1 = " << mVkp1 << std::endl;
    return mGamma * mBetaK * (1.0-mOmega) + mBetaK*mV0 + (1-mBetaK)*mVkp1;
  }
  else
  {
    double rb = reject_prob(mu,mBetaK);
    double a = optimal_alpha(mu, mOmega);
    double ra = reject_prob(mu,a);
    double gain = (mOmega * ra - a) - mGamma * (mOmega * rb - mBetaK) + rb * mV0 + (1-rb) * mVkp1;
    return gain;
  }
}
  
double
ExpertCompetitiveGain::value_to_oracle(double mu, double o0, double okp1) const    // expert 
{
  double value, rb;
  if (mu < 0.00001)
  { value = 0.0;
    rb = mBetaK;
  }
  else
  { double a = optimal_alpha(mu, mOmega);
    double ra = reject_prob(mu,a);
    value = mOmega * ra - a ;
    rb = reject_prob(mu, mBetaK);
  }
  return value + rb * o0 + (1-rb) * okp1;
}
  
double
ExpertCompetitiveGain::value_to_bidder (double mu, double b0, double bkp1) const    // universal bidder
{
  double rb = (mu < 0.00001) ? mBetaK : reject_prob(mu,mBetaK);
  return mOmega * rb - mBetaK + rb * b0 + (1-rb) * bkp1;
}    


/////////////////////////////////  Unconstrained Expert  ///////////////////////////////////////


void
ConstrainedExpertCompetitiveGain::set_delay (int i, int j, int t, int nRounds, double v00, double vi0, double v0j, double vij)
{
  mAlpha = mOmega * mSpendPct * mExpertProb(i,nRounds-t);
  mBeta  = mOmega * mSpendPct * mBidderProb(j,nRounds-t);
  mV00 = v00;
  mVi0 = vi0;
  mV0j = v0j;
  mVij = vij;
}

double
ConstrainedExpertCompetitiveGain::operator()(double mu) const
{
  //  double ra = reject_prob(mu,mAlpha);
  // --- ignore the state dependence
  double a = optimal_alpha(mu, mOmega);
  double ra = reject_prob(mu,a);
  //
  double rb = reject_prob(mu,mBeta);
  double gain = (mOmega * ra - a) - mGamma * (mOmega * rb - mBeta) + ra * rb * mV00 + (1-ra) * (1-rb) * mVij + ra * (1-rb) * mV0j + (1-ra) * rb * mVi0;
  return gain;
}
  

double
ConstrainedExpertCompetitiveGain::value_to_oracle(double mu, double v00, double vi0, double v0j, double vij) const
{
  double ra = reject_prob(mu,mAlpha);
  double rb = reject_prob(mu,mBeta);
  return  (mOmega * ra - mAlpha) + ra * rb * v00 + (1-ra) * (1-rb) * vi0 + ra * (1-rb) * v0j + (1-ra) * rb * vij;
}


double
ConstrainedExpertCompetitiveGain::value_to_bidder(double mu, double v00, double vi0, double v0j, double vij) const
{
  double ra = reject_prob(mu,mAlpha);
  double rb = reject_prob(mu,mBeta);
  return  (mOmega * rb - mBeta) + ra * rb * v00 + (1-ra) * (1-rb) * vi0 + ra * (1-rb) * v0j + (1-ra) * rb * vij;
}



