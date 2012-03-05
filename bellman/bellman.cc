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
reject_prob(double mu, double level)    // r_mu(alpha)
{
  if(level == 0)
    return 0.0;
  else
    return normal_cdf(mu-normal_quantile(1-level));
}  

  
double
optimal_alpha (double mu, double omega) 
{ if (mu < .001)
    return 0.0;
  else
  { double z = (mu * mu + 2 * log(1.0/omega))/(2 * mu);
    return 1.0 - normal_cdf(z);
  }
}

int
imin(int a, int b)
{ if (a < b) return a; else return b; }

/////////////////////////////////////////  Distributions  ////////////////////////////////////////

double universal (int k)
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
  return p/99.;
}

double uniform_to_end (int k, int left)         // equal spread over possible locations
{
  return 1.0/(double)(k + left);
}



// --------------------------------------------------------------------------------------------------------------
//
//      Constrained    Constrained    Constrained    Constrained    Constrained    Constrained    Constrained    
//
//      This version handles state-dependent expert, returning the grid of expert and bidder values
//      needed for the next round of the recursion.
//
// --------------------------------------------------------------------------------------------------------------


void
solve_constrained_bellman_alpha_equation (double gamma, double omega, int nRounds, double spendPct, double oracleProb, ProbDist bidderProb, bool writeDetails)
{
  const int maxIterations (200);   
  const double tolerance  (0.0001);
  const double grid       (0.5);
  const std::pair<double,double> searchInterval = std::make_pair(0.05,7.0);
  Line_Search::GoldenSection search(tolerance, searchInterval, grid, maxIterations);
  ConstrainedExpertCompetitiveAlphaGain compRatio (gamma, omega, spendPct, oracleProb, bidderProb);
    
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

  // Check the probability distribution being used
  //  std::cout << std::endl << std::endl;
  //  for (int i=0; i<10; ++i) std::cout << i << " " << omega * spendPct * bidderProb(i,0) << "    ";
  //  std::cout << std::endl << std::endl;

  // alternate between reading and writing these matrices
  Matrix* pGainSrc;   Matrix* pGainDest = NULL;
  Matrix* pOracleSrc; Matrix* pOracleDest = NULL;
  Matrix* pBidderSrc; Matrix* pBidderDest = NULL;
  std::pair<double,double> bestMeanInterval(100,0);
  for (int round = nRounds; round > 0; --round)
  { if (use0)
    { pGainSrc    = &gain0;    pOracleSrc  = &oracle0;   pBidderSrc  = &bidder0;
      pGainDest   = &gain1;    pOracleDest = &oracle1;   pBidderDest = &bidder1;
    } else
    { pGainSrc    = &gain1;    pOracleSrc  = &oracle1;   pBidderSrc  = &bidder1;
      pGainDest   = &gain0;    pOracleDest = &oracle0;   pBidderDest = &bidder0;
    }
    use0 = !use0;
    if (writeDetails)
    { std::cout << "\n\n--------------- Round " << round << " comparative value source --------------------- " << std::endl << pGainSrc->topLeftCorner(round+1,round+1) << std::endl;
      std::cout << " --------------- Expert (top 2 rows) --------------------- " << std::endl << pOracleSrc->topLeftCorner(imin(2,round+1),round+1) << std::endl;
      std::cout << " --------------- Bidder (top 2 rows) --------------------- " << std::endl << pBidderSrc->topLeftCorner(imin(2,round+1),round+1) << std::endl;
    }
    for (int i=0; i<round; ++i)        // next round status of expert
    { for (int j=0; j<round; ++j)      //                      bidder
      { std::pair<double,double> maxPair;
	compRatio.set_delay (i, j, round, nRounds, (*pGainSrc)(0,0), (*pGainSrc)(i+1,0), (*pGainSrc)(0,j+1), (*pGainSrc)(i+1,j+1));
	maxPair = search.find_maximum(compRatio);        // mean, f(mean)
	if(maxPair.first < bestMeanInterval.first)       // monitor range of optimal means
	  bestMeanInterval.first = maxPair.first;
	else if (maxPair.first > bestMeanInterval.second)
	  bestMeanInterval.second = maxPair.first;
	double atZero = compRatio(0.0);
	if (maxPair.second < atZero)
	  maxPair = std::make_pair(0.0,atZero);
	mean(i,j) = maxPair.first;
	(*pGainDest)(i,j) = maxPair.second;
	(*pOracleDest)(i,j) = compRatio.value_to_oracle(maxPair.first, (*pOracleSrc)(0,0), (*pOracleSrc)(i+1,0), (*pOracleSrc)(0,j+1), (*pOracleSrc)(i+1,j+1));
	(*pBidderDest)(i,j) = compRatio.value_to_bidder(maxPair.first, (*pBidderSrc)(0,0), (*pBidderSrc)(i+1,0), (*pBidderSrc)(0,j+1), (*pBidderSrc)(i+1,j+1));
      }
    }
    if (writeDetails)
      std::cout << "\n\n---------------   MEAN  --------------------- " << std::endl << mean.topLeftCorner(round,round) << std::endl;
  }
  // write parms and final values to std io
  std::clog << "Interval for optimal means is [" << bestMeanInterval.first << "," << bestMeanInterval.second << std::endl;
  std::cout << "Constrained " << oracleProb << "  " << gamma             << " " << omega               << " " << nRounds             << " " << spendPct << " "
	    << searchInterval.first << " " << searchInterval.second << " " 
	    << (*pGainDest)(0,0) << " " << (*pOracleDest)(0,0) << " " << (*pBidderDest)(0,0) << std::endl;
}

 /////////////////////////////////  Constrained Expert  ///////////////////////////////////////


 void
 ConstrainedExpertCompetitiveAlphaGain::set_delay (int i, int j, int t, int nRounds, double v00, double vi0, double v0j, double vij)
 {
   mAlpha = mOmega             * mExpertDist(i,nRounds-t);  // no spending constraint for expert
   mBeta  = mOmega * mSpendPct * mBidderProb(j,nRounds-t);
   // std::cout << "Setting malpha to " << mAlpha << " and mBeta to " << mBeta << std::endl;
   mV00 = v00;
   mVi0 = vi0;
   mV0j = v0j;
   mVij = vij;
 }

 double
 ConstrainedExpertCompetitiveAlphaGain::operator()(double mu) const
 {
   double a = mAlpha;                 // a = optimal_alpha(mu, mOmega); 
   double ra = reject_prob(mu,a);
   double rb = reject_prob(mu,mBeta);
   double gain = (mOmega * ra - a) - mGamma * (mOmega * rb - mBeta) + ra * rb * mV00 + (1-ra) * (1-rb) * mVij + ra * (1-rb) * mV0j + (1-ra) * rb * mVi0;
   // mondo output
   // if (mu == 0.0) std::cout << "\n gain @ 0 = " << gain << " using  a,ra = " << a << "," << ra << "   b,rb = " << mBeta << "," << rb
   //      		      << "    v " << mV00 << " " << mVij << " " << mVi0 << " " << mV0j << std::endl;
   return gain;
 }


 double
 ConstrainedExpertCompetitiveAlphaGain::value_to_oracle(double mu, double v00, double vi0, double v0j, double vij) const
 {
   double value, ra, rb;
   if (mu < 0.00001)
   { ra = mAlpha;                          // 0 if unconstrained
     value = mAlpha * (mOmega-1.0);        // 0 if unconstrained
     rb = mBeta;
   }
   else
   { double a = mAlpha;                    // a=optimal_alpha(mu, mOmega);
     ra = reject_prob(mu,a);
     value = mOmega * ra - a ;
     rb = reject_prob(mu, mBeta);
   }
   return  value + ra * rb * v00 +  (1-ra) * rb * vi0 + ra * (1-rb) * v0j + (1-ra) * (1-rb) * vij;
 }


double
ConstrainedExpertCompetitiveAlphaGain::value_to_bidder(double mu, double v00, double vi0, double v0j, double vij) const
{
  double ra, rb;
  if (mu < 0.00001)
  { ra = mAlpha;                           //     would be zero if unconstrained
    rb = mBeta;
  }
  else
  { ra = reject_prob(mu,mAlpha);           //     ra = reject_prob(mu,optimal_alpha(mu, mOmega));
    rb = reject_prob(mu, mBeta);
  }
  return  (mOmega * rb - mBeta)  + ra * rb * v00 +  (1-ra) * rb * vi0 + ra * (1-rb) * v0j + (1-ra) * (1-rb) * vij;
}



// --------------------------------------------------------------------------------------------------------------
//
//      Unconstrained     Unconstrained     Unconstrained     Unconstrained     Unconstrained     Unconstrained 
//
// --------------------------------------------------------------------------------------------------------------

void
solve_bellman_alpha_equation (double gamma, double omega, int nRounds, double spendPct, ProbDist f, bool writeDetails)
{
  const int maxIterations (100);
  const double tolerance  (0.0001);
  const double grid       (0.5);
  const std::pair<double,double> searchInterval = std::make_pair(1.5,6.5);
  Line_Search::GoldenSection search(tolerance, searchInterval, grid, maxIterations);
  ExpertCompetitiveAlphaGain compRatio (gamma, omega, f, spendPct);
    
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
    int gammaInt (trunc(10 * gamma));
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
  // write parameters and final values to std io
  std::cout << "Unconstrained " << 0 << " " << gamma     << " " << omega       << " " << nRounds     << " " << spendPct << " "
	    << searchInterval.first << " " << searchInterval.second  << " " 
	    << gain(0,0) << " " << oracle(0,0) << " " << bidder(0,0) << std::endl;
}



/////////////////////////////////  Unconstrained Expert  ///////////////////////////////////////


double
ExpertCompetitiveAlphaGain::operator()(double mu) const
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
ExpertCompetitiveAlphaGain::value_to_oracle(double mu, double o0, double okp1) const 
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
ExpertCompetitiveAlphaGain::value_to_bidder (double mu, double b0, double bkp1) const
{
  double rb = (mu < 0.00001) ? mBetaK : reject_prob(mu,mBetaK);
  return mOmega * rb - mBetaK + rb * b0 + (1-rb) * bkp1;
}



//     WealthArray     WealthArray     WealthArray     WealthArray     WealthArray     WealthArray     WealthArray     WealthArray


std::pair<int, double>
WealthArray::new_position (int k0, double increase)  const // k0 is current position denoting current wealth
{
  double target = mWealth[k0] + increase;                  // 'wealth' is 'new wealth' > 'current wealth'
  int k1 (mWealth.max_index());                            // W[k0] <= W[k1]
  assert (target <= mWealth[k1]);                          // needs to be in range of table
  while (k0+1 < k1)                                        // bracket between k0 and k1
  { int kk = floor((k0+k1)/2);
    // std::cout << "DEBUG: new_position   W[" << k0 << "]="
    //           << mWealth[k0] << " W[" << kk << "]=" << mWealth[kk] << "  W[" << k1 << "]=" << mWealth[k1] << std::endl;
    if (target < mWealth[kk])
    { k1 = kk; }
    else
    { k0 = kk; }
  }
  if (k0<k1)  // inside range
  { double p ( (target - mWealth[k0]) / (mWealth[k1] - mWealth[k0]) );
    return std::make_pair(k0,1-p);
  }
  else
    return std::make_pair(k0,1);
}


void
WealthArray::initialize_array(int maxSteps, Tfunc p)
{
  int const excessWealthSteps = 4;
  std::cout << "DEBUG: Building dyn array with indices " << -maxSteps << " to " << excessWealthSteps << std::endl;
  DynamicArray<double> da(-maxSteps,excessWealthSteps);
  da.assign(0,mOmega);
  for(int i=-1; i >= -maxSteps; --i)   da.assign(i, da[i+1] - mOmega * p(-i) );
  da.assign(1, (mOmega * 4.0)/3.0 );
  da.assign(2, (mOmega * 5.0)/3.0 );
  da.assign(3,  mOmega * 2);
  da.assign(4,  mOmega * 3);
  mWealth = da;
}


//    solve_bellman_reject     solve_bellman_reject     solve_bellman_reject     solve_bellman_reject     solve_bellman_reject

void
solve_bellman_reject_equation (double gamma, double omega, int nRounds, double (*pdf)(int), bool writeDetails)
{
  const int                      maxIterations   (200);   
  const double                   tolerance       (0.0001);
  const double                   grid            (0.5);
  const std::pair<double,double> searchInterval  (std::make_pair(0.05,7.0));

  // pad wealth to allow for initial bid
  WealthArray bidderWealth("bidder", omega, nRounds+1, pdf);
  if (writeDetails) std::cout << bidderWealth << std::endl;
  
  Line_Search::GoldenSection search(tolerance, searchInterval, grid, maxIterations);
  RejectUtility utility (gamma, bidderWealth);

  // need to pad arrays since need room to collect bid
  int maxK = bidderWealth.max_index();
  int nStates = nRounds+maxK+1;
  Matrix utilityMat= Matrix::Zero(nRounds+1, nStates+2);   // extra row and 2 cols pad for boundary conditions
  Matrix oracleMat = Matrix::Zero(nRounds+1, nStates+2);
  Matrix bidderMat = Matrix::Zero(nRounds+1, nStates+2);
  Matrix meanMat   = Matrix::Zero(nRounds  , nStates  );

  //  initialize boundary conditions
  for (int j=0; j<nStates; ++j)
  { utilityMat(nRounds,j) = 0.0;
    oracleMat (nRounds,j) = 0.0;
    bidderMat (nRounds,j) = 0.0;
  }
  // stores intermediates in trapezoidal array; col 1 pads start
  std::pair<double,double> maxPair;
  int done = 1;
  int offset = bidderWealth.max_steps();
  for (int row = nRounds-1; row > -1; --row, ++done)
  { // double b0 = bidderMat(row+1,0);
    // double o0 = oracleMat(row+1,0);
    if (writeDetails) std::cout << "  t = " << row+1 << std::endl; 
    for (int col=done; col<nStates; ++col)
    { int k = col - nRounds;
      std::pair<int,double> kp (bidderWealth.new_position(k,0.05 - bidderWealth.bid(k)));   // k runs -max_steps to +4
      double valueIfReject = utilityMat(row+1,kp.first+offset)*kp.second + utilityMat(row+1,kp.first+1+offset)*(1-kp.second);
      utility.set_constants(bidderWealth.bid(k), valueIfReject, utilityMat(row+1,col-1));   // bid, value if reject, value if not reject
      double atZero = utility(0.0);
      maxPair = search.find_maximum(utility);
      if (maxPair.second < atZero)
	maxPair = std::make_pair(0.0,atZero);
      meanMat   (row,col) = maxPair.first;
      utilityMat(row,col) = maxPair.second;
      if (writeDetails) std::cout << "     @ " << k << " " << row << " kk= " << kp.first
				  << " { (" << valueIfReject << "=" << utilityMat(row+1,kp.first+offset)  << "*" << kp.second << " + "
				  << utilityMat(row+1,kp.first+offset+1) << "*" << 1-kp.second  << "), " << utilityMat(row+1,col-1)
				  << "}  with bid " << bidderWealth.bid(k) << "    max  : " << maxPair.second << " @ " << maxPair.first << std::endl;
      // oracleMat (row,col) = utility.value_to_oracle(maxPair.first, o0, oracle(row+1,col+1));
      // bidderMat (row,col) = utility.value_to_bidder(maxPair.first, b0, bidder(row+1,col+1));
    }
  }
  // write parameters and final values to std io
  if (writeDetails)
  { std::cout << "Write details\n" << std::endl;
    for (int row=0; row<utilityMat.rows(); ++row)
    { std::cout << "[" << row << "] ";
      for (int j=0; j<utilityMat.cols(); ++j)
	std::cout << utilityMat(row,j) << " ";
      std::cout << std::endl;
    }
  }
  std::cout << "Solve reject bellman " << 0 << " " << gamma     << " " << nRounds   << " "
	    << searchInterval.first << " " << searchInterval.second  << " " << utilityMat(0,0)  << std::endl;
}


//   RejectUtility     RejectUtility     RejectUtility     RejectUtility     RejectUtility     RejectUtility     

double
RejectUtility::operator()(double mu) const
{
  // std::cout << "DEBUG: utility calculation, mu=" << mu << "   omega=" << mOmega << "  betaK=" << mBetaK << std::endl;
  double ra = reject_prob(mu,mOmega);
  double rb = reject_prob(mu,mBeta );
  double util = ra - mGamma * rb  + rb * mRejectValue + (1-rb) * mNoRejectValue;
  return util;
}
