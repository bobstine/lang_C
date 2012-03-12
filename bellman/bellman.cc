#include "bellman.h"
#include "line_search.Template.h"

#include <Eigen/Core>
#include <math.h>
#include <functional>
#include <iostream>
#include <iomanip>
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

int
imin(int a, int b)
{ if (a < b) return a; else return b; }



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
 ConstrainedExpertCompetitiveAlphaGain::set_delay (int i, int j, int /* t */, int /* nRounds */, double v00, double vi0, double v0j, double vij)
 {
   mAlpha = mOmega             * mExpertDist(i /*,nRounds-t*/ );  // no spending constraint for expert
   mBeta  = mOmega * mSpendPct * mBidderProb(j /*,nRounds-t*/ );
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



//    solve_bellman_utility     solve_bellman_utility     solve_bellman_utility     solve_bellman_utility     

void
solve_bellman_utility  (double gamma, double omega, int nRounds, Utility & utility, ProbDist pdf, bool writeDetails)
{
  // initialize: iZero is omega location, iZero+6 gives five states above omega
  const int iZero    (nRounds+1);   
  const int nColumns (iZero + 6);   
  WealthArray bidderWealth("bidder", nColumns, omega, iZero, pdf);
  if (writeDetails) std::cout << bidderWealth << std::endl;
  // line search to find max utility
  const int                      maxIterations   (200);   
  const double                   tolerance       (0.0001);
  const double                   initialGrid     (0.5);
  const std::pair<double,double> searchInterval  (std::make_pair(0.05,7.0));
  Line_Search::GoldenSection search(tolerance, searchInterval, initialGrid, maxIterations);
  // pad arrays since need room to collect bid; initialize to zero
  Matrix utilityMat= Matrix::Zero(nRounds+2, nColumns);   // extra 2 rows for start, stop
  Matrix oracleMat = Matrix::Zero(nRounds+2, nColumns);
  Matrix bidderMat = Matrix::Zero(nRounds+2, nColumns);
  Matrix meanMat   = Matrix::Zero(nRounds+1, nColumns );
  // store intermediates in trapezoidal array; done=1 pads start; fill from bottom up
  std::pair<double,double> maxPair;
  double bid, utilAtMuEqualZero;
  int done = 1;
  for (int row = nRounds; row > -1; --row, ++done)
  { // double b0 = bidderMat(row+1,0);
    // double o0 = oracleMat(row+1,0);
    // -1 leaves room to avoid if clause
    for (int k=done; k<nColumns-1; ++k)  
    { bid = bidderWealth.bid(k);
      std::pair<int,double> kp (bidderWealth.new_wealth_position(k,omega - bid));
      double utilityIfReject = utilityMat(row+1,kp.first)*kp.second + utilityMat(row+1,kp.first+1)*(1-kp.second);
      double bidderIfReject  =  bidderMat(row+1,kp.first)*kp.second +  bidderMat(row+1,kp.first+1)*(1-kp.second);
      double oracleIfReject  =  oracleMat(row+1,kp.first)*kp.second +  oracleMat(row+1,kp.first+1)*(1-kp.second);
      utility.set_constants(bid, utilityIfReject, utilityMat(row+1,k-1));   // last is util if not reject
      utilAtMuEqualZero = utility(0.0);
      maxPair = search.find_maximum(utility);
      if (maxPair.second < utilAtMuEqualZero)
	maxPair = std::make_pair(0.0,utilAtMuEqualZero);
      meanMat   (row,k) = maxPair.first;
      utilityMat(row,k) = maxPair.second;
      bidderMat (row,k) = utility.bidder_utility(maxPair.first, bidderIfReject, bidderMat(row+1,k-1));
      oracleMat (row,k) = utility.oracle_utility(maxPair.first, oracleIfReject, oracleMat(row+1,k-1));
      if (false) std::cout << "     @ " << k << " " << row << " kk= " << kp.first << " { (" << utilityIfReject << "="
			   << utilityMat(row+1,kp.first  ) << "*" <<   kp.second << " + "
			   << utilityMat(row+1,kp.first+1) << "*" << 1-kp.second  << "), " << utilityMat(row+1,k-1)
			   << "}  with bid " << bidderWealth.bid(k) << "    max  : " << maxPair.second << " @ " << maxPair.first << std::endl;
    }
  }
  // write solution (without boundary row) to file
  if(writeDetails)
  { std::string fileName;
    std::ostringstream ss;
    int gammaInt (trunc(10 * gamma));
    ss << "bellman.g" << gammaInt << ".n" << nRounds << ".";
    fileName  = ss.str() + "utility";
    write_matrix_to_file(fileName, utilityMat.topLeftCorner(nRounds+1, utilityMat.cols()-1));  // omit boundary row, col
    fileName = ss.str() + "oracle";
    write_matrix_to_file(fileName,  oracleMat.topLeftCorner(nRounds+1, oracleMat.cols()-1));
    fileName = ss.str() + "bidder"; 
    write_matrix_to_file(fileName,  bidderMat.topLeftCorner(nRounds+1, bidderMat.cols()-1));
    fileName = ss.str() + "mean";
    write_matrix_to_file(fileName,    meanMat.topLeftCorner(nRounds+1, meanMat.cols()));
  }
  std::cout << gamma << " " << omega << "   " << nRounds   << "   " << searchInterval.first << " " << searchInterval.second  << "     "
	    << utilityMat(0,nRounds+1) << " " << oracleMat(0,nRounds+1) << " " << bidderMat(0,nRounds+1) << std::endl;
}



//    solve_bellman_utility  2  solve_bellman_utility  2  solve_bellman_utility  2  solve_bellman_utility  2

void
solve_bellman_utility  (double gamma, double omega, int nRounds, Utility & utility, ProbDist oraclePDF, ProbDist bidderPDF, bool writeDetails)
{
  // initialize: iZero is omega location, iZero+6 gives five states above omega
  const int iZero    (nRounds+1);   
  const int nColumns (iZero + 6);   
  WealthArray bidderWealth("bidder", nColumns, omega, iZero, bidderPDF);
  WealthArray oracleWealth("oracle", nColumns, omega, iZero, oraclePDF);
  if (writeDetails) std::cout << bidderWealth << std::endl;
  // line search to find max utility
  const int                      maxIterations   (200);   
  const double                   tolerance       (0.0001);
  const double                   initialGrid     (0.5);
  const std::pair<double,double> searchInterval  (std::make_pair(0.50,7.0));
  Line_Search::GoldenSection search(tolerance, searchInterval, initialGrid, maxIterations);
  // pad arrays since need room to collect bid; initialize to zero
  // code flips between these on read and write
  Matrix utilityMat0= Matrix::Zero (nColumns, nColumns);
  Matrix oracleMat0 = Matrix::Zero (nColumns, nColumns);
  Matrix bidderMat0 = Matrix::Zero (nColumns, nColumns);
  Matrix utilityMat1= Matrix::Zero (nColumns, nColumns);
  Matrix oracleMat1 = Matrix::Zero (nColumns, nColumns);
  Matrix bidderMat1 = Matrix::Zero (nColumns, nColumns);
  Matrix meanMat    = Matrix::Zero (nRounds, nRounds);
  bool use0 = true;
  // alternate between reading and writing these matrices
  Matrix* pUtililtySrc;   Matrix* pUtilityDest = NULL;
  Matrix* pOracleSrc;     Matrix* pOracleDest = NULL;
  Matrix* pBidderSrc;     Matrix* pBidderDest = NULL;
  std::pair<double,double> maxPair;
  double oracleBid,bidderBid, utilAtMuEqualZero, bidderRejectValue, oracleRejectP;
  int bidderRejectK, oracleRejectK;
  int done = 1;
  for (int round = nRounds; round > 0; --round, ++done)
  { if (use0)
    { pUtilitySrc = &utilityMat0;    pOracleSrc  = &oracleMat0;   pBidderSrc  = &bidderMat0;
      pUtilityDest= &utilityMat1;    pOracleDest = &oracleMat1;   pBidderDest = &bidderMat1;
    } else
    { pUtilitySrc = &utilityMat1;    pOracleSrc  = &oracleMat1;   pBidderSrc  = &bidderMat1;
      pUtilityDest= &utilityMat0;    pOracleDest = &oracleMat0;   pBidderDest = &bidderMat0;
    }
    use0 = !use0;

    for (int kb=done; kb<nColumns-1; ++kb)        //               bidder wealth array position on rows
    { bidderBid= bidderWealth.bid(kb);
      std::pair<int,double> bidderKP (bidderWealth.new_wealth_position(kb,omega - bidderBid));
      for (int ko=done; ko<nColumns-1; ++ko)      //               oracle on columns of matrix
      { oracleBid = oracleWealth.bid(ko);
	std::pair<int,double> oracleKP (oracleWealth.new_wealth_position(ko,omega - oracleBid));
	double utilityIfReject = reject_value(bidderKP, oracleKP, pUtilitySrc)
	utility.set_constants(oracleBid, bid, utilityIfReject, utilityMat(row+1,k-1));   // last is util if not reject
      
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



      double bidderIfReject  =  bidderMat(row+1,kp.first)*kp.second +  bidderMat(row+1,kp.first+1)*(1-kp.second);
      double oracleIfReject  =  oracleMat(row+1,kp.first)*kp.second +  oracleMat(row+1,kp.first+1)*(1-kp.second);
      utilAtMuEqualZero = utility(0.0);
      maxPair = search.find_maximum(utility);
      if (maxPair.second < utilAtMuEqualZero)
	maxPair = std::make_pair(0.0,utilAtMuEqualZero);
      meanMat   (row,k) = maxPair.first;
      utilityMat(row,k) = maxPair.second;
      bidderMat (row,k) = utility.bidder_utility(maxPair.first, bidderIfReject, bidderMat(row+1,k-1));
      oracleMat (row,k) = utility.oracle_utility(maxPair.first, oracleIfReject, oracleMat(row+1,k-1));


      
      if (false) std::cout << "     @ " << k << " " << row << " kk= " << kp.first << " { (" << utilityIfReject << "="
			   << utilityMat(row+1,kp.first  ) << "*" <<   kp.second << " + "
			   << utilityMat(row+1,kp.first+1) << "*" << 1-kp.second  << "), " << utilityMat(row+1,k-1)
			   << "}  with bid " << bidderWealth.bid(k) << "    max  : " << maxPair.second << " @ " << maxPair.first << std::endl;
    }
  }
  // write solution (without boundary row) to file
  if(writeDetails)
  { std::string fileName;
    std::ostringstream ss;
    int gammaInt (trunc(10 * gamma));
    ss << "bellman.g" << gammaInt << ".n" << nRounds << ".";
    fileName  = ss.str() + "utility";
    write_matrix_to_file(fileName, utilityMat.topLeftCorner(nRounds+1, utilityMat.cols()-1));  // omit boundary row, col
    fileName = ss.str() + "oracle";
    write_matrix_to_file(fileName,  oracleMat.topLeftCorner(nRounds+1, oracleMat.cols()-1));
    fileName = ss.str() + "bidder"; 
    write_matrix_to_file(fileName,  bidderMat.topLeftCorner(nRounds+1, bidderMat.cols()-1));
    fileName = ss.str() + "mean";
    write_matrix_to_file(fileName,    meanMat.topLeftCorner(nRounds+1, meanMat.cols()));
  }
  std::cout << gamma << " " << omega << "   " << nRounds   << "   " << searchInterval.first << " " << searchInterval.second  << "     "
	    << utilityMat(0,nRounds+1) << " " << oracleMat(0,nRounds+1) << " " << bidderMat(0,nRounds+1) << std::endl;
}

//     Utility     Utility     Utility     Utility     Utility     Utility     Utility     Utility     Utility
