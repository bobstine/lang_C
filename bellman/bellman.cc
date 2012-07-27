#include "bellman.h"
#include "line_search.Template.h"
#include "eigen_utils.h"


#include <math.h>
#include <functional>
#include <iostream>
#include <iomanip>

int
imin(int a, int b)
{ if (a < b) return a; else return b; }


//
//    Unconstrained   Unconstrained   Unconstrained   Unconstrained   Unconstrained   Unconstrained
//
//    solve_bellman_utility     solve_bellman_utility     solve_bellman_utility     solve_bellman_utility     
//

void
solve_bellman_utility  (int nRounds, VectorUtility & utility, WealthArray const& bidderWealth, bool writeDetails)
{
  // initialize: iOmega is omega location, iOmega+6 gives five states above omega
  const int nColumns (bidderWealth.size());   

  // line search to max utility (or min risk)
  const int                      maxIterations   (200);   
  const double                   tolerance       (0.0001);
  const double                   initialGrid     (0.5);
  const std::pair<double,double> searchInterval  (std::make_pair(0.05,10.0));
  Line_Search::GoldenSection     search(tolerance, searchInterval, initialGrid, maxIterations);
  // pad arrays since need room to collect bid; initialize to zero
  Matrix utilityMat= Matrix::Zero(nRounds+2, nColumns);   // extra 2 rows for start, stop
  Matrix oracleMat = Matrix::Zero(nRounds+2, nColumns);
  Matrix bidderMat = Matrix::Zero(nRounds+2, nColumns);
  Matrix meanMat   = Matrix::Zero(nRounds+1, nColumns );
  // store intermediates in trapezoidal array; done=1 pads start; fill from bottom up
  int done = 1;
  for (int row = nRounds; row > -1; --row, ++done)
  { for (int k=done; k<nColumns-1; ++k)    // -1 leaves room to avoid if clause
    { double bid (bidderWealth.bid(k));
      std::pair<int,double> kp (bidderWealth.wealth_position(k));
      double utilityIfReject = utilityMat(row+1,kp.first)*kp.second + utilityMat(row+1,kp.first+1)*(1-kp.second);
      double bidderIfReject  =  bidderMat(row+1,kp.first)*kp.second +  bidderMat(row+1,kp.first+1)*(1-kp.second);
      double oracleIfReject  =  oracleMat(row+1,kp.first)*kp.second +  oracleMat(row+1,kp.first+1)*(1-kp.second);
      utility.set_constants(bid, utilityIfReject, utilityMat(row+1,k-1));   // last is util if not reject
      std::pair<double,double> maxPair (search.find_maximum(utility));
      double utilAtMuEqualZero (utility(0.0));
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
  { std::ostringstream ss;
    int gammaInt (trunc(10 * utility.gamma()));
    ss << "bellman.g" << gammaInt << ".n" << nRounds << ".";
    write_matrix_to_file(ss.str() + "utility", utilityMat.topLeftCorner(nRounds+1, utilityMat.cols()-1));  // omit boundary row, col
    write_matrix_to_file(ss.str() + "oracle" ,  oracleMat.topLeftCorner(nRounds+1, oracleMat.cols()-1));
    write_matrix_to_file(ss.str() + "bidder" ,  bidderMat.topLeftCorner(nRounds+1, bidderMat.cols()-1));
    write_matrix_to_file(ss.str() + "mean"   ,    meanMat.topLeftCorner(nRounds+1, meanMat.cols()));
  }
  std::cout << utility.gamma() << " " << bidderWealth.omega() << "   " << nRounds   << "   " << searchInterval.first << " " << searchInterval.second  << "     "
	    << utilityMat(0,nRounds+1) << " " << oracleMat(0,nRounds+1) << " " << bidderMat(0,nRounds+1) << std::endl;
}



//    solve_bellman_utility  2  solve_bellman_utility  2  solve_bellman_utility  2  solve_bellman_utility  2

void
solve_bellman_utility  (int nRounds, MatrixUtility & utility, WealthArray const& oracleWealth, WealthArray const& bidderWealth, bool writeDetails)
{
  // initialize: omega location, size includes padding for wealth above omega
  const int iOmega   (nRounds + 1);   
  const int nColumns (bidderWealth.size());   
  // line search to find max utility
  const int                      maxIterations   (200);   
  const double                   tolerance       (0.0001);
  const double                   initialGrid     (0.5);
  const std::pair<double,double> searchInterval  (std::make_pair(0.05,10.0));
  Line_Search::GoldenSection search(tolerance, searchInterval, initialGrid, maxIterations);
  // pad arrays since need room to collect bid; initialize to zero
  // code flips between these on read and write using use0
  bool use0 = true;
  Matrix utilityMat0= Matrix::Zero (nColumns, nColumns);
  Matrix utilityMat1= Matrix::Zero (nColumns, nColumns);
  Matrix oracleMat0 = Matrix::Zero (nColumns, nColumns);
  Matrix oracleMat1 = Matrix::Zero (nColumns, nColumns);
  Matrix bidderMat0 = Matrix::Zero (nColumns, nColumns);
  Matrix bidderMat1 = Matrix::Zero (nColumns, nColumns);
  // save two slices of the 'pyramid' of optimal means picked by oracle
  // arrays to save mean chosen by oracle at two fixed wealths identified mIndexA and mIndexB
  // top row A holds bid, second row the wealth for oracle, with specific bid in last col
  // top row B holds information for the bidder
  const int mIndexA = iOmega -  1;
  const int mIndexB = iOmega - imin(50,iOmega/2);  // less wealth
  Matrix meanMatA    = Matrix::Zero (nRounds+2, nColumns);
  Matrix meanMatB    = Matrix::Zero (nRounds+2, nColumns);
  for (int col=0; col<nColumns-1; ++col)
  { meanMatA(0,col) = oracleWealth.bid(col);
    meanMatA(1,col) = oracleWealth[col];
    meanMatB(0,col) = bidderWealth.bid(col);
    meanMatB(1,col) = bidderWealth[col];
  }
  meanMatA(0,nColumns-1) = oracleWealth.bid(mIndexA);
  meanMatB(0,nColumns-1) = oracleWealth.bid(mIndexB);
  // alternate between reading and writing these matrices
  Matrix* pUtilitySrc (&utilityMat0), * pUtilityDest (&utilityMat1);
  Matrix* pOracleSrc  (&oracleMat0 ), * pOracleDest  (&oracleMat1);
  Matrix* pBidderSrc  (&bidderMat0 ), * pBidderDest  (&bidderMat1);
  // iteration vars
  std::pair<double,double> maxPair, bestMeanInterval;
  std::pair<int, double> bidderKP, oracleKP;
  double oracleBid,bidderBid;
  bestMeanInterval = std::make_pair(10,0);
  int done = 1;
  std::cout << std::setprecision(3);  // debug
  for (int round = nRounds; round > 0; --round, ++done)
  { if (use0)
    { pUtilitySrc = &utilityMat0;    pOracleSrc  = &oracleMat0;   pBidderSrc  = &bidderMat0;
      pUtilityDest= &utilityMat1;    pOracleDest = &oracleMat1;   pBidderDest = &bidderMat1;
    } else
    { pUtilitySrc = &utilityMat1;    pOracleSrc  = &oracleMat1;   pBidderSrc  = &bidderMat1;
      pUtilityDest= &utilityMat0;    pOracleDest = &oracleMat0;   pBidderDest = &bidderMat0;
    }
    use0 = !use0;
    //    std::cout << " ---------  Round " << round << " ----------------------\n";
    // recursion is in 'reverse time' starting from final round, working forward
    for (int ko=done; ko<nColumns-1; ++ko) 
    { oracleBid = oracleWealth.bid(ko);
      oracleKP  = oracleWealth.wealth_position(ko);         // position if rejects
      for (int kb=done; kb<nColumns-1; ++kb) 
      { bidderBid = bidderWealth.bid(kb);
	bidderKP  = bidderWealth.wealth_position(kb);
	utility.set_constants(oracleBid, bidderBid,                               // oracle is alpha, bidder is beta;  bidder position on cols
			      (*pUtilitySrc)(ko-1    , kb-1     ),                // v00  neither rejects
			      reject_value  (ko-1    , bidderKP, *pUtilitySrc),   // v01  only bidder rejects
			      reject_value  (oracleKP, kb-1    , *pUtilitySrc),   // v10  only oracle rejects
			      reject_value  (oracleKP, bidderKP, *pUtilitySrc));  // v11  both reject
	maxPair = search.find_maximum(utility);       // returns opt, f(opt)
	// monitor range of optimal means
	if(maxPair.first < bestMeanInterval.first)
	  bestMeanInterval.first = maxPair.first;
	else if (maxPair.first > bestMeanInterval.second)
	  bestMeanInterval.second = maxPair.first;
	double utilAtMuEqualZero = utility(0.0);
	if (maxPair.second < utilAtMuEqualZero)
	  maxPair = std::make_pair(0.0,utilAtMuEqualZero);
	// save mean if oracle in desired wealth state
	if      (mIndexA == ko) meanMatA(round+1,kb) = maxPair.first;
	else if (mIndexB == ko) meanMatB(round+1,kb) = maxPair.first; 
	// oracle on rows of outcome, bidder on columns
	(*pUtilityDest)(ko,kb) = maxPair.second;
	(* pOracleDest)(ko,kb) = utility.oracle_utility(maxPair.first,
							(*pOracleSrc)(ko-1    , kb-1   ),
							reject_value (ko-1    , bidderKP, *pOracleSrc),
							reject_value (oracleKP, kb-1    , *pOracleSrc),
							reject_value (oracleKP, bidderKP, *pOracleSrc));
	(* pBidderDest)(ko,kb) = utility.bidder_utility(maxPair.first,
							(*pBidderSrc)(ko-1    , kb-1   ),
							reject_value (ko-1    , bidderKP, *pBidderSrc),
							reject_value (oracleKP, kb-1    , *pBidderSrc),
							reject_value (oracleKP, bidderKP, *pBidderSrc));
	    /* huge debugging output... */
	    /* std::cout << "\n " << "alpha=" << utility.alpha() << "  beta=" << utility.beta()
	       << "  Oracle value is " << (*pOracleDest)(ko,kb) << "= Util(" << maxPair.first << " , "
	       << (*pOracleSrc)(ko-1    , kb-1   ) << " , "
	       << reject_value (ko-1    , bidderKP, *pOracleSrc) << " , "
	       << reject_value (oracleKP, kb-1    , *pOracleSrc) << " , "
	       << reject_value (oracleKP, bidderKP, *pOracleSrc) 
	       << ")   Bidder value is " << (*pBidderDest)(ko,kb) << "= Util(" << maxPair.first << " , "
	       << (*pBidderSrc)(ko-1    , kb-1   ) << " , "
	       << reject_value (ko-1    , bidderKP, *pBidderSrc) << " , "
	       << reject_value (oracleKP, kb-1    , *pBidderSrc) << " , "
	       << reject_value (oracleKP, bidderKP, *pBidderSrc) << ")" << std::endl;  */
	    // partial debugging output (pick up next two line feeds as well 
	    //	std::cout << "  [" << ko << "," << kb << "] " << std::setw(5) << (*pUtilityDest)(ko,kb) << "=" << (*pOracleDest)(ko,kb) << "- gamma * " << (*pBidderDest)(ko,kb)
	    // << "{" << oracleBid << "," << bidderBid << "," << maxPair.first << "," << maxPair.second << "}";
	}
	// std::cout << std::endl;
    }
    // std::cout << std::endl;
  }
  std::cout << std::setprecision(6);
  if(writeDetails)
  { std::ostringstream ss;
    int gammaInt (trunc(10 * utility.gamma()));
    ss << "runs/bellman2.g" << gammaInt << ".n" << nRounds << ".";
    write_matrix_to_file(ss.str() + "meanA"  ,    meanMatA  );
    write_matrix_to_file(ss.str() + "meanB"  ,    meanMatB  );
    bool writeOneRow = false;
    if (!writeOneRow) // write final destination matrices
    { write_matrix_to_file(ss.str() + "utility", *pUtilityDest);
      write_matrix_to_file(ss.str() + "oracle" ,  *pOracleDest);
      write_matrix_to_file(ss.str() + "bidder" ,  *pBidderDest);
    }
    else // write omega row of each to summary file
    { std::string fileName (ss.str() + "summary");
      write_vector_to_file(fileName, pUtilityDest->row(iOmega));
      write_vector_to_file(fileName,  pOracleDest->row(iOmega), true);  // append 
      write_vector_to_file(fileName,  pBidderDest->row(iOmega), true);
    }
  }
  // write summary of configuration and results to stdio
  std::cout << utility.gamma() << " " << bidderWealth.omega() << "   " << nRounds   << "   " << searchInterval.first << " " << searchInterval.second  << "     "
	    << (*pUtilityDest)(nRounds,nRounds) << " " << (*pOracleDest)(nRounds,nRounds) << " " << (*pBidderDest)(nRounds,nRounds) << std::endl;
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
solve_constrained_bellman_alpha_equation (double gamma, double omega, int nRounds, double spendPct, double oracleProb, ProbDist const& bidderProb, bool writeDetails)
{
  const int maxIterations (200);   
  const double tolerance  (0.0001);
  const double grid       (0.5);
  const std::pair<double,double> searchInterval = std::make_pair(0.05,10.0);
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
	maxPair = search.find_minimum(compRatio);        // mean, f(mean)
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
  std::cout << gamma             << " " << omega               << " " << nRounds             << " " << spendPct << " "
	    << searchInterval.first << " " << searchInterval.second << " " 
	    << (*pGainDest)(0,0) << " " << (*pOracleDest)(0,0) << " " << (*pBidderDest)(0,0) << std::endl;
}

 /////////////////////////////////  Constrained Expert  ///////////////////////////////////////


 void
 ConstrainedExpertCompetitiveAlphaGain::set_delay (int i, int j, int /* t */, int /* nRounds */, double v00, double vi0, double v0j, double vij)
 {
   mAlpha = mOmega             * mExpertDist(i /*,nRounds-t*/ );  // no spending constraint for expert
   mBeta  = mOmega * mSpendPct * mBidderProb(j /*,nRounds-t*/ );
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


// this version does alpha-wealth rather than input utility
void
solve_bellman_alpha_equation (double gamma, double omega, int nRounds, double spendPct, ProbDist const& f, bool writeDetails)
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
      maxPair = search.find_minimum(compRatio);
      if (maxPair.second < atZero)
	maxPair = std::make_pair(0.0,atZero);
      gain  (row,col) = maxPair.second;
      oracle(row,col) = compRatio.value_to_oracle(maxPair.first, o0, oracle(row+1,col+1));
      bidder(row,col) = compRatio.value_to_bidder(maxPair.first, b0, bidder(row+1,col+1));
      mean (row,col) = maxPair.first;
    }
  }
  // write solution (without boundary row) to file
  if(writeDetails)
  { std::string fileName;
    std::ostringstream ss;
    int gammaInt (trunc(10 * gamma));
    ss << "bellman.g" << gammaInt << ".n" << nRounds << ".";
    write_matrix_to_file(ss.str() + "gain",     gain.topLeftCorner(gain.rows()-1, gain.rows()-1));  // omit boundary row
    write_matrix_to_file(ss.str() + "oracle", oracle.topLeftCorner(oracle.rows()-1,oracle.rows()-1));
    write_matrix_to_file(ss.str() + "bidder", bidder.topLeftCorner(bidder.rows()-1, bidder.rows()-1));
    write_matrix_to_file(ss.str() + "mu",       mean.topLeftCorner(mean.rows(), mean.rows()));
  }
  // write parameters and final values to std io
  std::cout <<  gamma     << " " << omega       << " " << nRounds     << " " << spendPct << " "
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

