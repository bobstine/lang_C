#include <assert.h>

#include "utility.h"

#include <iostream>      // debug

/***********************************************************************************

  Bellman backward recursion for comptitive alpha-investing using
  several possible probability distributions, with spending percentage.

***********************************************************************************/


//  These use a discrete wealth array to track the wealth of the bidder and (in constrained case) the oracle.
//  Both use a convex mixture of states when new wealth is not element of the array
//  Note: It's evil to pass in the reference,  but we don't care that the utility is modifiable; its there to be used.

// all powerful oracle
void
solve_bellman_utility  (int nRounds, VectorUtility & util,                                  WealthArray const& bidderWealth, bool writeDetails);

// constrained oracle
void
solve_bellman_utility  (int nRounds, MatrixUtility & util, WealthArray const& oracleWealth, WealthArray const& bidderWealth, bool writeDetails);







// --- Earlier version with the alpha-investing utility, only spending down alpha-wealth
void
solve_bellman_alpha_equation             (double gamma, double omega, int nRounds, double spendPct, ProbDist const& f, bool writeDetails);

void
solve_constrained_bellman_alpha_equation (double gamma, double omega, int nRounds, double spendPct, double oracleGeoProb, ProbDist const& bidderProb, bool printDetails);


//  This guy does the optimization to find the best mu at given state
//  with no constraint, a real oracle.
//
//  This operator has one state item, k, the number of rounds since the
//  last time the bidder rejected.  The bidder is deterministic, controlled
//  by the input probablity distribution (defined above).

class ExpertCompetitiveAlphaGain: public std::unary_function<double,double>
{
 private:
  const double mGamma;
  const double mOmega;
  ProbDist const& mProb;
  const double mSpendPct;
  double mBetaK;
  double mV0, mVkp1;
  
 public:

 ExpertCompetitiveAlphaGain(double gamma, double omega, ProbDist const& f, double spendPct)
   : mGamma(gamma), mOmega(omega), mProb(f), mSpendPct(spendPct), mBetaK(0.0) {}
  
  double beta_k (void) const { return mBetaK; }
  
  void set_k (int k, int , double v0, double vkp1)  // ignore left item
  { mBetaK = mOmega * mSpendPct * mProb(k);
    mV0 = v0; mVkp1 = vkp1;  }
  
  double operator()(double mu) const;
  double value_to_oracle (double mu, double o0, double okp1) const;
  double value_to_bidder (double mu, double b0, double bkp1) const;
}; 

//  This expert is constrained by a spending policy since its last
//  rejection, so its state depends on 2 things: the number of tests since
//  its last rejection and the number since the  bidder's rejection.

class ConstrainedExpertCompetitiveAlphaGain: public std::unary_function<double,double>
{
 private:
  const double mGamma;
  const double mOmega;
  const GeometricDist mExpertDist;
  ProbDist const& mBidderProb;
  const double mSpendPct;
  double mAlpha, mBeta;
  double mV00, mVi0, mVij, mV0j;
  
 public:

 ConstrainedExpertCompetitiveAlphaGain(double gamma, double omega, double spendPct, double geoProb, ProbDist const& bidderP)
   : mGamma(gamma), mOmega(omega), mExpertDist(geoProb), mBidderProb(bidderP), mSpendPct(spendPct), mAlpha(0.0), mBeta(0.0) {}

  double alpha (void) const { return mAlpha; }
  double beta  (void) const { return mBeta; }
  
  void set_delay (int i, int j, int t, int nRounds, double v00, double vi0, double v0j, double vij);
  
  double operator()(double mu) const;
  double value_to_oracle (double mu, double o00, double oi0, double o0j, double oij) const;
  double value_to_bidder (double mu, double b00, double bi0, double b0j, double bij) const;  
};

