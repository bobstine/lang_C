#include <utility>         // pair
#include <functional>

/***********************************************************************************

  Bellman backward recursion for comptitive alpha-investing using
  several possible probability distributions, with spending percentage.

***********************************************************************************/

typedef double (*ProbDist)(int,int);

void
solve_bellman_equation             (double gamma, double omega, int nRounds, double spendPct, ProbDist f, bool writeDetails);

void
solve_constrained_bellman_equation (double gamma, double omega, int nRounds, double spendPct, ProbDist oracleProb, ProbDist bidderProb);




/**********************************************************************************

   Probability distributions that control spending

     All take 2 integer arguments (k since reject, j remaining)
     but some (like universal) don't use the second argument

 **********************************************************************************/

double universal (int k, int);

double geometric (int k, int);

double equal (int k, int left);         // equal spread over possible tests since reject



//  This guy does the optimization to find the best mu at given state
//  with no constraint, a real oracle.
//
//  This operator has one state item, k, the number of rounds since the
//  last time the bidder rejected.  The bidder is deterministic, controlled
//  by the input probablity distribution (defined above).

class ExpertCompetitiveGain: public std::unary_function<double,double>
{
 private:
  const double mGamma;
  const double mOmega;
  const ProbDist mProb;
  const double mSpendPct;
  double mBetaK;
  double mV0, mVkp1;
  
 public:

 ExpertCompetitiveGain(double gamma, double omega, ProbDist f, double spendPct)
   : mGamma(gamma), mOmega(omega), mProb(f), mSpendPct(spendPct), mBetaK(0.0) {}
  
  double beta_k (void) const { return mBetaK; }
  
  void set_k (int k, int left, double v0, double vkp1)
  { mBetaK = mOmega * mSpendPct * mProb(k,left);
    mV0 = v0; mVkp1 = vkp1;  }
  
  double operator()(double mu) const;
  double value_to_oracle (double mu, double o0, double okp1) const;
  double value_to_bidder (double mu, double b0, double bkp1) const;
}; 



//  This expert is constrained by a spending policy since its last
//  rejection, so its state depends on 2 things: the number of tests since
//  its last rejection and the number since the  bidder's rejection.

class ConstrainedExpertCompetitiveGain: public std::unary_function<double,double>
{
 private:
  const double mGamma;
  const double mOmega;
  const ProbDist mExpertProb, mBidderProb;
  const double mSpendPct;
  double mAlpha, mBeta;
  double mV00, mVi0, mVij, mV0j;
  
 public:

 ConstrainedExpertCompetitiveGain(double gamma, double omega, double spendPct, ProbDist expertP, ProbDist bidderP)
   : mGamma(gamma), mOmega(omega), mExpertProb(expertP), mBidderProb(bidderP), mSpendPct(spendPct), mAlpha(0.0), mBeta(0.0) {}

  double alpha (void) const { return mAlpha; }
  double beta  (void) const { return mBeta; }
  
  void set_delay (int i, int j, int t, int nRounds, double v00, double vi0, double v0j, double vij);
  
  double operator()(double mu) const;
  double value_to_oracle (double mu, double o00, double oi0, double o0j, double oij) const;
  double value_to_bidder (double mu, double b00, double bi0, double b0j, double bij) const;  
};
