#include <utility>         // pair
#include <functional>
#include <math.h>
#include <assert.h>

#include "dynamic_array.h"
#include <iostream>      // debug

/***********************************************************************************

  Bellman backward recursion for comptitive alpha-investing using
  several possible probability distributions, with spending percentage.

***********************************************************************************/

typedef double (*ProbDist)(int,int);
class WealthArray;


double universal      (int k);
inline  // for compability with the uniform to end 
double universal      (int k, int) { return universal(k); }

double geometric      (int k, int);
double uniform_to_end (int k, int left);


void
solve_bellman_reject_equation            (double gamma, double omega, int steps, double (*pdf)(int), bool writeDetails);


void
solve_bellman_alpha_equation             (double gamma, double omega, int nRounds, double spendPct, ProbDist f, bool writeDetails);

void
solve_constrained_bellman_alpha_equation (double gamma, double omega, int nRounds, double spendPct, double oracleGeoProb, ProbDist bidderProb, bool printDetails);


/**********************************************************************************

   Wealth tracker basically maps from integers that follow the tests
   to the wealth of the expert or bidder.  The wealth is monotone increasing
   in the index/position k.

   Normalized to have value omega at index 0.

 **********************************************************************************/


class WealthArray
{
  typedef double(*Tfunc)(int);
  
  const std::string     mName;
  const double          mOmega;      // defines wealth at position k=0 and determines how far 'up' wealth can go 
  DynamicArray<double>  mWealth;     // negative indices indicate wealth below omega

 public:
 WealthArray(std::string name, double omega, int maxSteps, Tfunc pdf)
   : mName(name), mOmega(omega), mWealth() { initialize_array(maxSteps, pdf); }      // extra 1 pads for initial bid

  int    max_steps ()                        const { return -mWealth.min_index(); }  //  pad for initial bid
  int    min_index ()                        const { return mWealth.min_index() ; }
  int    max_index ()                        const { return mWealth.max_index() ; }
  
  double bid(int k)                          const { return mWealth[k]-mWealth[k-1]; }
  double wealth(int k)                       const { return mWealth[k]; }
  double operator[](int k)                   const { return mWealth[k]; }
  
  std::pair<int, double>  new_position (int k, double increaseInWealth) const;
  
  void print_to (std::ostream& os) const { os << "Wealth array " << mName << "  " << mWealth; }
  
 private:
  void initialize_array(int steps, Tfunc p);
};

inline
std::ostream&
operator<< (std::ostream& os, WealthArray const& wa)
{
  wa.print_to(os);
  return os;
}



/**********************************************************************************

   Probability distributions that control spending

     All take 2 integer arguments (k since reject, j remaining)
     but some (like universal) don't use the second argument

 **********************************************************************************/

class GeometricDist: public std::binary_function<int,int,double>   // has flexible prob
{
  const double mP;
  const double mNorm;

  public:

  GeometricDist (double p): mP(p), mNorm((1-p)/p) { }

  double operator()(int k, int) const
  { double p=1;
    for (int i=0; i<=k; ++i) p *= mP;
    return p * mNorm;
  }

};
  


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
  const ProbDist mProb;
  const double mSpendPct;
  double mBetaK;
  double mV0, mVkp1;
  
 public:

 ExpertCompetitiveAlphaGain(double gamma, double omega, ProbDist f, double spendPct)
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

class ConstrainedExpertCompetitiveAlphaGain: public std::unary_function<double,double>
{
 private:
  const double mGamma;
  const double mOmega;
  const GeometricDist mExpertDist;
  const ProbDist mBidderProb;
  const double mSpendPct;
  double mAlpha, mBeta;
  double mV00, mVi0, mVij, mV0j;
  
 public:

 ConstrainedExpertCompetitiveAlphaGain(double gamma, double omega, double spendPct, double geoProb, ProbDist bidderP)
   : mGamma(gamma), mOmega(omega), mExpertDist(geoProb), mBidderProb(bidderP), mSpendPct(spendPct), mAlpha(0.0), mBeta(0.0) {}

  double alpha (void) const { return mAlpha; }
  double beta  (void) const { return mBeta; }
  
  void set_delay (int i, int j, int t, int nRounds, double v00, double vi0, double v0j, double vij);
  
  double operator()(double mu) const;
  double value_to_oracle (double mu, double o00, double oi0, double o0j, double oij) const;
  double value_to_bidder (double mu, double b00, double bi0, double b0j, double bij) const;  
};



////  Rejects     Rejects     Rejects     Rejects     Rejects     Rejects     Rejects     

class RejectUtility: public std::unary_function<double,double>
{
 private:
  const double mGamma;
  const double mOmega;
  const WealthArray mBidderWealth;
  double mBeta;
  double mRejectValue, mNoRejectValue;
  
 public:

 RejectUtility(double gamma, WealthArray wealth)
   : mGamma(gamma), mOmega(wealth[0]), mBidderWealth(wealth), mBeta(0.0), mRejectValue(0.0), mNoRejectValue(0.0) {}

  int    max_steps () const { return mBidderWealth.max_steps(); }
  double beta      () const { return mBeta; }
  
  void set_constants (double beta, double rejectValue, double noRejectValue)
  { assert((0 <= beta) && (beta <= 1.0));
    mBeta = beta;
    mRejectValue = rejectValue;
    mNoRejectValue = noRejectValue;
    // std::cout << "DEBUG:  setting beta to " << mBeta << " for k=" << bidderPos << std::endl;
  }
  
  double operator()(double mu) const;

}; 


