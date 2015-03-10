#ifndef _F_STATISTIC_
#define _F_STATISTIC_
#include "base_types.h"

#include <Eigen/Core>

class FStatistic
{
  typedef SCALAR  Scalar;
  typedef VECTOR  Vector;
  
private:
  Scalar   mF;
  int      mNumDF, mDenDF;
  Scalar   mPValue;
  Vector   mSSx;             // length mNumDF; used to decorate printed output
  
public:
  ~ FStatistic() {}
  
 FStatistic()                                                              // use empty version to signal singular
   : mF((Scalar)0.0), mNumDF(0), mDenDF(0), mPValue((Scalar)1.0), mSSx(Vector::Zero(1)) { }
  
 FStatistic(Scalar f, Scalar p, int numDF, int denDF, Vector const& ssx)   // use to return Bennett p-value
   : mF(f), mNumDF(numDF), mDenDF(denDF), mPValue( p ), mSSx(ssx) { assert(ssx.size()==numDF); }
  
 FStatistic(Scalar f, int numDF, int denDF, Vector const& ssx)
   : mF(f), mNumDF(numDF), mDenDF(denDF), mPValue(0.0), mSSx(ssx) { assert(ssx.size()==numDF); calc_p_value(); }
  
 FStatistic(Scalar numSS, int numDF, Scalar denSS, int denDF, Vector ssx)
   : mF((numSS/(Scalar)numDF)/(denSS/(Scalar)denDF)), mNumDF(numDF), mDenDF(denDF), mSSx(ssx) { assert(ssx.size()==numDF); calc_p_value(); }
  
  Scalar f_stat()                   const { return mF; }
  Scalar p_value()                  const { return mPValue; }
  Vector sum_of_squares()           const { return mSSx; }
  Scalar critical_value(Scalar p)   const;
  
  void   print_to(std::ostream& os) const { os << "F(" << mNumDF << "," << mDenDF << ") = " << mF << " (p=" << mPValue <<") { ss = " << mSSx.transpose() << " } "; }
  
private:
  void calc_p_value(); 
};


inline
std::ostream&
operator<<(std::ostream& os, FStatistic const& fStat)
{
  fStat.print_to(os);
  return os;
}

#endif
