#ifndef _F_STATISTIC_
#define _F_STATISTIC_

#include <Eigen/Array>

class FStatistic
{
  typedef Eigen::VectorXd Vector;
  
private:
  double   mF;
  int      mNumDF, mDenDF;
  double   mPValue;
  Vector   mSSx;             // length mNumDF; used to decorate printed output
  
public:
  ~ FStatistic() {}
  
 FStatistic()                                                              // use empty version to signal singular
   : mF(0.0), mNumDF(0), mDenDF(0), mPValue(1.0), mSSx(Vector::Zero(1)) { }
  
 FStatistic(double f, double p, int numDF, int denDF, Vector const& ssx)   // use to return Bennett p-value
   : mF(f), mNumDF(numDF), mDenDF(denDF), mPValue( p ), mSSx(ssx) { assert(ssx.size()==numDF); }
  
 FStatistic(double f, int numDF, int denDF, Vector const& ssx)
   : mF(f), mNumDF(numDF), mDenDF(denDF), mPValue(0.0), mSSx(ssx) { assert(ssx.size()==numDF); calc_p_value(); }
  
 FStatistic(double numSS, int numDF, double denSS, int denDF, Vector ssx)
   : mF((numSS/(double)numDF)/(denSS/(double)denDF)), mNumDF(numDF), mDenDF(denDF), mSSx(ssx) { assert(ssx.size()==numDF); calc_p_value(); }
  
  double f_stat()                   const { return mF; }
  double p_value()                  const { return mPValue; }
  Vector sum_of_squares()           const { return mSSx; }
  double critical_value(double p)   const;
  
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
