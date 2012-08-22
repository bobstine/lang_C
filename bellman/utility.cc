#include "utility.h"

#include "normal.h"

#include <utility> // pair


////////////////////////////////////  Utility functions  /////////////////////////////////////////

double maximumZ = 8.0;
double epsilon  = 1.0e-15;

double
reject_prob(double mu, double level)    // r_mu(alpha)
{
  if(level < epsilon)
    return 0.0;
  else
    return normal_cdf(mu-normal_quantile(1-level));
}

double
reject_value(int i, WIndex const& kp, Matrix const& value, bool show)
{
  const int    k (kp.first);
  const double p (kp.second);
  const double v (value(i,k)*p + value(i,k+1)*(1-p));
  if (show) std::cout << "   reject_value( i=" << i << ",kp=(" << k << "," << p << ") ) = " << value(i,k) << "*" << p << "+" << value(i,k+1) << "*" << (1-p) << "=" << v << std::endl;
  return v;
}

double
reject_value(WIndex const& ip, int k, Matrix const& value, bool show)
{
  const int    i (ip.first);
  const double p (ip.second);
  const double v (value(i,k)*p + value(i+1,k)*(1-p));
  if (show) std::cout << "   reject_value( ip=(" << i << "," << p << "),k=" << k << " ) = " << value(i,k) << "*" << p << "+" << value(i+1,k) << "*" << (1-p) << "=" << v << std::endl;
  return v;
}

double   reject_value(WIndex const& kp1, WIndex const& kp2, Matrix const& value,bool show)
{
  int    r  (kp1.first);
  double pr (kp1.second);
  int    c  (kp2.first);
  double pc (kp2.second);
  const double v (pr * ( pc * value(r,c) + (1-pc) * value(r,c+1) ) + (1-pr) * ( pc * value(r+1,c) + (1-pc) * value(r+1,c+1) ));
  if (show)  std::cout << "   reject_value( (" << r << "," << pr << "),(" << c << "," << pc << ") ) = "
		       << pr     << "*(" << pc << "*" << value(r  ,c) << "+" << 1-pc << "*" << value(r  ,c+1) << ")  +  "
		       << (1-pr) << "*(" << pc << "*" << value(r+1,c) << "+" << 1-pc << "*" << value(r+1,c+1) << ") = " << v << std::endl;
  return v;
}

double
z_alpha (double alpha)
{
  if (alpha < epsilon)
    return maximumZ;
  else
    return normal_quantile(1-alpha);
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

double
neg_risk(double mu, double alpha)
{
  return -risk(mu,alpha);
}

double
risk(double mu, double alpha)
{
  double ra, R;

  if (0 == alpha)
  { ra = alpha;
    R = 0.0;
  }
  else
  { ra = reject_prob(mu, alpha);
    R = (1.0 - ra) * mu*mu;
  }
  double dev = z_alpha(alpha) - mu;
  R += dev * normal_density(dev) + normal_cdf(-dev);
  return R;
}


// ------------------------------------------------------------------------------------------------------------
// -----  VectorUtility  -----  VectorUtility  -----  VectorUtility  -----  VectorUtility  -----  VectorUtility

double
VectorUtility::r_mu_beta (double mu) const
{
  return (0.0 == mu) ? mBeta : reject_prob(mu, mBeta);
}

double
VectorUtility::r_mu_alpha (double mu) const
{
  return (0.0 == mu) ? mAlpha : reject_prob(mu, mAlpha);
}

std::pair<double,double>
VectorUtility::reject_probabilities (double mu) const
{
  double ra,rb;
  if (mu == 0.0)
  { ra = mAlpha;
    rb = mBeta;
  }
  else
  { ra = reject_prob(mu,mAlpha);
    rb = reject_prob(mu,mBeta );
  }
  return std::make_pair(ra,rb);
}  

//   RejectUtility     RejectUtility     RejectUtility     RejectUtility     RejectUtility     RejectUtility     

double
RejectVectorUtility::operator()(double mu) const
{
  std::pair<double,double>  rprob  (reject_probabilities(mu));
  double rb (rprob.second);
  return mSin*rprob.first + mCos*rb  + rb * mRejectValue + (1-rb) * mNoRejectValue;
}

double
RejectVectorUtility::bidder_utility (double mu, double rejectValue, double noRejectValue) const
{
  double rb (r_mu_beta(mu));
  return rb  + rb * rejectValue + (1-rb) * noRejectValue;
}

double
RejectVectorUtility::oracle_utility (double mu, double rejectValue, double noRejectValue) const
{
  std::pair<double,double>  rejectProbs  (reject_probabilities(mu));
  double rb (rejectProbs.second);
  return rejectProbs.first + rb * rejectValue + (1-rb) * noRejectValue;
}


//    RiskUtility      RiskUtility      RiskUtility      RiskUtility      RiskUtility      RiskUtility      RiskUtility      

double
RiskVectorUtility::operator()(double mu) const
{
  std::pair<double,double>  rprob  (reject_probabilities(mu));
  double rb (rprob.second);
  return  mSin*neg_risk(mu,mAlpha) + mCos*neg_risk(mu,mBeta) + rb * mRejectValue + (1-rb) * mNoRejectValue;
}


double
RiskVectorUtility::bidder_utility (double mu, double rejectValue, double noRejectValue) const
{
  double rb (r_mu_beta(mu));
  return  neg_risk(mu,mBeta)  + rb * rejectValue + (1-rb) * noRejectValue;
}

double
RiskVectorUtility::oracle_utility (double mu, double rejectValue, double noRejectValue) const 
{
  std::pair<double,double>  rprob  (reject_probabilities(mu));
  double rb (rprob.second);
  return  neg_risk(mu,mAlpha) + rb * rejectValue + (1-rb) * noRejectValue;
}



// -------------------------------------------------------------------------------------------------------------
// -----  MatrixUtility  -----  MatrixUtility  -----  MatrixUtility  -----  MatrixUtility  -----  MatrixUtility


//
//   Note:  alpha is the first, beta is the second bid in the order supplied (not the math notation)
// 


double
MatrixUtility::r_mu_beta (double mu) const
{
  if (0.0 == mu)
    return mBeta;
  else
  { if (0.0 == mBeta)
      return 0.0;
    else
      return reject_prob(mu, mBeta);
  }
}

double
MatrixUtility::r_mu_alpha (double mu) const
{
  if (0.0 == mu)
    return mAlpha;
  else
  { if (0.0 == mAlpha)
      return 0.0;
    else
      return reject_prob(mu, mAlpha);
  }
}

std::pair<double,double>
MatrixUtility::reject_probabilities (double mu) const
{
  double ra,rb;
  if (mu == 0.0)
  { ra = mAlpha;
    rb = mBeta;
  }
  else
  { ra = (0.0 == mAlpha) ? 0.0 : reject_prob(mu,mAlpha);
    rb = (0.0 == mBeta ) ? 0.0 : reject_prob(mu,mBeta );
  }
  return std::make_pair(ra,rb);
}  


//   RejectMatrixUtility     RejectUtility     RejectUtility     RejectUtility     RejectUtility     RejectUtility     

double
RejectMatrixUtility::operator()(double mu) const
{
  std::pair<double,double>  rprob  (reject_probabilities(mu));
  double rAlpha (rprob.first);
  double rBeta (rprob.second);
  double util (mSin*rAlpha + mCos*rBeta);
  if (rAlpha > rBeta)
    return  util + mV00 * (1-rAlpha) + mV10 * (rAlpha-rBeta) +  mV11 * rBeta;
  else
    return  util + mV00 * (1- rBeta) + mV01 * (rBeta-rAlpha) +  mV11 * rAlpha;
}


double
RejectMatrixUtility::oracle_utility (double mu, double v00, double v01, double v10, double v11) const
{
  std::pair<double,double>  rprob  (reject_probabilities(mu));
  double rAlpha (rprob.first);
  double rBeta (rprob.second);
  if (rAlpha > rBeta)
    return  rAlpha + v00 * (1-rAlpha) + v10 * (rAlpha-rBeta) +  v11 * rBeta;
  else
    return  rAlpha + v00 * (1- rBeta) + v01 * (rBeta-rAlpha) +  v11 * rAlpha;
}


double
RejectMatrixUtility::bidder_utility (double mu, double v00, double v01, double v10, double v11) const
{
  // same recursive structure as oracle_utility, just different recursive values appear in call
  std::pair<double,double>  rprob  (reject_probabilities(mu));
  double rAlpha (rprob.first);
  double rBeta (rprob.second);
  if (rAlpha > rBeta)
    return  rBeta + v00 * (1-rAlpha) + v10 * (rAlpha-rBeta) +  v11 * rBeta;
  else
    return  rBeta + v00 * (1- rBeta) + v01 * (rBeta-rAlpha) +  v11 * rAlpha;
}




//    RiskUtility      RiskUtility      RiskUtility      RiskUtility      RiskUtility      RiskUtility      RiskUtility      

double
RiskMatrixUtility::operator()(double mu) const
{
  std::pair<double,double>  rprob  (reject_probabilities(mu));
  double rAlpha (rprob.first);
  double rBeta (rprob.second);
  double util (mSin*neg_risk(mu,mAlpha) + mCos*neg_risk(mu,mBeta));  
  if (rAlpha > rBeta)
    return  util + mV00 * (1-rAlpha) + mV10 * (rAlpha-rBeta) +  mV11 * rBeta;
  else
    return  util + mV00 * (1- rBeta) + mV01 * (rBeta-rAlpha) +  mV11 * rAlpha;
}



double
RiskMatrixUtility::oracle_utility  (double mu, double v00, double v01, double v10, double v11) const
{
  std::pair<double,double>  rprob  (reject_probabilities(mu));
  double rAlpha (rprob.first);
  double rBeta (rprob.second);
  if (rAlpha > rBeta)
    return  neg_risk(mu,mAlpha)  + v00 * (1-rAlpha) + v10 * (rAlpha-rBeta) +  v11 * rBeta;
  else
    return  neg_risk(mu,mAlpha)  + v00 * (1- rBeta) + v01 * (rBeta-rAlpha) +  v11 * rAlpha;
}


double
RiskMatrixUtility::bidder_utility (double mu, double v00, double v01, double v10, double v11) const
{
  std::pair<double,double>  rprob  (reject_probabilities(mu));
  double rAlpha (rprob.first);
  double rBeta (rprob.second);
  if (rAlpha > rBeta)
    return  neg_risk(mu,mBeta)  + v00 * (1-rAlpha) + v10 * (rAlpha-rBeta) +  v11 * rBeta;
  else
    return  neg_risk(mu,mBeta)  + v00 * (1- rBeta) + v01 * (rBeta-rAlpha) +  v11 * rAlpha;
}

