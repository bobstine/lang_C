#include "normal.h"


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
reject_value(int i, std::pair<int,double> const& kp, Matrix const& value)
{
  const int    k (kp.first);
  const double p (kp.second);
  return value(i,k)*p + value(i,k+1)*(1-p);
}

double
reject_value(std::pair<int,double> const& kp, int j, Matrix const& value)
{
  const int    k (kp.first);
  const double p (kp.second);
  return value(k,j)*p + value(k+1,j)*(1-p);
}


double
z_alpha (double a)
{
  return normal_quantile(1-a);
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
  return rprob.first - mGamma * rb  + rb * mRejectValue + (1-rb) * mNoRejectValue;
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
  return negative_risk(mu,mAlpha) - mGamma * negative_risk(mu,mBeta) + rb * mRejectValue + (1-rb) * mNoRejectValue;
}


double
RiskVectorUtility::bidder_utility (double mu, double rejectValue, double noRejectValue) const
{
  double rb (r_mu_beta(mu));
  return negative_risk(mu,mBeta)  + rb * rejectValue + (1-rb) * noRejectValue;
}

double
RiskVectorUtility::oracle_utility (double mu, double rejectValue, double noRejectValue) const 
{
  std::pair<double,double>  rprob  (reject_probabilities(mu));
  double rb (rprob.second);
  return negative_risk(mu,mAlpha) + rb * rejectValue + (1-rb) * noRejectValue;
}

double
RiskVectorUtility::negative_risk(double mu, double alpha) const
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
  return -R;
}
