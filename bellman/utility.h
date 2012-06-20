#ifndef _UTILITY_H_
#define _UTILITY_H_

#include "wealth.h"

#include <Eigen/Core>

#include <utility>         // pair
#include <functional>
#include <math.h>
#include <assert.h>
#include <iostream>      // debug


typedef Eigen::MatrixXf Matrix;

typedef std::pair<int,double> WIndex;

////////////////////////////////////  Utility functions  /////////////////////////////////////////

double   reject_prob(double mu, double level);


double   reject_value(   int i         , WIndex const& kp , Matrix const& value);
double   reject_value(WIndex const& kp ,       int j      , Matrix const& value);
double   reject_value(WIndex const& kp1, WIndex const& kp2, Matrix const& value);


double   z_alpha       (double a);

double   optimal_alpha (double mu, double omega);

double   risk          (double mu, double alpha); 
double   neg_risk      (double mu, double alpha); 


////   Vector utility trades off between two possible values

class VectorUtility: public std::unary_function<double,double>
{
 protected:
  const double mGamma;
  const double mOmega;
  double mAlpha, mBeta;
  double mRejectValue, mNoRejectValue;
  
 public:

 VectorUtility(double gamma, double omega)
   : mGamma(gamma), mOmega(omega), mAlpha(omega), mBeta(0.0), mRejectValue(0.0), mNoRejectValue(0.0) {}

  double alpha      () const { return mAlpha; }   // equal to omega unless set outside
  double beta       () const { return mBeta;  }
  double gamma      () const { return mGamma; }
  double omega      () const { return mOmega; }   // is alpha unless otherwise set
  
  void set_constants (double alpha, double beta, double rejectValue, double noRejectValue)
  { assert((0 <= alpha) && (alpha <= 1.0));
    mAlpha = alpha;
    set_constants(beta,rejectValue,noRejectValue);
  }

  void set_constants (double beta, double rejectValue, double noRejectValue)
  { assert((0 <= beta) && (beta <= 1.0));
    mBeta = beta;
    mRejectValue = rejectValue;
    mNoRejectValue = noRejectValue;
  }

  double r_mu_alpha (double mu) const;
  double r_mu_beta  (double mu) const;
  
  std::pair<double,double> reject_probabilities (double mu) const;    // prob rejecting for alpha and beta
  
  virtual
    double operator()(double mu) const  { std::cout << "UTIL:  Call to operator of base class." << std::endl; return 0*mu; }

  virtual
    double bidder_utility (double mu, double rejectValue, double noRejectValue) const = 0;
  
  virtual
    double oracle_utility (double mu, double rejectValue, double noRejectValue) const = 0;
  
}; 



////  RejectVectorUtility     Rejects     Rejects     Rejects     Rejects     Rejects     Rejects     

class RejectVectorUtility: public VectorUtility
{
 public:

 RejectVectorUtility(double gamma, double omega)
   : VectorUtility(gamma, omega) { }

  double operator()(double mu) const;

  double bidder_utility (double mu, double rejectValue, double noRejectValue) const;
  double oracle_utility (double mu, double rejectValue, double noRejectValue) const;
  
}; 


////  RiskVectorUtility     Risk     Risk     Risk     Risk     Risk     Risk     Risk     Risk     Risk

class RiskVectorUtility: public VectorUtility
{
 public:

 RiskVectorUtility(double gamma, double omega)
   : VectorUtility(gamma,omega) { }
  
  double operator()(double mu) const;
  
  double bidder_utility (double mu, double rejectValue, double noRejectValue) const;
  double oracle_utility (double mu, double rejectValue, double noRejectValue) const;
}; 





////   Matrix utility trades off between four possible values

class MatrixUtility: public std::unary_function<double,double>
{
 protected:
  const double mGamma;
  const double mOmega;
  double mAlpha, mBeta;
  double mV00, mV01, mV10, mV11;  // 0 for not reject, 1 for reject
  
 public:

 MatrixUtility(double gamma, double omega)
   : mGamma(gamma), mOmega(omega), mAlpha(omega), mBeta(0.0), mV00(0.0), mV01(0.0), mV10(0.0), mV11(0.0) {}

  double alpha      () const { return mAlpha; }
  double beta       () const { return mBeta;  }
  double gamma      () const { return mGamma; }
  double omega      () const { return mOmega; }   // is alpha unless otherwise set
  
  void set_constants (double alpha, double beta, double v00, double v01, double v10, double v11)
  { assert((0 <= alpha) && (alpha <= 1.0));
    assert((0 <=  beta) && ( beta <= 1.0));
    mAlpha=alpha;
    mBeta = beta;
    mV00 = v00; mV01 = v01; mV10 = v10; mV11 = v11;
  }

  double r_mu_alpha (double mu) const;
  double r_mu_beta  (double mu) const;
  
  std::pair<double,double> reject_probabilities (double mu) const;    // prob rejecting for alpha and beta
  
  virtual
    double operator()(double mu) const  { std::cout << "UTIL:  Call to operator of matrix base class." << std::endl; return 0*mu; }

  virtual
    double bidder_utility (double mu, double v00, double v01, double v10, double v11) const = 0;
  
  virtual
    double oracle_utility (double mu, double v00, double v01, double v10, double v11) const = 0;
  
}; 



////  RejectMatrixUtility     Rejects     Rejects     Rejects     Rejects     Rejects     Rejects     

class RejectMatrixUtility: public MatrixUtility
{
 public:

 RejectMatrixUtility(double gamma, double omega)
   : MatrixUtility(gamma, omega) { }

  double operator()(double mu) const;

  double bidder_utility (double mu, double v00, double v01, double v10, double v11) const;
  double oracle_utility (double mu, double v00, double v01, double v10, double v11) const;
  
}; 


////  Risk     Risk     Risk     Risk     Risk     Risk     Risk     Risk     Risk     Risk

class RiskMatrixUtility: public MatrixUtility
{
 public:

 RiskMatrixUtility(double gamma, double omega)
   : MatrixUtility(gamma,omega) { }
  
  double operator()(double mu) const;
  
  double negative_risk(double mu, double alpha) const;

  double bidder_utility (double mu, double v00, double v01, double v10, double v11) const;
  double oracle_utility (double mu, double v00, double v01, double v10, double v11) const;
}; 



#endif
