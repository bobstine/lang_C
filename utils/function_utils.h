/*  $Id: function_utils.h,v 1.20 2008/01/29 23:44:01 bob Exp $

  Functions

  Utility functions used in various places, particularly in stat routines.
  Put them here to allow use of templates.

  23 Mar 02 ... Update for GCC 3 style.
  
*/

#ifndef _FUNCTION_UTILS_H_
#define _FUNCTION_UTILS_H_

#include "operator_traits.h"

#include <math.h>
#include <functional>
#include <string>

namespace Function_Utils
{

  class AbsValue : public std::unary_function<double,double> {
  public:
    double operator()(double x) const { return (x >= 0.0) ? x : -x; }
  };
  
  class Log : public std::unary_function<double,double> {
  public:
    double operator()(double x) const { if (x > 0) return log(x); else return -7.7; }
  };
  
  class Sqrt : public std::unary_function<double,double> {
  public:
    double operator()(double x) const { if (x >= 0) return sqrt(x); else return -7.7; }
  };


  class Square : public std::unary_function<double,double> {
  public:
    double operator()(double x) const { return x*x; }
  };
  

  class CenteredSquare : public std::unary_function<double,double> {
  private:
    double mCenter;
  public:
    CenteredSquare(double center) : mCenter(center) {};

    double operator()(double x) const { double dev (x - mCenter); return dev*dev; }
  };
  
  
  class Cube : public std::unary_function<double,double> {
  public:
    double operator()(double x) const { return x*x*x; }
  };
  

  class CenteredCube : public std::unary_function<double,double> {
  private:
    double mCenter;
  public:
    CenteredCube(double center) : mCenter(center) {};

    double operator()(double x) const { double dev (x - mCenter); return dev*dev*dev; }
  };
  
  
  
  class Power : public std::unary_function<double,double> {
    int mPower;
  public:
    Power(int p): mPower(p) { }
    double operator()(double x) const { int i=mPower; double result(x); while(--i) result *= x; return result; }
  };

  
  class LogisticNeg : public std::unary_function<double,double> {
  public:
    double operator()(double x) const { return 1.0/(1.0 + exp(-x)); }
  };

  
  const double eps (0.000001);
  const double minLogit (log(eps/(1.0-eps)));
  const double maxLogit (-minLogit);
  class Logit : public std::unary_function<double,double> {
  public:
    double operator()(double p) const
      {
	if (p < eps)
	  return minLogit;
	else if (p > 1.0-eps)
	  return maxLogit;
	else
	  return log (p / (1.0 - p));
      }
  };

  
  class LogisticPos : public std::unary_function<double,double> {
  public:
    double operator()(double x) const { return 1.0/(1.0 + exp(x)); } 
  };

  
  class LogisticLikeTerm : public std::binary_function<double,double,double>
  {
  public:
    double operator()(double y, double xb) { return y * xb - log(1.0 + exp(xb)); }
  };


  class Center : public std::unary_function<double,double> {
    const double mC;
  public:
    Center(double c) : mC(c) { }
    double operator()(double x) const { return x-mC; }
  };

  
  class SquaredDeviation : public std::unary_function<double,double> 
  {
    const double mC;
   public:
    SquaredDeviation(double c) : mC(c) { }
    double operator()(double x) const { double dev (x-mC); return dev*dev; }
  };
  
  
  
  class Standardize : public std::unary_function<double,double> {
    const double mC, mS;
  public:
    Standardize(double c, double s) : mC(c),mS(s) { }
    double operator()(double x) const { return (x-mC)/mS; }
  };

  
  class AXPY : public std::binary_function<double,double,double> {
    const double mA;
  public:
    AXPY(double a) : mA(a) { }
    double operator()(double x, double y) const { return mA * x + y; }
  };

  
  class BinomialVariance :public std::unary_function<double,double> {
  public:
    double operator()(double p) { return p * (1-p) ; }
  };

  
  template <class Container, class Scalar>
    class Index : public std::unary_function<Container, Scalar> {
    int mIndex;
  public:
    Index(int i) : mIndex(i) { }
    Scalar operator() (const Container& v) const
    { return v[mIndex]; }
  };
}

#endif
