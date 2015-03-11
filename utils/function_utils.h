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
  typedef float Scalar;
  
  class AbsValue : public std::unary_function<Scalar,Scalar> {
  public:
    Scalar operator()(Scalar x) const { return (x >= 0.0) ? x : -x; }
  };
  
  class Log : public std::unary_function<Scalar,Scalar> {
  public:
    Scalar operator()(Scalar x) const { if (x > 0) return (Scalar)log(x); else return (Scalar)-7.7; }
  };
  
  class Sqrt : public std::unary_function<Scalar,Scalar> {
  public:
    Scalar operator()(Scalar x) const { if (x >= 0) return (Scalar)sqrt(x); else return (Scalar)-7.7; }
  };


  class Square : public std::unary_function<Scalar,Scalar> {
  public:
    Scalar operator()(Scalar x) const { return x*x; }
  };
  

  class CenteredSquare : public std::unary_function<Scalar,Scalar> {
  private:
    Scalar mCenter;
  public:
    CenteredSquare(Scalar center) : mCenter(center) {};

    Scalar operator()(Scalar x) const { Scalar dev (x - mCenter); return dev*dev; }
  };
  

  class CenteredMultiply : public std::binary_function<Scalar,Scalar,Scalar> {
  private:
    Scalar mAvg1, mAvg2;
  public:
  CenteredMultiply(Scalar avg1, Scalar avg2) : mAvg1(avg1), mAvg2(avg2) {};

    Scalar operator()(Scalar x1, Scalar x2) const { return (x1 - mAvg1)*(x2 - mAvg2); }
  };
  
  
  class Cube : public std::unary_function<Scalar,Scalar> {
  public:
    Scalar operator()(Scalar x) const { return x*x*x; }
  };
  

  class CenteredCube : public std::unary_function<Scalar,Scalar> {
  private:
    Scalar mCenter;
  public:
    CenteredCube(Scalar center) : mCenter(center) {};

    Scalar operator()(Scalar x) const { Scalar dev (x - mCenter); return dev*dev*dev; }
  };
  

  class CenteredQuad : public std::unary_function<Scalar,Scalar> {
  private:
    Scalar mCenter;
  public:
    CenteredQuad(Scalar center) : mCenter(center) {};

    Scalar operator()(Scalar x) const { Scalar dev (x - mCenter); dev = dev * dev; return dev*dev; }
  };
  

  class CenteredQuint : public std::unary_function<Scalar,Scalar> {
  private:
    Scalar mCenter;
  public:
    CenteredQuint(Scalar center) : mCenter(center) {};

    Scalar operator()(Scalar x) const { Scalar dev (x - mCenter); Scalar result (dev*dev); return result*result*dev; }
  };
  
  
  
  class Power : public std::unary_function<Scalar,Scalar> {
    int mPower;
  public:
    Power(int p): mPower(p) { }
    Scalar operator()(Scalar x) const { int i=mPower; Scalar result(x); while(--i) result *= x; return result; }
  };

  
  
  class CenteredPower : public std::unary_function<Scalar,Scalar> {
    size_t mPower;
    Scalar mCenter;
  public:
  CenteredPower(size_t p, Scalar center): mPower(p), mCenter(center) { }
    Scalar operator()(Scalar x) const { size_t i=mPower; x = x-mCenter; Scalar result(x); while(--i) result *= x; return result; }
  };


  class LogisticNeg : public std::unary_function<Scalar,Scalar> {
  public:
    Scalar operator()(Scalar x) const { return (Scalar)(1.0/(1.0 + exp(-x))); }
  };

  
  const Scalar eps      = (Scalar)0.000001;
  const Scalar minLogit = (Scalar) (log(eps/(1.0-eps)));
  const Scalar maxLogit (-minLogit);
  class Logit : public std::unary_function<Scalar,Scalar> {
  public:
    Scalar operator()(Scalar p) const
      {
	if (p < eps)
	  return minLogit;
	else if (p > 1.0-eps)
	  return maxLogit;
	else
	  return (Scalar)(log(p / ((Scalar)1.0 - p)));
      }
  };

  
  class LogisticPos : public std::unary_function<Scalar,Scalar> {
  public:
    Scalar operator()(Scalar x) const { return (Scalar)(1.0/(1.0 + exp(x))); } 
  };

  
  class LogisticLikeTerm : public std::binary_function<Scalar,Scalar,Scalar>
  {
  public:
    Scalar operator()(Scalar y, Scalar xb) { return y * xb - (Scalar)log(1.0 + exp(xb)); }
  };


  class Center : public std::unary_function<Scalar,Scalar> {
    const Scalar mC;
  public:
    Center(Scalar c) : mC(c) { }
    Scalar operator()(Scalar x) const { return x-mC; }
  };

  
  class SquaredDeviation : public std::unary_function<Scalar,Scalar> 
  {
    const Scalar mC;
   public:
    SquaredDeviation(Scalar c) : mC(c) { }
    Scalar operator()(Scalar x) const { Scalar dev (x-mC); return dev*dev; }
  };
  
  
  
  class Standardize : public std::unary_function<Scalar,Scalar> {
    const Scalar mC, mS;
  public:
    Standardize(Scalar c, Scalar s) : mC(c),mS(s) { }
    Scalar operator()(Scalar x) const { return (x-mC)/mS; }
  };

  
  class AXPY : public std::binary_function<Scalar,Scalar,Scalar> {
    const Scalar mA;
  public:
    AXPY(Scalar a) : mA(a) { }
    Scalar operator()(Scalar x, Scalar y) const { return mA * x + y; }
  };

  
  class BinomialVariance :public std::unary_function<Scalar,Scalar> {
  public:
    Scalar operator()(Scalar p) { return p * (1-p) ; }
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
