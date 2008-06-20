/*  $Id: functions.h,v 1.7 2002/03/23 22:49:20 foster Exp $

  Functions

  Utility functions used in various places, particularly in stat routines.
  Put them here to allow use of templates.

  23 Mar 02 ... Update for GCC 3 style.
  
*/

#ifndef _FUNCTIONS_H_
#define _FUNCTIONS_H_

#include <functional>
#include <iostream>

namespace Function
{

  class AbsValue : public std::unary_function<double,double> {
  public:
    double operator()(double x) const { return (x >= 0.0) ? x : -x; }
  };
  
  class Square : public std::unary_function<double,double> {
  public:
    double operator()(double x) const { return x*x; }
  };

  class Center : public std::unary_function<double,double> {
    const double mC;
  public:
    Center(double c) : mC(c) { }
    double operator()(double x) const { return x-mC; }
  };
  
  class CenterSquare : public std::unary_function<double,double> {
    const double mC;
  public:
    CenterSquare(double c) : mC(c) { }
    double operator()(double x) const { double z = x - mC; return z*z; }
  };

  class BinomialVariance :public std::unary_function<double,double> {
  public:
    double operator()(double p) { return p * (1-p) ; }
  };
  
  class Index : public std::unary_function<const std::vector<double>&, double> {
    int mIndex;
  public:
    Index(int i) : mIndex(i) { }
    double operator()(const std::vector<double>& v)
    { return v[mIndex]; }
  };
}

#endif
