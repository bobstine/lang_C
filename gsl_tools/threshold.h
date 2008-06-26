// $Id: threshold.h,v 1.1 2003/06/20 04:29:05 bob Exp $

/*
  These functions are for finding the threshold when using a
  sequential selection procedure.
*/

#ifndef _THRESHOLD_H_
#define _THRESHOLD_H_

#include <functional>

class Threshold : public std::unary_function<int,double>
{
  double mEps;
  
 public:

  Threshold () :
    mEps (0.0) {}
  
  double
    operator()(int j);

 private:
  double newton_step (double x0) const;
  
};

class CodeProbability : public std::unary_function<int,double>
{
 public:
  double
    operator()(int j) const;
};

#endif
