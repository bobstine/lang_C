// $Id: threshold.cc,v 1.1 2003/06/20 04:29:05 bob Exp $

/*
  These functions are for finding the threshold when using a
  sequential selection procedure.
*/

#include "threshold.h"
#include "math.h"

#include <iostream>

double
Threshold::operator()(int j)
{
  mEps = CodeProbability()(j);

  // prep Newton's method
  if (mEps > 0.5) mEps = 0.5;
  // set initial value
  double x (sqrt (2.0 * log(10./mEps)));
  // three steps
  x += newton_step(x);
  x += newton_step(x);
  x += newton_step(x);
  return x;
}

double
Threshold::newton_step (double x0) const  // this is the negative step
{
  double sqrt_2pi (2.506628274631);
  double c1       (1.0 + x0 * x0);
  double c2       (sqrt_2pi * c1 * (mEps - 1.0));
  double c3       (2.0 * exp(x0 * x0 / 2.0) * mEps);

  return c1 * (c2 + c3)/(x0 * (c1 * c2 + 2.0 * c3));
}



double
CodeProbability::operator()(int j) const
{
  double logj (log(j));
  double log2 (log(2));
  double log2j (logj/log2);
  
  return 0.5625/(1.0 + (j * log2j * log2j));
}
