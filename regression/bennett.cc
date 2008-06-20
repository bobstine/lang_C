/*  $Id: bennett.cc,v 1.1 2002/12/19 15:45:46 bob Exp $

    26 Feb 02 ... Created

*/

#include "bennett.h"

#include <math.h>
#include <assert.h>

namespace {
  inline double abs_val(double x)
  { return (x>=0) ? x  : -x; }
}

double
bennett_lower_bound (double beta, double a, double b, double pValue)
{
  // assert(b > 0);
  if (b > 0.0)
    {
      double m = a/b;
      double beta0 = abs_val(beta);
      double p = bennett_p_value(beta0/b,m);
      if (p > pValue)
	return 0.0;
      else {
	while (bennett_p_value(beta0/b,m) < pValue)
	  beta0 -= b;
	beta0 += .1 * b;
	while (bennett_p_value(beta0/b,m) > pValue)
	  beta0 += .1 * b;
	return beta-beta0;
      }
    }
  else
    return 0.0;
}

double
bennett_p_value (double bHat, double a, double b)
{
  if (b > 0.0)
    return bennett_p_value (bHat/b, a/b);
  else
    return 1.0;
}


double
bennett_p_value (double t, double m)  
{
  return exp( t/m -
	      (t/m + 1.0/(m*m)) * log(1.0 + m * t));
}
