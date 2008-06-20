/*  $Id: bennett.cc,v 1.3 2002/03/13 15:44:26 bob Exp $

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
  assert (a > 0.0);
  assert (b > 0.0);
  double sign = (beta > 0) ? -1.0 : 1.0;
  double absBeta = abs_val (beta);
  double z = bennett_critical_value (a/b, pValue);
  if (z * b > absBeta)
    return 0.0;
  else
    return beta + sign * z * b;
}

double
bennett_critical_value (double m, double pValue)
{
  assert(m > 0.0);
  double z = 1.0;
  while (bennett_p_value(z,m) > pValue)
    z += 1.0;
  z -= 1.0;
  while (bennett_p_value(z,m) > pValue)
    z += .1;
  z -= .1;
  while (bennett_p_value(z,m) > pValue)
    z += .01;
  return z;
}

double
bennett_p_value (double bHat, double a, double b)
{
  return bennett_p_value (abs_val(bHat)/b, a/b);
}


double
bennett_p_value (double t, double m)  
{
  double at = abs_val(t);
  return exp( at/m -
	      (at/m + 1.0/(m*m)) * log(1.0 + m * at));
}
