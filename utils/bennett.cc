/*  $Id: bennett.cc,v 1.2 2003/06/05 03:13:07 bob Exp $

    26 Feb 02 ... Created

*/

#include "bennett.h"

#include <math.h>
#include <assert.h>

namespace {
  inline double abs_val(double x)  { return (x>=0) ? x  : -x; }
  inline double max(double x, double y) { return (x >= y) ? x : y; }
}


double
bennett_bound (double beta, double a, double b, double pValue)
{
  assert (a > 0.0);
  assert (b > 0.0);
  double absBeta (abs_val(beta));
  double z       (bennett_critical_value (a/b, pValue));
  if (z * b > absBeta)
    return 0.0;
  else
    return absBeta - z * b;
}

double
bennett_critical_value (double m, double pValue)
{
  assert(m > 0.0);
  double z (3.0);
  while (bennett_p_value(z,m) > pValue)
    z += 1.0;
  z -= 0.1;
  while (bennett_p_value(z,m) < pValue)
    z -= .1;
  z += .1;
  while (bennett_p_value(z,m) > pValue)
    z += .01;
  return z;
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
