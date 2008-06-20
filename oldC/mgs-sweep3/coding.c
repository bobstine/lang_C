/*
  21 Sep 01 ...   Created to encapsulate coding-related functions.
*/

#include <math.h>
#include "coding.h"

#define LN2 0.69315
#define PI  3.14159

double log2 (double x)
{
  return (log (x)/LN2);  //    (ln x)/(ln 2) 
}

////////////////////////////////////////////////////////////////////////

double
codeIndexLength(double gShare, double gProb, long k)
{
  return (-log2( (   gShare   ) * geometricPDF(gProb, k) +
		(1.0 - gShare) * cauchyPDF(k)));
}


double
fStatisticThreshold (double gShare, double gProb, long k)
{
  double ell_k = codeIndexLength (gShare, gProb, k);
  if (k <= 5) // use quadratic approximation here
  { double z;
    z = 0.272727 * (4.0 + sqrt(28.8374 + 22.0 * LN2 * ell_k));
    return z*z;
  }
  else return (2.0 * LN2) * (4.0 + log2(LN2) + log2(ell_k) + ell_k);
}

/////////////////////////////////////////////////////////////////////

double geometricPDF (double p, long k)
{
  double prob = p;
  for (long i=1; i<k; ++i)
    prob *= (1.0 - p);
  return prob;
}

#define CAUCHY_1 1.07667
#define CAUCHY_2 0.861028

double cauchyPDF (long k)
{
  double dk = (double) k;
   
  return (1.0 / ((1.0 + dk * dk) * CAUCHY_1));
  // replace CAUCHY by PI * PI /6.0 for 1/i^2
}
