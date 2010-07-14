/*
  21 Sep 01 ...   Created to encapsulate coding-related functions.
*/

#include <math.h>
#include <assert.h>
#include <iostream>
#include "coding.h"

namespace {
  const double twoLogTwo (1.386294361);
  const double LN2       (0.693147181);
  const double PI        (3.141592654);
}


// gaussian  gaussian  gaussian  gaussian  gaussian  gaussian  gaussian  gaussian  gaussian

double
gaussian_data_length (int n, double RSS)
{
  assert (RSS >=0);
  if (RSS == 0.0)
    return 1.0;
  else
  {
    double s2 (RSS / n);        // MLE for sigma^2
    return n/(2 * LN2) * (log(2.0 * PI * s2) + 1);
  }
}

double
change_in_code_length (int n, double RSS, int q, double dRSS)
{
  double logRatio              (.5 * log2(RSS/(RSS-dRSS)));
  double gainInDataCompression (n * logRatio);
  SignedCauchyCoder            scc;
  double z                     (sqrt (n * dRSS/RSS));  // no df since using MLEs
  double addedPrefixLength     (scc(z) + (q+1)*logRatio);
  // std::clog << "CODE: (" << n << "," << q << "," << RSS << "," << dRSS << ") --> z="
  //    << z << " with change " << addedPrefixLength << "-" << gainInDataCompression
  //    << " = " << addedPrefixLength - gainInDataCompression << std::endl;
  return addedPrefixLength - gainInDataCompression;
}

// CAUCHY CODERS  CAUCHY CODERS  CAUCHY CODERS  CAUCHY CODERS  CAUCHY CODERS  CAUCHY CODERS  CAUCHY CODERS  


double
CauchyCoder::operator()(int j) const
{
  if (j == 0)
  { std::clog << "CODE: cauchy code for zero asked; returning 0" << std::endl;
    return 0.0;
  }
  else
    return SignedCauchyCoder()(j) - 2.0;
}

double
SignedCauchyCoder::operator()(double z) const
{
  assert (z > 0.0);
  double log2z (log2(z));

  if (log2z > 1.0)
    return 3.0 + log2z + 2.0 * log2 (log2z);
  else 
    return 3.0 + log2z;                 // sign + zero + |z|
}


////////////////////////////////////////////////////////////////////////

double
codeIndexLength(double gShare, double gProb, int k)
{
  return (-log2( (   gShare   ) * geometricPDF(gProb, k) +
		(1.0 - gShare) * cauchyPDF(k)));
}


double
fStatisticThreshold (double gShare, double gProb, int k)
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

double geometricPDF (double p, int k)
{
  double prob = p;
  for (int i=1; i<k; ++i)
    prob *= (1.0 - p);
  return prob;
}

#define CAUCHY_1 1.07667
#define CAUCHY_2 0.861028

double cauchyPDF (int k)
{
  double dk = (double) k;
   
  return (1.0 / ((1.0 + dk * dk) * CAUCHY_1));
  // replace CAUCHY by PI * PI /6.0 for 1/i^2
}

namespace {
  double log_star (int k) 
  {
    switch (k) 
    {
      case 1 :  return 0.0;
      case 2 :  return 1.0; 
      case 3 :  return 2.249411208; 
      case 4 :  return 3.0;
      default:  
        double cum (0.0);
        double z   (log2 ((double)k));
        while (z > 0) {
          cum += z;
          z = log2(z);
        };
        return cum;
    }
  }
}

double
universalPDF (int k)  
{
  const double normalizer (2.865);
  return exp2(-log_star(k))/normalizer;
}
  
  
double
slowUniversalPDF (int k)
{
  assert(k>0);
  const double normalizer (2.865 - 1.5);   // adjust for skipping first 2 log-star terms
  return exp2(-log_star(k+2))/normalizer;
}

  
