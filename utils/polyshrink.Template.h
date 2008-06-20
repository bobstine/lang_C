/*
 *  polyshrink.Template.h
 *  utils
 *
 *  Created by Robert Stine on 11/12/07.
 *  Copyright 2007. All rights reserved.
 *
 */


#include "normal.h"

#include <math.h>
#include <vector>
#include <numeric>

// debug only 
// #include <iostream>             

namespace {  
  const double SQRT2  (1.414213562373095049);
  const double SQRTPI (1.772453850905516027);
  
  double mixture(double z, double eps)
  {
    return (1.0-eps) * normal_density(z) + eps * cauchy_density(z/SQRT2)/SQRT2;
  }
  
  double score(double z, double eps)
  {
    double zz      (z * z);
    double twopzz  (2.0 + zz);   // 2 + z*z
    double exp_zz2 (exp(zz/2));  // exp(z*z/2)
    double num     (-SQRTPI*twopzz*twopzz * (eps-1.0) + 4.0 * exp_zz2 * eps);
    double den     ( SQRTPI*twopzz        * (eps-1.0) - 2.0 * exp_zz2 * eps);
    return ((z*num)/(twopzz*den));
  }
}


template <class Container>
void
polyshrink (Container const& input, Container & output)
{
  const int n           (input.size());
  const int k (1.0 + log2((double)n));
  typename Container::const_iterator z (input.begin());   // 3 passes
  typename Container::iterator       y (output.begin());
  
  // identify any larger than upper size and set them to this limit
  const double TOO_BIG  (25.0);      
  for(int i=0; i<n; ++i, ++z, ++y)
  { if ((*z < -TOO_BIG) || (*z > TOO_BIG))
      *y = *z;
    else 
      *y = TOO_BIG; 
  }
  // accumulate weighted product of mixtures in gProd
  double delta   (0.5);        // init to 2^(-1)
  double epsilon (1.0);        //         2^(-k)
  for (int i=0; i<k; ++i) epsilon /= 2.0;
  double two_m_k (epsilon);    // save 2^(-k)
  std::vector<double> p (k,0.0);
  for (int j=1; j<=k; ++j)
  { double gProd (1.0);      
    z = input.begin();
    for (int i=0; i<n; ++i, ++z)
      gProd *= 4.0 * mixture(*z,epsilon);  // avoid underflow by 4 * 
    p[j-1] = delta * gProd;
    delta   /= 2.0;
    epsilon *= 2.0;
  }
  // form probability weights, then matrix product
  double pSum (std::accumulate(p.begin(), p.end(), 0.0));
  for (int j=0; j<k; ++j) 
  { p[j] /= pSum;
    // std::cout << " p[" << j << "]" << p[j] << "\n";
  }
  z = input.begin();
  y = output.begin();
  for (int i=0; i<n; ++i, ++z, ++y) 
  { double incr (0.0);
    double eps  (two_m_k);
    for (int j=0; j<k; ++j)
    { incr += score(*z, eps) * p[j];
      eps *= 2.0;
    }
    *y = *z + incr;
  }
  // restore the big ones
  z = input.begin();
  y = output.begin();
  for (int i=0; i<n; ++i, ++z, ++y)
    if ((*z < -TOO_BIG) || (*z > TOO_BIG)) *y = *z;
}
