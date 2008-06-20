/*
  26 Feb 02 ... Use Bennett's method to compute a conservative p-value.
*/

#ifndef _BENNETT_H_
#define _BENNETT_H_

double
bennett_p_value (double t, double m);  // m is a/b, t is beta^/b (positive)

double
bennett_p_value (double bHat, double a, double b);


double
bennett_critical_value (double m, double pValue);


double
bennett_bound (double beta, double a, double b, double pValue);

/*
  The bound is the value on the end of the CI nearest to zero.
  
  Th bound is found by first solving for the b* that attains the input
  p-value (which would just be 0 + 2 SE in the normal case. The lower
  bound for the interval then 'subtracts' that value from the given
  estimate, moving it toward zero.

  The returned bound is zero if the resulting CI would contain the
  origin.  The constant b serves as the SE for the interval.
*/
  
#endif
