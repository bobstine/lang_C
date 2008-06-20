/*
  26 Feb 02 ... Use Bennett's method to compute a conservative p-value.
*/

#ifndef _BENNETT_H_
#define _BENNETT_H_

double
bennett_p_value (double t, double m);  // m is a/b, t is beta^/b

double
bennett_p_value (double bHat, double a, double b);

double
bennett_critical_value (double m, double pValue);

double
bennett_lower_bound (double beta, double a, double b, double pValue);

#endif
