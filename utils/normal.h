// $Id: normal.h,v 1.5 2007/11/09 22:20:53 bob Exp $

/*
  9 Nov 07 ... Add cauchy.
  2 Jun 03 ... Adapted from Luke Tierney's xlispstat code.
*/


#ifndef _NORMAL_H_
#define _NORMAL_H_

double
normal_density(double x);

double
normal_cdf(double x);

double
normal_quantile(double p);


double
cauchy_density(double x);

double
cauchy_cdf(double x);

double
cauchy_quantile(double p);


#endif
