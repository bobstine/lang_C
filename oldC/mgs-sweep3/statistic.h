/* $Id: statistic.h,v 1.14 2002/02/15 22:31:05 bob Exp $
   
  Basic statistics computed for variables applied to data sets.

  11 Dec 01 ... Created.
  
*/
#ifndef _STATISTIC_H_
#define _STATISTIC_H_

#include <vector>

#include "dataset.h"
#include "variable.h"

vector<double>
realization (const Variable &var, const Dataset& data);

vector<double>
centered_realization (const Variable &var, double varBar, const Dataset& data);

double
raw_average (const Variable& var, const Dataset& data);

double
raw_variance (const Variable& var, const double yBar, const Dataset& data);

inline double
raw_variance (const Variable& var, const Dataset& data)
{ return raw_variance(var, raw_average(var,data), data); }

inline double
raw_SD (const Variable& var, const double yBar, const Dataset& data)
{ return sqrt(raw_variance(var,yBar,data)); }

inline double
raw_SD (const Variable& var, const Dataset& data)
{ return raw_SD(var, raw_average(var,data), data); }


double
average (const Variable& var, const Dataset& data);

double
sum_of_squares (const Variable& var, const double center, const Dataset& data);

double
variance (const Variable& var, const double avg, const Dataset& data);

inline double
variance (const Variable& var, const Dataset& data)
{ return variance(var, average(var,data), data); }

inline double
SD (const Variable& var, const double avg, const Dataset& data)
{ return sqrt(variance(var, avg, data)); }

inline double
SD (const Variable& var, const Dataset& data)
{ return sqrt(variance(var,data)); }

////  Vector average, SS  :  all of these take account of sampling wts in data set

vector<double>
average (const vector<Variable>& varVec, const Dataset& data);

vector<double>
sum_of_squares (const vector<Variable>& varVec, const Dataset& data);

vector<double>
weighted_sum_of_squares (const vector<Variable>& varVec, const Dataset& data, const vector<double>& wts);

//////////////////////  Covariances  ////////////////////////////

double
covariance (const Variable& y, const double yBar,
	    const Variable& z, const double zBar,
	    const Dataset& data);


double
covariance (pair <double *, double *> e, // e bar is zero
	    const Variable& z, const double zBar, const Dataset& data);


vector<double> 
covariance (const Variable& y, const double yBar,   // short-cut without z-bar
	    const vector<Variable>& z, const Dataset& data);

inline double
correlation(const Variable& x, const Variable& y, const Dataset& data)
{
  double xBar(average(x,data));
  double yBar(average(y,data));
  return covariance(x,xBar,y,yBar,data) /
    sqrt(variance(x,xBar,data) * variance(y,yBar,data));
}

#endif
