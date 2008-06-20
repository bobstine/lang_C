/* $Id: statistic.h,v 1.10 2003/05/05 22:14:07 bob Exp $
   
  Basic statistics computed for variables applied to data sets.

  19 Dec 02 ... Lots of mods to work with new range code.
  11 Dec 01 ... Created.
  
*/
#ifndef _STATISTIC_H_
#define _STATISTIC_H_

#include <vector>

#include "dataset.h"
#include "variable.h"
#include "range_traits.h"
#include "range.h"
#include "range_ops.h"
#include "compose.h"
#include "function_utils.h"

inline
unary_range_traits<Variable,Dataset>::range
realization (Variable var, const Dataset& data)
{
  return make_unary_range(var,data);
}

inline
unary_range_traits<boost::compose_f_gx_t<Function_Utils::Center,Variable>,Dataset>::range
centered_realization (Variable var, double center, const Dataset& data)
{
  return make_unary_range(boost::compose_f_gx(Function_Utils::Center(center),var),
			  data);
}

/********************************************************************
  Raw calculations ignore sample weights in the dataset.
*/

inline
double
raw_average (Variable var, const Dataset& data)
{
  return range_ops::accumulate(make_unary_range(var, data),
			       0.0)
    / data.nRows();
}

inline
double
raw_sum_of_squares (Variable var, double yBar, const Dataset& data)
{
  return range_ops::accumulate(
			       make_unary_range(
						boost::compose_f_gx(Function_Utils::CenterSquare(yBar),var),
						data),
			       0.0);
}

inline
double
raw_variance (Variable var, const double yBar, const Dataset& data)
{
  return raw_sum_of_squares(var,yBar,data)/(data.nRows()-1.0);
}

inline
double
raw_variance (Variable var, const Dataset& data)
{ return raw_variance(var, raw_average(var,data), data); }

inline
double
raw_SD (Variable var, const double yBar, const Dataset& data)
{ return sqrt(raw_variance(var,yBar,data)); }

inline
double
raw_SD (Variable var, const Dataset& data)
{ return raw_SD(var, raw_average(var,data), data); }



/********************************************************************
  The following functions use the weights in the dataset to obtain the
  associated sums of the form sum_i w_i f(x_i)
*/


template <class UnaryOp>  // Need template to match composed variables
typename binary_range_traits<std::multiplies<double>,
			     typename unary_range_traits<UnaryOp, Dataset>::range,
			     typename Dataset::weight_range>::range
make_weighted_range(const UnaryOp& op, const Dataset& data)
{
  return make_binary_range(std::multiplies<double>(),
			   make_unary_range(op, data),
			   data.weights());
}

double
average (Variable var, const Dataset& data);

double
sum_of_squares (Variable var, const double center, const Dataset& data);


double
variance (Variable var, const double avg, const Dataset& data);

inline
double
variance (Variable var, const Dataset& data)
{ return variance(var, average(var,data), data); }

inline
double
SD (Variable var, const double avg, const Dataset& data)
{ return sqrt(variance(var, avg, data)); }

inline
double
SD (Variable var, const Dataset& data)
{ return sqrt(variance(var,data)); }


/********************************************************************
  Vector based operations.
  
  The goal of these functions is to accumulate all of the desired
  information, such as for a collection of averages in "one pass" over
  the dataset.  If this is not a time saver, might as well just
  iterate the scalar versions over the dataset.
*/

/*
template<class Range, class WriteableRange>
void
average_range_of_variables (Range vars, WriteableRange* avgs, const Dataset& data)
{
  typename writeable_range_traits<WriteableRange>::range avgRange (make_writeable_range(avgs));
  
  range_ops::accumulate_weighted_table (make_unary_range(make_function_range<Observation>(vars), data),
					avgRange,
					data.weights());
  range_ops::transform (avgRange, avgRange,
			std::bind2nd(std::divides<double>(), data.sum_weights()));
}
*/
/********************************************************************
  Covariances

*/

double
cross_product (Variable y, const double yBar,
	       Variable z, const double zBar,
	       const Dataset& data);

// Would prefer to do this like average_range_of_variables, but code bloats

void
cross_product_vector (Variable y, const double yBar,
		      const std::vector<Variable>& z, const std::vector<double>& zBar,
		      std::pair<double *,double *> cpVector,
		      const Dataset& data);

inline
double
covariance (Variable y, const double yBar,
	    Variable z, const double zBar,
	    const Dataset& data)
{ return cross_product(y,yBar, z,zBar, data)/(data.sum_weights()-1.0); }


double
covariance (Variable e, Variable z, const Dataset& data);  // assume e has avg 0


inline double
correlation(Variable x, Variable y, const Dataset& data)
{
  double xBar(average(x,data));
  double yBar(average(y,data));
  return covariance(x,xBar,y,yBar,data) /
    sqrt(variance(x,xBar,data) * variance(y,yBar,data));
}


#endif
