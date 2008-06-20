/*
  $Id: statistic.cc,v 1.1 2005/06/13 20:47:51 bob Exp $
*/

#include <iostream>
#include <functional>
#include <algorithm>
#include <numeric>
#include <utility>

#include "statistic.h"
#include "statistic_base.h"
#include "range.h"
#include "functions.h"
#include "utility.h"

#include "compose.h"  // boost

/////////////////////////   Explicit realization  ////////////////////////

vector<double>
realization (const Variable &var, const Dataset& data)
{
  vector<double> x(data.nRows());
  copy(make_unary_range(var,data.range()), x.begin());
  return x;
}

vector<double>
centered_realization (const Variable &var, double varBar, const Dataset& data)
{
  vector<double> x(data.nRows());
  copy(make_unary_range(boost::compose_f_gx(Function::Center(varBar),var),
			data.range()),
       x.begin());
  return x;
}


////////////////////////  Raw calculations  //////////////////////////////

double
raw_average (const Variable& var, const Dataset& data)
{
  return average(make_unary_range(var,data.range()), data.nRows());
}

double
raw_variance (const Variable& var, const double yBar, const Dataset& data)
{
  return variance(make_unary_range(var,data.range()), yBar, data.nRows()-1);
}


/////////////////////////  Weighted mean and variance  ///////////////////

double
average (const Variable& var, const Dataset& data)
{
  return average(make_weighted_unary_range(var, data.range(), data.range_weights()),
		 data.sum_weights());
}

vector<double>
average (const vector<Variable>& varVec, const Dataset& data)
{
  vector<double> initial (varVec.size(),0.0);
  vector<double> total = accumulate_range
    (make_weighted_row_range(varVec.begin(), varVec.end(),
			data.begin(), data.end(),
			data.begin_weights(), data.end_weights()),
     make_range(initial));
  cout << "total after accumulate in vector average is " << total;
  transform (total.begin(), total.end(), total.begin(),
	     bind2nd(divides<double>(), data.sum_weights()));
  return total;
}

double
sum_of_squares (const Variable& var, const double center, const Dataset& data)
{
  return weighted_sum_of_squares ( make_unary_range ( Variable(var,center),
						      data.range()),
				   data.range_weights() );
}

double
variance (const Variable& var, const double yBar, const Dataset& data)
{
  return weighted_variance( make_unary_range( var,
					      data.range() ),
			    yBar,
			    data.sum_weights()-1.0,
			    data.range_weights()  );
}


///////////////////////  Covariance  /////////////////////////////////////


double
covariance (const Variable& y, const double yBar, const Variable& z, const double zBar,
	    const Dataset& data)
{
  return covariance
    (make_weighted_unary_range (boost::compose_f_gx(Function::Center(yBar),y),
			       data.range(), data.range_weights()),
     make_unary_range(boost::compose_f_gx(Function::Center(zBar),z),
		      data.range()),
     data.sum_weights()-1.0);
}

  
double
covariance (pair<double *, double *> e, // mean of e assumed zero
	    const Variable& z, const double zBar, const Dataset& data)
{
  return covariance
    (e,
     make_weighted_unary_range(boost::compose_f_gx(Function::Center(zBar),z),
			       data.range(), data.range_weights()),
     data.sum_weights()-1.0);
}


namespace {
  class SquareVar : unary_function<Variable::argument_type,double>  {
    Variable mVar;
  public:
    SquareVar(const Variable& v)
      : mVar(v) { }
    double
    operator()(Variable::argument_type x)
    { double y = mVar(x); return y*y; }
  };

  SquareVar make_square_var (Variable v)
  { return SquareVar(v); }
}


vector<double>
sum_of_squares (const vector<Variable>& varVec, const Dataset& data)
{
  vector<double> initial (varVec.size(),0.0);
  Function::Square sqr;
  return
    accumulate_range(
  		     make_weighted_row_range(make_unary_range(make_composer(sqr),make_range(varVec)),
					     data.range(),
					     data.range_weights()),
		     make_range(initial)
		     );
} 


vector<double>
weighted_sum_of_squares (const vector<Variable>& varVec, const Dataset& data, const vector<double>& estimation_wts)
{
  vector<double> initial (varVec.size(),0.0);
  Function::Square sqr;
  return
    accumulate_range(
		     make_weighted_row_range(make_unary_range(make_composer(sqr),make_range(varVec)),
					     data.range(),
					     make_binary_range(multiplies<double>(),
							       data.range_weights(), make_range(estimation_wts))  ),
		     make_range(initial)  );
}


/*  ???  generates 373 compiler error  
vector<double>
sum_of_squares (const vector<Variable>& varVec, const Dataset& data)
{
  vector<double> initial (varVec.size(),0.0);
  return
    accumulate_range(
		     make_row_range(
				    make_unary_range (bind1st(
							      ptr_fun(boost::compose_f_gx<Function::Square,Variable>()),
							      SquareVar()
							      ),
						      make_range(varVec)),
				    data.range()
				    ),
		     make_range(initial)
		     );
}
*/

vector<double> 
covariance (const Variable& y, const double yBar,   // short-cut without z-bar
	    const vector<Variable>& z, const Dataset& data)
{
  vector<double> initial (z.size(),0.0);
  vector<double> crossProd = accumulate_range
    (make_weighted_row_range(make_range(z), data.range(), // weight rows defined by z by wts*(y-yBar)
			make_weighted_unary_range(boost::compose_f_gx(Function::Center(yBar),y),
						  data.range(), data.range_weights())),
     make_range(initial));
  cout << "total after accumulate in vector covariance is " << crossProd;
  transform (crossProd.begin(), crossProd.end(), crossProd.begin(),
	     bind2nd(divides<double>(), data.sum_weights()-1.0));
  return crossProd;
}

////////////////////////  Partial Covariance  /////////////////////////////

/*
namespace
{
  class SubtractBX {
    double mB;
  public:
    SubtractBX (double b) : mB(b) { }
    double operator(double z, double x) { return z - mB * x; }
  };
}

double
partial_covariance (const vector<double>& e,
		    const Variable& zAsVar,
		    const vector< vector<double> >& x, const vector<double> xSS,
		    const Dataset& data)
{

  // need weights in computing IP, avoid explicit z?

  vector<double> z = realization(z,data);
  for (int j=0; j<x.size(); ++j) {
    double b = inner_product(z, x[j], 0.0) / xSS[j];
    transform(z.begin(), z.end(), x[1].begin(), z.begin(), SubtractBX(b));
  }
  return inner_product(e.begin(), e.end(), z.begin(), 0.0)/data.sum_weights();
}
  
*/
