// $Id: statistic.cc,v 1.10 2003/05/05 22:14:07 bob Exp $

#include <iostream>
#include <functional>
#include <algorithm>
#include <numeric>
#include <utility>

#include "statistic.h"
// #include "statistic_base.h"  not used now
#include "range_traits.h"
#include "range.h"
#include "range_ops.h"
#include "function_utils.h"
#include "compose.h"  // boost

/////////////////////////

using range_ops::transform;
using range_ops::inner_product;
using range_ops::weighted_inner_product;
using range_ops::accumulate;
// using range_ops::accumulate_table;
// using range_ops::accumulate_weighted_table;

/////////////////////////  Weighted mean and variance  ///////////////////

double
average (Variable var, const Dataset& data)
{
  return accumulate(make_weighted_range(var,data),0.0) / data.sum_weights();
}
					
double
variance (Variable var, const double yBar, const Dataset& data)
{
  return sum_of_squares(var,yBar,data) / (data.sum_weights() - 1.0);
}

double
sum_of_squares (Variable var, double center, const Dataset& data)
{
  return
    accumulate (make_weighted_range(
				    boost::compose_f_gx(
							Function_Utils::CenterSquare(center),
							var),
				    data
				    ),
		0.0);
}

//       This is the former way to find the avgs... replaced by template with ranges.
// 
// std::vector<double>
// average_vector (const std::vector<Variable>& varVec, const Dataset& data)
// {
//   std::vector<double> total (varVec.size(),0.0);
//   accumulate_weighted_table (make_unary_range(make_function_range<Observation>(varVec), data),
// 			     make_writeable_range(&total),
// 			     data.weights());
//   transform (make_range(total),
// 	     make_writeable_range(&total),
// 	     std::bind2nd(std::divides<double>(), data.sum_weights()));
//   return total;
// }



 ///////////////////////  Covariance  /////////////////////////////////////

namespace  {

  class ProductVar : public std::unary_function<Observation, double>
  {
    Variable mVar1;
    Variable mVar2;
  public:

    ProductVar(Variable var1, Variable var2)
      : mVar1(var1), mVar2(var2) { }

    double operator()(const Observation& obs) const
    { return mVar1(obs) * mVar2(obs); }

  };

  class ProductVarConstructor : public std::unary_function<Variable,ProductVar>
  {
    Variable mVar;
  public:
    
    ProductVarConstructor(Variable var)
      : mVar(var) { }
    
    ProductVar
    operator()(Variable var) const
    { return ProductVar(mVar,var); }
  };
  
}

double
cross_product (Variable y, const double yBar, Variable z, const double zBar,
	       const Dataset& data)
{
  return
    inner_product(
		  make_unary_range( ProductVar(y, z), data),
		  data.weights(),
		  0.0)
    - data.sum_weights() * yBar * zBar;
}

/*
  void
  cross_product_vector (Variable y, double yBar,
		      const std::vector<Variable>& z, const std::vector<double>& zBar,
		      std::pair<double *,double *> cpVector,
		      const Dataset& data)
{
  // form range of Y * Z_j  
  std::vector<ProductVar> products;
  transform(begin(z), end(z), std::back_inserter< std::vector<ProductVar> >(products),
	    ProductVarConstructor(y));
  // put products of variables into a table and accumulate
  accumulate_weighted_table (make_unary_range(make_function_range<Observation>(products), data),
			     cpVector, // is a writeable range
			     data.weights());
  // subtract n y_bar z_jbar
  std::vector<double> meanProd(z.size(),0.0);
  transform(make_range(zBar), make_writeable_range(&meanProd),
	    std::bind1st(std::multiplies<double>(), data.sum_weights() * yBar));
  range_ops::transform (cpVector, make_range(meanProd), cpVector,
	     std::minus<double>());
}
  */

double
covariance (Variable e, Variable z, const Dataset& data)
{
  return
    weighted_inner_product(
			   make_unary_range(e,data),
			   make_unary_range(z,data),
			   data.weights(),
			   0.0
			   )
    / (data.sum_weights()-1.0);
}



// std::vector<double> 
// covariance (Variable y, const double yBar,   // short-cut without z-bar
// 	    const std::vector<Variable>& z, const Dataset& data)
// {
//   std::vector<double> initial (z.size(),0.0);
//   std::vector<double> crossProd = accumulate_range
//     (make_weighted_row_range(make_range(z), data.range(), // weight rows defined by z by wts*(y-yBar)
// 			make_weighted_unary_range(boost::compose_f_gx(Function_Utils::Center(yBar),y),
// 						  data.range(), data.weights())),
//      make_range(initial));
//   std::clog << "total after accumulate in vector covariance is " << crossProd;
//   transform (crossProd.begin(), crossProd.end(), crossProd.begin(),
// 	     bind2nd(divides<double>(), data.sum_weights()-1.0));
//   return crossProd;
// }

// ////////////////////////  Partial Covariance  /////////////////////////////

// /*
// namespace
// {
//   class SubtractBX {
//     double mB;
//   public:
//     SubtractBX (double b) : mB(b) { }
//     double operator(double z, double x) { return z - mB * x; }
//   };
// }

// double
// partial_covariance (const std::vector<double>& e,
// 		    Variable zAsVar,
// 		    const std::vector< std::vector<double> >& x, const std::vector<double> xSS,
// 		    const Dataset& data)
// {

//   // need weights in computing IP, avoid explicit z?

//   std::vector<double> z = realization(z,data);
//   for (int j=0; j<x.size(); ++j) {
//     double b = inner_product(z, x[j], 0.0) / xSS[j];
//     transform(z.begin(), z.end(), x[1].begin(), z.begin(), SubtractBX(b));
//   }
//   return inner_product(e.begin(), e.end(), z.begin(), 0.0)/data.sum_weights();
// }
  
// */
