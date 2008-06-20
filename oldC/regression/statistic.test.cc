/*  $Id: statistic.test.cc,v 1.8 2003/01/09 00:33:21 bob Exp $
 
    Code to test the use of statistics functions.

*/
#include <iostream>
#include <time.h>
#include <numeric>

#include "statistic.h"
#include "range.h"
#include "random.h"
#include "function_utils.h"
#include "print_utils.h"

RandomGenerator ranGen(12);

double genVal(int column)
{ return column + ranGen.normal(); }

int main()
{
  // Create a dataset from reference file or randomly
  // Dataset data(25000,10,genVal);
  Dataset data("test/statistic");
  
  // Make a few variables and work on them one at a time
  Variable y   = make_index_variable(0);
  Variable x1  = make_index_variable(1);
  Variable x2  = make_index_variable(2);

  Variable xt  = make_transformed_variable(Function_Utils::Sqrt(), x1, "sqrt");

  Variable xm  = make_binary_variable(std::multiplies<double>(),x1,x2,"*");
  Variable xs  = make_binary_variable(std::plus<double>(),x1,x2,"+");
  
  {
    std::cout << "\n   Basic operations on variables:" << std::endl;
    
    double avg = raw_average(y, data);
    std::cout << "Variable: " << y << std::endl;
    std::cout << "Realization: " << realization(y,data) << std::endl;
    std::cout << "Raw average = " << avg << std::endl;
    std::cout << "   variance = " << raw_variance(y,avg,data)
	      << "   SD = " << raw_SD(y,data) << std::endl;
    
    std::cout << "Wtd average = " << average(y, data) << std::endl;
    std::cout << "   variance = " << variance(y,data)
	      << " TSS = " << sum_of_squares (y,average(y,data),data) << std::endl;
  }

  {
    std::cout << "\n Now make a container of variables ..." << std::endl;
    std::vector<Variable> varVec;
    varVec.push_back(y);
    varVec.push_back(x1);
    varVec.push_back(x2);
    varVec.push_back(xt);
    varVec.push_back(xs);
    varVec.push_back(xm);
    std::vector<double> avgs(5,0.0);
    average_range_of_variables (varVec, &avgs, data);
    std::cout << "Averages:  " << avgs << std::endl;
  }
  
  {
    std::cout << "Computing covariance \n";
    
    // This version uses a function for y
    clock_t start = clock();
    double total(0.0), corr(0.0);
    for (int j=0; j<10; ++j) {
      corr = correlation (y, y, data);
      total += corr;
    }
    clock_t ticks = clock() - start;      // or  / CLOCKS_PER_SEC;
    std::cout << "Corr(Y,X) = " << corr << " in " << ticks << " ticks" << std::endl;
  }

  {
    std::cout << "Computing covariance vector\n" ;
    double yBar (average(y,data));
    std::vector<Variable> xx; // avoid empty constructor and do not initialize size unless use *reserve*
    std::vector<double> xBar;      
    xx.push_back(x1); xBar.push_back(average(x1,data));
    xx.push_back(x2); xBar.push_back(average(x2,data));
    double cp[2];
    cross_product_vector(y, yBar, xx, xBar, std::make_pair(cp,cp+2), data);
    std::cout << "covariances:  " << cp[0] << " " << cp[1] << std::endl;
  }
  
  return 0;
}
