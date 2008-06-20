/*  $Id: model.test.cc,v 1.7 2003/02/09 20:03:13 bob Exp $

    Code to test the use of statistics functions.
 
*/
#include <iostream>
#include <time.h>
#include <numeric>

#include "model.h"
#include "statistic.h"
#include "random.h"
#include "range.h"
#include "print_utils.h"

RandomGenerator mRand(463);

double genVal(int column)
{ return column + mRand.normal(); }

int main()
{
  Dataset data("test/model");

  //  Dataset data(25000,10,genVal);
  
  // Make a few variables and work on them one at a time
  Variable y  = make_index_variable(0);
  Variable x1 = make_index_variable(1);
  Variable x4 = make_index_variable(4);
  Variable xx = make_binary_variable(std::multiplies<double>(), x4, x4, "*");

  Variable yCopy(y);
  std::cout << "Response variable is " << yCopy << std::endl;
  
  // Build and print base regression model

  Model regr(y, data, "/Users/bob/C/regression/test/");

  std::cout << " *************  Initialized Model  *************** \n"
	    << regr << std::endl;

  std::cout << "Evaluation of " << x1 << std::endl 
    	    << regr.gaussian_predictor_evaluation(x1)
	    << regr.bennett_predictor_evaluation(x1) << std::endl;
  std::cout << " *************  One-predictor Model  *************** \n"
	    << "Adding first, change in SS = " << regr.add_predictor(x1) << std::endl
	    << regr << std::endl;

  std::cout << "Evaluation of " << x4 << std::endl
	    << regr.gaussian_predictor_evaluation(x4)
  	    << regr.bennett_predictor_evaluation(x4) << std::endl;
  std::cout << " *************  2-predictor Model  *************** \n"
	    << "Adding second, change in SS = " << regr.add_predictor(x4) << std::endl
	    << regr << std::endl;  
  
  // Evaluate interaction predictor
  std::cout << "Evaluation of " << xx << std::endl
	    << regr.gaussian_predictor_evaluation(xx)
	    << regr.bennett_predictor_evaluation(xx) << std::endl;
  std::cout << " *************  3-predictor Model  *************** \n"
	    << "Adding third, change in SS = " << regr.add_predictor(xx) << std::endl
	    << regr << std::endl;

  std::cout << "Slopes (including intercept): " << make_range(regr.beta()) << std::endl;

  std::cout.flush();
  return 0;
}
