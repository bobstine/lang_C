/*  $Id: gsmodel.test.cc,v 1.1 2005/06/13 20:47:51 bob Exp $

    Code to test the use of statistics functions.
 
*/
#include <iostream>
#include <time.h>
#include <numeric>

#include "gsmodel.h"
#include "statistic.h"
#include "statistic_base.h"
#include "utility.h"
#include "range.h"

int main()
{
  Dataset data("test/gsmodel");

  // RandomGenerator rand(12);
  // Dataset data(25000,10,rand);
  
  // Make a few variables and work on them one at a time
  Variable y(0);
  Variable x1(1);
  Variable x2(2);
  Variable xx (1,1);
  
  // Build and print base GS model
  GSModel regr(y, data, "/home/bob/C/mgs-sweep3/test");
  cout << regr;

  // First add one predictor, consider second, then add it
  regr.add_variable(x1);
  cout << regr;
  cout << regr.evaluate_predictor(x2);
  regr.add_variable(x2);
  cout << regr;
  
  // Evaluate interaction predictor
  cout << regr.evaluate_predictor(xx);
  regr.add_variable(xx);
  cout << regr;
  
  return 0;
}
