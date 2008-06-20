/*  $Id: statistic.test.cc,v 1.14 2002/03/12 18:36:45 bob Exp $

    Code to test the use of statistics functions.

*/
#include <iostream>
#include <time.h>
#include <numeric>

#include "statistic.h"
#include "statistic_base.h"
#include "random.h"
#include "utility.h"

#include "range.h"

int main()
{
  Dataset data("test/statistic");

  // RandomGenerator rand(12);
  // Dataset data(25000,10,rand);
  
  // Make a few variables and work on them one at a time
  Variable y(0);
  Variable x1(1);
  Variable x2(2);
  Variable z1(1,2);        // two ways to make an interaction: constructor
  Variable z2 = x1 * x2;     //              or via a product of two others

  cout << "Variable: " << y << endl;
  cout << "Raw average of      " << y << raw_average(y, data)
       << " variance    "        << raw_variance(y,data) << endl;
  cout << "Weighted average of " << y << average(y, data)
       << " wt variance "        << variance(y,data)
       << " TSS = " << sum_of_squares (y,average(y,data),data) << endl;
     
  
  // Now make a container of variables
  vector<Variable> varVec;
  varVec.push_back(y);
  varVec.push_back(x1);
  varVec.push_back(x2);
  varVec.push_back(z1);
  varVec.push_back(z2);

  cout << "Average vector " << average (varVec, data);

  { // sum of squares
    vector<double> ss = sum_of_squares(varVec,data);
    cout << "SS = " << ss << endl;
    vector<double> wts(data.nRows(),1.0);
    vector<double> wss = weighted_sum_of_squares(varVec,data, wts);
    cout << "wSS= " << wss << endl;
  }
  
  {
    cout << "Computing covariance \n";
    double yBar = average(y, data);
    double xBar = average(x1, data);
    
    // This version uses a function for y
    clock_t start = clock();
    double total(0.0), cov(0.0);
    for (int j=0; j<10; ++j) {
      cov = covariance (y, yBar, x1, xBar, data);
      total += cov;
    }
    clock_t ticks = clock() - start;      // or  / CLOCKS_PER_SEC;
    cout << "Cov(Y,X) = " << cov << " in " << ticks << " ticks" << endl;
  }
  
  { // This block has y as numerical
    clock_t start = clock();
    vector<double> yNum(realization(y,data));
    double xBar = average(x1, data);
    double total = 0.0;
    double cov;
    cout << "Y Num " << yNum << endl;
    for (int j=0; j<10; ++j) {
      cov = covariance (make_range(yNum), x1, xBar, data);
      total += cov;
    }
    clock_t ticks = clock() - start;
    cout << "Cov(Y,X) = " << cov << " in " << ticks << " ticks (10 iterations)" << endl;
  }

  { // Now find covariance vector
    clock_t start = clock();
    double yBar = average(y, data);
    vector<double> covs = covariance(y, yBar, varVec, data);
    clock_t ticks = clock() - start;
    cout << "Cov(Y,vec) = " << covs << " in " << ticks << " ticks" << endl;
  }
  /*
    
    // Finally for correlation
    double corr;
    corr = correlation(y,y,data);
    cout << "Corr(Y,Y) = " << corr << endl;
    corr = correlation(y,x,data);
    cout << "Corr(Y,X) = " << corr << endl;

  */
  return 0;
}
