// $Id: gsl_regr.test.cc,v 1.2 2004/08/23 21:33:36 bob Exp $

#include <iostream>
#include <vector>
#include <numeric>

#include <time.h>

#include "gsl_regr.h"

#define LEN 10

double
avg(double *x)
{
  return std::accumulate(x,x+LEN,0.0)/LEN;
}

double
dot (double *x, double *y)
{
  return std::inner_product(x,x+LEN,y,0.0);
}

int
main (void)
{
  std::pair<double,double> eval;
  
  // make response boolean (for testing with bennett)
  double y[LEN] = {0, 0, 0, 1, 0, 1, 1, 0, 1, 1};
  double yBar(avg(y));
  double ySS(dot(y,y)-LEN*yBar*yBar);
  
  gslRegression regr(LEN, y, yBar, ySS);
  std::cout << regr;

  // add first predictor, making sure SS are centered
  double  x1[LEN] = {0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0};
  double x1Bar (avg(x1));
  double x1SS  (dot(x1,x1) - LEN*x1Bar*x1Bar);
  double x1y   (dot(x1,y)-LEN*yBar*x1Bar);

  eval = regr.Gaussian_evaluation(x1SS, NULL, x1y);
  std::cout << "Evaluation gives [" << eval.first << " p=" << eval.second << "]" << std::endl;

  eval = regr.Bennett_evaluation(x1, x1Bar, x1SS, NULL);
  std::cout << "Bennett evaluation gives [" << eval.first << " p=" << eval.second << "]" << std::endl;
  const double* gz (regr.gamma());
  std::cout << "Gamma_z for sweeping mean from X1 is {" << gz[0] << "}" << std::endl;

  regr.AddPredictor(x1, x1Bar, x1SS, NULL, x1y);
  std::cout << regr;

  // add second predictor
  double  x2[LEN] = {0.1, 1.1, 2.2, 3.2, 4.1, 5.3, 6.2, 7.1, 8.4, 9.5};
  double x2Bar (avg(x2));
  double x2SS  (dot(x2,x2) - LEN*x2Bar*x2Bar);
  double x2Cov (dot(x1,x2) - LEN*x1Bar*x2Bar);
  double x2y   (dot(x2,y)-LEN*yBar*x2Bar);

  eval = regr.Gaussian_evaluation(x2SS, &x2Cov, x2y);
  std::cout << "Gaussian evaluation gives [" << eval.first << " p=" << eval.second << "]" << std::endl;

  eval = regr.Bennett_evaluation(x2, x2Bar, x2SS, &x2Cov);
  std::cout << "Bennett evaluation gives [" << eval.first << " p=" << eval.second << "]" << std::endl;
  std::cout << "Gamma_z for sweeping X1 from X2 is {" << gz[0] << " " << gz[1] << "}" << std::endl;

  regr.AddPredictor(x2, x2Bar, x2SS, &x2Cov, x2y);
  std::cout << regr;

  // add third predictor
  double x3[LEN] = {10.1, 1.1, 2.2, 3.2, 4.1, 15.3, 6.2, 7.1, 8.4, 19.5};
  double x3Bar (avg(x3));
  double x3SS  (dot(x3,x3) - LEN*x3Bar*x3Bar);
  double x3y   (dot(x3,y)-LEN*yBar*x3Bar);
  double covs[2]; 
  covs[0] = (dot(x1,x3) - LEN*x1Bar*x3Bar);
  covs[1] = (dot(x2,x3) - LEN*x2Bar*x3Bar);

  eval = regr.Gaussian_evaluation(x3SS, covs, x3y);
  std::cout << "Evaluation gives [" << eval.first << " p=" << eval.second << "]" << std::endl;
  regr.AddPredictor(x3, x3Bar, x3SS, covs, x3y);
  std::cout << regr;

  return 0;
}

