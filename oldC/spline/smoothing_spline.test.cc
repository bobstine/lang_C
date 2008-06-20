/*   $Id: smoothing_spline.test.cc,v 1.11 2004/08/18 22:06:25 bob Exp $

  Test the smoothing spline code.  Runs as naked executable since it generates all
  of the data internally.

  26 Apr 04 ... Created from old xlisp module.
  
*/

#include "smoothing_spline.h"
#include "random.h"
#include <math.h>

#include <iostream>
#include <fstream>

#define LENGTH 2000
#define DF 3

double test_operator (SmoothingSplineOperator const& ssOp, double x)
{ return ssOp(x); }

int
main()
{
  // generate data in random order
  RandomGenerator rand(2312);
  std::vector<double> x(LENGTH);
  std::vector<double> y(LENGTH);
  for (int i=0; i<LENGTH; ++i)
  { x[i] = ((double)round(10. * 5. * rand.normal()))/10.0;
    y[i] = x[i]*x[i] + 25.0*rand.normal();
  }

  // build spline, use it to find several fits of varying smoothness
  SmoothingSpline ss(3*DF,x,y);
  double *smth3DF = new double[LENGTH];
  ss.fill_with_smooth(3*DF, smth3DF);
  double *smthDF = new double[LENGTH];
  ss.fill_with_smooth(2*DF, smthDF);
  double *smth1 = new double[LENGTH];
  ss.fill_with_smooth(DF, smth1);

  // write spline to a file
  std::cout << "TEST: Smoothing spline gives smooth values in file test/ss.out (last ought to be line)\n";
  std::ofstream output("test/ss.txt");
  output << "x_i    y_i     smth_3df    smth_df    smth_1\n";
  for (int i=0; i<LENGTH; ++i)
    output << x[i] << " " << y[i] << "  " << smth3DF[i] << "  " << smthDF[i] << " " << smth1[i] << std::endl;
  output.close();
  
  // make a spline operator
  std::cout << "\nTEST: Making smoothing spline operators.\n";
  SmoothingSplineOperator ssOp (ss.spline_operator());
  std::cout << "TEST: Operator from calculation engine is " << ssOp << std::endl;
  
  // copy a spline operator 
  std::cout << "\nTEST: Copy constructing smoothing spline operator\n";
  SmoothingSplineOperator ssOp1 (ssOp);
  std::cout << "TEST: Operator is " << ssOp1 << std::endl;

  // call operator in function
  std::cout << "\nTEST: Calling operator\n";
  std::cout << "TEST: ssOp(1) = " << test_operator(ssOp, 1.0) << std::endl;
  std::cout << "TEST: ssOp(2) = " << test_operator(ssOp, 2.0) << std::endl;
  
  // write operator to a file, then read in and build a second operator
  std::cout << "\nTEST: Writing operator to file, and then reading in from file (test/ss.dat)\n";
  std::ofstream output2("test/ss.dat");
  ssOp.write_to(output2);
  output2.close();
  std::ifstream input2("test/ss.dat");
  SmoothingSplineOperator ssOp2(input2);
  std::cout << "TEST: Operator from file is " << ssOp2 << std::endl;

  // use both operators to fill in values;  messsage on out of range
  const int npred (7);
  double xx[npred] = {-1.1, 0.0, 1.0, 2.0, 5.0, 10.5, 200.0};
  double sp[npred] ;
  ss.compute_spline_at_x (npred, make_range(xx, xx+npred), sp, 0);
  for (int i=0; i<npred; ++i)
    std::cout << i << " input " << xx[i] << "-->  smth " << sp[i]
	      << "   =   " << ssOp(xx[i])
	      << "   =   " << ssOp1(xx[i])
	      << "   =   " << ssOp2(xx[i]) << std::endl;

  return 0;
}
