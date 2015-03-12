/*  -*- c++ -*-

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

typedef SmoothingSplineData::Scalar  Scalar;

Scalar test_operator (SmoothingSplineOperator const& ssOp, Scalar x)
{ return ssOp(x); }

int
main()
{
  // generate data in random order
  RandomGenerator rand(2312);
  std::vector<Scalar> x(LENGTH);
  std::vector<Scalar> y(LENGTH);
  std::vector<Scalar> w(LENGTH);
  for (int i=0; i<LENGTH; ++i)
  { x[i] = (round((Scalar)10. * (Scalar)5. * (Scalar)rand.normal()))/(Scalar)10.0;
    y[i] = x[i]*x[i] + (Scalar)(25.0)*rand.normal();
    w[i] = ((-5 < x[i]) && (x[i] < 5)) ? 5.0 : 1.0;  // larger weight at origin
  }
  
  // build spline, use it to find several fits of varying smoothness
  SmoothingSpline ss(DF,x,y);
  Scalar *smthDF = new Scalar[LENGTH];
  ss.fill_with_smooth(DF, smthDF);
  Scalar *smth3DF = new Scalar[LENGTH];
  ss.fill_with_smooth(3*DF, smth3DF);

  SmoothingSpline ssw(DF,x,y,w);
  Scalar *smthw = new Scalar[LENGTH];
  ssw.fill_with_smooth(DF, smthw);

  // write spline to a file
  std::cout << "TEST: Smoothing spline gives smooth values in file test/ss.out (last ought to be line)\n";
  std::ofstream output("test/ss.txt");
  output << "x_i    y_i     smth_df    smth_3df    smth_w\n";
  for (int i=0; i<LENGTH; ++i)
    output << x[i] << " " << y[i] << "  " << smthDF[i] << "  " << smth3DF[i] << " " << smthw[i] << std::endl;
  output.close();
  
  // make a spline operator    ssOp
  std::cout << "\nTEST: Making smoothing spline operators.\n";
  SmoothingSplineOperator ssOp (ss.spline_operator());
  std::cout << "TEST: Operator from calculation engine is " << ssOp << std::endl;
  
  // copy a spline operator    ssOp1
  std::cout << "\nTEST: Copy constructing smoothing spline operator\n";
  SmoothingSplineOperator ssOp1 (ssOp);
  std::cout << "TEST: Operator is " << ssOp1 << std::endl;

  // call operator in function
  std::cout << "\nTEST: Calling operator\n";
  std::cout << "TEST: ssOp(1) = " << test_operator(ssOp, 1.0) << std::endl;
  std::cout << "TEST: ssOp(2) = " << test_operator(ssOp, 2.0) << std::endl;
  
  // write operator to a file, then read in and build a second operator  ssOp2
  std::cout << "\nTEST: Writing operator to file, and then reading in from file (test/ss.dat)\n";
  std::ofstream output2("test/ss.dat");
  if (output2)
  { ssOp.write_to(output2);
    output2.close();
  }
  else std::cout << "TEST: *** Error *** Could not open file to write results.\n";
  std::ifstream input2("test/ss.dat");
  SmoothingSplineOperator ssOp2(input2);
  std::cout << "TEST: Operator from file is " << ssOp2 << std::endl;

  // use both operators to fill in values;  messsage on out of range
  // results *should* agree for all 4
  const int npred (7);
  Scalar xx[npred] = {-1.1, 0.0, 1.0, 2.0, 5.0, 10.5, 200.0};
  Scalar sp[npred] ;
  ss.compute_spline_at_x (npred, Ranges::make_range(xx, xx+npred), sp, 0);
  for (int i=0; i<npred; ++i)
    std::cout << i << " input " << xx[i] << "-->  smth " << sp[i]
	      << "   =   " << ssOp(xx[i])
	      << "   =   " << ssOp1(xx[i])
	      << "   =   " << ssOp2(xx[i]) << std::endl;

  return 0;
}
 
