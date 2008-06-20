// $Id: my_features.test.cc,v 1.8 2004/08/23 21:34:42 bob Exp $

#include "my_features.h"
#include "column.h"

#include "smoothing_spline.h"
#include "range_ops.h"
#include "anonymous_iterator.h"

#include <iostream>
#include <fstream>

int
main ()
{
  const int n (99);
  
  // define the base data with name string and vector of numbers
  std::string name1("x1");
  double*     x1 = new double[n];
  for (int i=0; i<n; ++i)
    x1[i] = i;
  std::cout << "TEST: X initialized with name " << name1 << std::endl;
  
  // make two columns
  Column  xColumn1  (name1.c_str(), x1, x1+n);
  Column  xColumn2  ("duplicate", x1, x1+n);
  std::cout << xColumn1 << std::endl;
   
  // make a feature from a column, add some attributes
  ColumnFeature dup (xColumn2);
  ColumnFeature x   (xColumn1);
  x.add_attribute ("test");
  x.add_attribute ("test_2");
  x.set_model_results(true, 0.04);
  std::cout << "TEST: " <<  &x << std::endl;
  std::cout << "TEST: average of feature is " << x.average() << std::endl;
  std::cout << "      feature attribute {test}  = " << x.has_attribute("test") << std::endl;
  std::cout << "      feature attribute {test2} = " << x.has_attribute("test2") << std::endl;

  // make an interaction
  InteractionFeature xd(&x, &dup);
  std::cout << "TEST: average of x x duplicate interaction feature is " << xd.average() << std::endl;
  std::cout << "      " << &xd << std::endl;

  // now another interaction (that squares)
  InteractionFeature xx(&x, &x);
  std::cout << "TEST: average of xx interaction feature is " << xx.average() << std::endl;
  std::cout << "      " << &xx << std::endl;

  // a linear combination of several
  std::vector<double>      wts (3); wts[0] = 0.7; wts[1] = 100.0; wts[2] = 10000;
  std::vector<FeatureABC*> fv (2); fv[0] = &dup ; fv[1] = &x;
  double * cp = new double[n];
  Column dest("Destination for lc", cp, cp+n);
  LinearCombinationFeature lc(wts,fv, &dest);
  std::cout << "TEST: Linear combination feature... \n"
	    << "      " << &lc << std::endl;
  lc.write_to(std::cout);
			       
  // an interaction of x with xx
  InteractionFeature xxx(&x, &xx);
  std::cout << "TEST: average of xxx interaction feature is " << xxx.average() << std::endl;
  std::cout << "      " << &xxx << std::endl;
  
  // an interaction of xx with xx
  InteractionFeature xxxx(&xx, &xx);
  std::cout << "TEST: average of xxxx interaction feature is " << xxxx.average() << std::endl;
  std::cout << "      " << &xxxx << std::endl;

  // a unary feature
  std::cout << "\nTEST: Now build unary composition features... \n";
  UnaryFeature< Function_Utils::LogisticPos > lOflc ( (Function_Utils::LogisticPos) Function_Utils::LogisticPos(), &lc );
  std::cout <<   "      " << &lOflc << std::endl;
  lOflc.write_to(std::cout);
  
  // and a binary feature
  std::cout << "\nTEST: Now a binary feature... \n";
  BinaryFeature< std::plus<double> > xpx( (std::plus<double>) std::plus<double>(), &x, &x  ); // need explicit cast
  std::cout <<   "      " << &xpx << std::endl;

  // and a spline feature (a very messy unary feature)
  std::cout << "\nTEST: Making the spline feature ... \n";
  SmoothingSpline ss(3, begin(x.range()), begin(xx.range()), n);  // 3 df
  UnaryFeature< SmoothingSplineOperator > spline (ss.spline_operator(), &x );
  std::cout <<   "      " << &spline << std::endl;
  spline.write_to(std::cout);

  // write features to a file
  std::ofstream output("test/features.dat");
  x.write_to(output);
  xx.write_to(output);
  xxx.write_to(output);
  xpx.write_to(output);
  spline.write_to(output);
  output.close();

  return 0;
}
    
