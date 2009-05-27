// $Id: my_features.test.cc,v 3.4 2008/01/30 22:39:01 bob Exp $

#include "my_features.h"
#include "column.h"

#include "smoothing_spline.h"
#include "range_ops.h"
#include "anonymous_iterator.h"

#include <gsl/gsl_vector.h>

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
  Column  xColumn1  (name1.c_str(), n, x1);
  Column  xColumn2  ("duplicate", n, x1);
  std::cout << xColumn1 << std::endl;
   
  // make a feature from a column, add some attributes
  Feature x   (xColumn1);
  Feature dup (xColumn2);
  x->add_attribute ("test");
  x->add_attribute ("test_2");
  x->set_model_results(true, 0.04);
  std::cout << "TEST: average of feature is " << x->center() << std::endl;
  std::cout << "      feature attribute {test}  = " << x->has_attribute("test") << std::endl;
  std::cout << "      feature attribute {test2} = " << x->has_attribute("test2") << std::endl;



  // a unary feature
  std::cout << "\nTEST: Now build unary feature... \n";
  Feature x2 ((Function_Utils::Square) Function_Utils::Square(), x);
  std::cout << "      " << x2 << std::endl;
  x2->write_to(std::cout);
  

  // make an interaction
  Feature inter (x, dup);
  std::cout << "TEST: center of x x duplicate interaction feature is " << inter->center() << std::endl;
  std::cout << "      " << inter << std::endl;

  // now another interaction (that squares)
  Feature xx(x, x);
  std::cout << "TEST: center of xx interaction feature is " << xx->center() << std::endl;
  std::cout << "      " << &xx << std::endl;

  // an interaction of xx with xx
  Feature xxxx(xx, xx);
  std::cout << "TEST: center of xxxx interaction feature is " << xxxx->center() << std::endl;
  std::cout << "      " << xxxx << std::endl;

  
  // a linear combination of several
  std::vector<double>      wts (3); wts[0] = 0.7; wts[1] = 100.0; wts[2] = 10000;
  std::vector<Feature> fv (2); fv[0] = dup ; fv[1] = x;
  Feature lc(x->size(), wts, fv);
  std::cout << "TEST: Linear combination feature with center " << lc->center() << std::endl;
  std::cout << "      " << lc << std::endl;
			       
  
  // another unary feature
  std::cout << "\nTEST: Now build unary composition features... \n";
  Feature lOflc ( (Function_Utils::LogisticPos) Function_Utils::LogisticPos(), lc);
  std::cout <<   "      " << lOflc << std::endl;
  lOflc->write_to(std::cout);
  
  // and a binary feature
  std::cout << "\nTEST: Now a binary feature... \n";
  Feature  xpx  (std::plus<double>(), x, x );
  std::cout <<   "      " << xpx << std::endl;

  
  // and a spline feature (a very messy unary feature)
  std::cout << "\nTEST: Making the spline feature ... \n";
  SmoothingSpline ss(3, begin(x->range()), begin(xx->range()), n);  // 3 df
  Feature spline (ss.spline_operator(), x);
  std::cout <<   "      " << spline << std::endl;
  spline->write_to(std::cout);
  
  
  // write features to a file
  std::ofstream output("test/features.dat");
  x->write_to(output);
  xx->write_to(output);
  xxxx->write_to(output);
  xpx->write_to(output);
  spline->write_to(output);
  output.close();

  

  /*
  // make a feature derived from a gsl_vector
    gsl_vector * gv (gsl_vector_alloc(n));
    for (int i=0; i<n; ++i)
    gsl_vector_set(gv,i, x1[i]);
    gslVectorFeature g (gv);
    std::cout << "TEST: " <<  &g << std::endl;
    std::cout << "TEST: average of gsl feature is " << g.center() << std::endl;
  */
  
  return 0;
}
    
