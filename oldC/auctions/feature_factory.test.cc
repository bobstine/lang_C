// $Id: feature_factory.test.cc,v 1.5 2004/08/23 21:34:42 bob Exp $

#include "feature_factory.h"
#include "column.h"

#include "range_ops.h"
#include "anonymous_iterator.h"

#include <functional>
#include <iostream>
#include <fstream>

int
main ()
{
  const int n (25);
  
  // define the base data with name string and vector of numbers
  std::string name1("x1");
  double*     x1 = new double[n];
  double*     x2 = new double[n];
  for (int i=0; i<n; ++i)
  { x1[i] = i;
    x2[i] = x1[i]*x1[i];
  }
  std::cout << "TEST: X1 initialized with name " << name1 << std::endl;
  
  // make vector of columns
  Column  xColumn1  (name1.c_str(), x1, x1+n);
  Column  xColumn2  ("square"     , x2, x2+n);
  std::vector<Column> cv;
  cv.push_back(xColumn1);
  cv.push_back(xColumn2);
  std::cout << "TEST: Made vector of " << cv.size() << " columns.\n";

  // initialize factory and extract initial column features
  FeatureFactory factory(cv);
  std::vector<FeatureABC*> fv = factory.features("ColumnFeature");
  
  // make a linear combination

  std::vector<double>      wts (3); wts[0] = 0.7; wts[1] = 100.0; wts[2] = 10000;
  LinearCombinationFeature* lc(factory.make_linear_combination_feature_ptr(wts, fv));
  std::cout << "TEST: Linear combination feature... \n"
	    << "      " << lc << std::endl;
  lc->write_to(std::cout);
  
  // now make an interaction (that squares)
  
  InteractionFeature* xx (factory.make_interaction_feature_ptr(fv[0], fv[1]));
  std::cout << "TEST: average of xx interaction feature is " << xx->average() << std::endl;
  std::cout << "     " << xx << std::endl;
  
  // an interaction of x with xx
  InteractionFeature* xxx (factory.make_interaction_feature_ptr(fv[0], xx));
  std::cout << "TEST: average of xxx interaction feature is " << xxx->average() << std::endl;
  std::cout << "      " << xxx << std::endl;
  
  // an interaction of xx with xx
  InteractionFeature* xxxx (factory.make_interaction_feature_ptr(xx, xx));
  std::cout << "TEST: average of xxxx interaction feature is " << xxxx->average() << std::endl;
  std::cout << "      " <<  xxxx << std::endl;
  
  // a unary feature
  std::cout << "TEST: Now build unary feature... \n";
  FeatureABC* logF (factory.make_unary_feature_ptr((Function_Utils::LogisticPos) Function_Utils::LogisticPos(), fv[0]));
  std::cout <<   "      " << logF << std::endl;
  
  // and a binary feature
  std::cout << "TEST: Binary feature... \n";
  FeatureABC* xpx (factory.make_binary_feature_ptr(std::plus<double>(), fv[0],fv[0]));
  std::cout <<   "      " << xpx << std::endl;
  
  // and a spline feature
  std::cout << "\nTEST: Spline feature ... \n";
  SmoothingSpline ss(3, x1, x2, n);  // 3 df
  FeatureABC* ssF (factory.make_unary_feature_ptr(ss.spline_operator(), fv[0]));
  std::cout <<   "      " << ssF << std::endl;
  
  // write features to a file
  std::cout << "\nTEST: Writing features to file ... \n";
  const std::string test ("test/features.dat");
  std::ofstream output(test.c_str());
  // fv[0]->write_to(output);
  // fv[1]->write_to(output);
  lc->write_to(output);
  xx->write_to(output);
  xxx->write_to(output);
  xpx->write_to(output);
  logF->write_to(output);
  ssF->write_to(output);
  output.close();

  // now read them back
  std::ifstream input(test.c_str());
  std::cout << "\nTEST: (a) made "; std::cout << "     " << factory.make_feature_ptr_from_stream(input, fv) << std::endl;
  std::cout <<   "TEST: (b) made "; std::cout << "     " << factory.make_feature_ptr_from_stream(input, fv) << std::endl;
  std::cout <<   "TEST: (c) made "; std::cout << "     " << factory.make_feature_ptr_from_stream(input, fv) << std::endl;
  input.close();

  // now read them all back in, using the factory to build those that require arguments
  input.open(test.c_str());
  FeatureFactory::FeatureVector fromFile;
  factory.append_features_from_stream(input, fv, std::back_inserter(fromFile));
  std::cout << "TEST: features from file are ... \n" << fromFile << std::endl;
  
  return 0;
}
    
