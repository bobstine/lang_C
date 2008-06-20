// $Id: feature.test.cc,v 3.0 2004/11/19 18:58:36 foster Exp $

#include "feature.h"
#include "features.h"
#include "column.h"

#include "range_ops.h"
#include "anonymous_iterator.h"

#include <iostream>
#include <fstream>

class Sequence 
{
  Feature              mFeature;
  FeatureVector const& mSource;
  int                  mPosition;

  public:
   Sequence(FeatureVector const& src)
     :  mFeature(), mSource(src), mPosition(0) { make_feature(); }

  //  Feature feature() { Feature f = mSource[mPosition]; return f; }  this worked 
  Feature feature() { return mFeature; }
  
  void make_feature() { mFeature = mSource[mPosition]; std::cout << "Made feature " << mFeature << std::endl; }
};

Feature
pass_through (Feature f)
{
  std::cout << "PASS: incoming feature is " << f << std::endl;
  f.set_model_results (true, 0.05);
  return f;
}

int
main ()
{
  const int n (10);
  
  // define the base data with name string and vector of numbers
  std::string name1("x1");
  double*     x1 = new double[n];
  for (int i=0; i<n; ++i)
    x1[i] = i;
  std::cout << "X initialized with name " << name1 << std::endl;
  
  // make two columns
  Column  xColumn1  (name1.c_str(), x1, x1+n);
  Column  xColumn2  ("duplicate", x1, x1+n);
  std::cout << xColumn1 << std::endl;
      
  // make a feature vector directly from the columns
  std::vector<Column> cols;
  cols.push_back(xColumn1);
  cols.push_back(xColumn2);
  FeatureVector fv (cols);
  std::cout << "TEST: fv = " << fv << std::endl;
  std::cout << "TEST: fv[0] " << fv[0] << std::endl;
  std::cout << "TEST: fv[1] " << fv[1] << std::endl;
  Feature fAt0 = fv[0];
  std::cout << "TEST: fAt0 " << fAt0 << std::endl;

  // put them in a sequence
  std::cout << "TEST: build sequence.\n";
  Sequence sr(fv);
  std::cout << "TEST: extract feature from sequence.\n";
  sr.feature();
  std::cout << sr.feature() << std::endl;
  
  // make feature from a column, add some attributes
  Feature f1   (xColumn1);
  Feature f2   (xColumn2);
  std::cout << "TEST: x is " << f1 << std::endl;
  std::cout << "TEST: average of feature is " << f1.average() << std::endl;
  std::cout << "TEST: sd of feature is      " << f1.std_dev() << std::endl;

  // pass through copy test
  Feature passed = pass_through(f1);
  std::cout << "TEST: Passed feature is " << passed << std::endl;
  
  // make raw interaction
  Feature xd = make_interaction_feature(f1,f2);
  std::cout << "TEST: average of x x duplicate interaction feature is " << xd.average() << std::endl;
  std::cout << xd.range() << std::endl;

  // now make an interaction (that squares)
  Feature xx = make_interaction_feature(f1, f1);
  std::cout << "TEST: average of xx interaction feature is " << xx.average() << std::endl;
  std::cout << xx.range() << std::endl;

  // an interaction of x with xx
  Feature xxx = make_interaction_feature(f1, xx);
  std::cout << "TEST: average of xxx interaction feature is " << xxx.average() << std::endl;
  std::cout << xxx.range() << std::endl;
  
  // an interaction of xx with xx
  Feature xxxx = make_interaction_feature(xx, xx);
  std::cout << "TEST: average of xxxx interaction feature is " << xxxx.average() << std::endl;
  std::cout << xxxx.range() << std::endl;

  // and a binary feature
  Feature xpx = make_binary_feature (std::plus<double>(), f1, f2  );
  std::cout << xpx << std::endl;
  
  // write features to a file
  std::cout << "\nTEST: Writing features to file.\n";
  const std::string test ("test/features.dat");
  std::ofstream output(test.c_str());
  f1.write_to(output);
  xx.write_to(output);
  xpx.write_to(output);
  xxx.write_to(output);
  output.close();

  // now try to read them back
  std::cout << "\nTEST: Now reading from test file.\n";
  std::ifstream input(test.c_str());
  FeatureVector fv2 (input, fv);
  std::cout << "TEST: fv2 " << fv2 << std::endl;

  return 0;
}
    
