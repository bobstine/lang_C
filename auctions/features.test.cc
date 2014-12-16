#include "features.h"
#include "column.h"

#include "smoothing_spline.h"
#include "range_ops.h"
#include "anonymous_iterator.h"

#include <iostream>
#include <fstream>

int 
main ()
{
  const int n (20);
  
  // define the base data with name string and vector of numbers
  std::string name1("x1");
  std::string name2("x2");
  std::string ind1("i1");    // mutually exclusive dummy
  std::string ind2("i2");
  double*     x1 = new double[n];
  double*     x2 = new double[n];
  double*     i1 = new double[n];
  double*     i2 = new double[n];
  double*     ii = new double[n];
  for (int i=0; i<n; ++i)
  { x1[i] = i;
    x2[i] = 2 * i;
    i1[i] = i % 2;
    i2[i] = 1 - i1[i];
    ii[i] = n-i-1;
  }
  std::cout << "TEST: X initialized with name1 " << name1 << " and name2 " << name2 << std::endl;  
  
  // make columns
  Column  xColumn1  (name1.c_str(), "first column", n, x1);
  Column  xColumn2  ("duplicate", "second column", n, x1);
  Column  xColumn3  ("has_role_x", "role x", n, x1);
  Column  xColumn4  ("has_role_x2", "  role x ", n, x1);  // blanks were causing empty attribute
  Column  x2Column  (name2.c_str(), "x2 column",n, x2);
  Column  i1Column  ("Dummy_1", "i1", n, i1);
  Column  i2Column  ("Dummy_2", "i2", n, i2);
  Column  iiColumn  ("Dummy_2", "i2", n, ii);
  IntegerColumn indices (iiColumn);
  std::cout << xColumn1 << std::endl << x2Column << std::endl << i1Column << std::endl << i2Column << std::endl << iiColumn << std::endl;
  
  // feature source
  std::vector<Column> colVec;
  colVec.push_back(xColumn1);
  colVec.push_back(xColumn2);
  colVec.push_back(xColumn3);
  colVec.push_back(xColumn4);
  colVec.push_back(x2Column);

  std::cout << "TEST: Makeing Feature Source from vector of columns" << std::endl;
  FeatureSource fs(colVec,0);
  fs.print_summary(std::cout);
  
  // make a feature from a column, add some attributes
  Feature x   (xColumn1);
  Feature xx2 (x2Column);
  Feature dup (xColumn2);
  x->set_attribute ("test", "value of test");
  x->set_attribute ("test_int", "23423");
  x->set_attribute ("test_dbl", "234.235");
  x->set_model_results(true, 0.04);
  std::cout << "TEST: average of feature is " << x->center() << std::endl;
  std::cout << "      feature attribute {test}     = " << x->has_attribute("test") << std::endl;
  std::cout << "      feature attribute {test2}    = " << x->has_attribute("test2") << std::endl;
  std::cout << "      feature attribute {test}     = " << x->attribute_str_value("test") << std::endl;
  std::cout << "      feature attribute {test_int} = " << x->attribute_int_value("test_int") << std::endl;
  std::cout << "      feature attribute {test_dbl} = " << x->attribute_dbl_value("test_dbl") << std::endl;

  // dummy variable features
  Feature d1 (i1Column);
  Feature d2 (i2Column);
  d1->set_attribute("parent", "group");
  d2->set_attribute("parent", "group");
  
  { // find name in feature vector
    FeatureVector fv;
    fv.push_back(x); fv.push_back(xx2); fv.push_back(dup);
    std::cout << "\nTEST: eligible features are:\n" << fv << std::endl;
    std::cout <<   "TEST: features with 'x' in name:\n" << features_with_name("x", fv) << std::endl;
  }
  
  { // a lag feature
    std::cout << "\nTEST: lag the x feature by 2 and by 4: \n";
    Feature lag2  (x,2);
    Feature lag4  (x,4);
    Feature lag13 (x,1,3);
    Feature lag22 (x,2,2);
    std::cout << "TEST: lag  2  ---> " << lag2  << std::endl;
    std::cout << "TEST: lag  4  ---> " << lag4  << std::endl;
    std::cout << "TEST: lag 3,1 ---> " << lag13 << std::endl;
    std::cout << "TEST: lag 2,2 ---> " << lag22 << std::endl;
  }
  
  // a unary feature
  std::cout << "\nTEST: Now build unary feature... \n";
  Feature xSq ((Function_Utils::Square) Function_Utils::Square(), x);
  std::cout << "      " << xSq << std::endl;
  xSq->write_to(std::cout);  

  // make an interaction with the unary feature
  Feature prod (xx2, xSq);
  std::cout << "TEST: interaction of " << xx2->name() << " with " << xSq->name() << " is " << prod << std::endl;

  { // indexed feature
    std::cout << "\nTEST: indexed features (reverse):\n"
	      << make_indexed_feature(x   , indices) << std::endl;
    std::cout << make_indexed_feature(xSq , indices) << std::endl;
    std::cout << make_indexed_feature(prod, indices) << std::endl;
    std::cout << std::endl;
  }

  
  // make an interaction
  Feature inter (x, dup);
  std::cout << "TEST: center of x x duplicate interaction feature is " << inter->center() << std::endl;
  std::cout << "TEST: begin of x x interaction feature is " << *(inter->begin()) << std::endl;
  std::cout << "      " << inter << std::endl;

  // now another interaction (that squares)
  Feature xx(x, x);
  std::cout << "TEST: center of xx interaction feature is " << xx->center() << std::endl;
  std::cout << "TEST: *begin of xx interaction feature is " << *(xx->begin()) << std::endl;
  std::cout << "      " << xx << std::endl;

  // an interaction of xx with xx
  Feature xxxx(xx, xx);
  std::cout << "TEST: center of xxxx interaction feature is " << xxxx->center() << std::endl;
  std::cout << "      " << xxxx << std::endl;

  // an interaction with odd names
  Feature another(xx2,x);
  std::cout << "TEST: interaction with odd name is " << another << std::endl;

  // interact dummy variables
  Feature zero(d1,d2);
  std::cout << "TEST: interaction of two dummy variables is " << zero << std::endl;
  std::cout << "      with mean value " << zero->average() << std::endl;
  std::cout << "      attributes are " << zero->attributes() << std::endl  << std::endl;
  
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
  /*
  std::cout << "\nTEST: Making the spline feature ... \n";
  SmoothingSpline ss(3, begin(x->range()), begin(xx->range()), n);  // 3 df
  Feature spline (ss.spline_operator(), x);
  std::cout <<   "      " << spline << std::endl;
  spline->write_to(std::cout);
  */
  
  // write features to a file
  std::ofstream output("test/features.dat");
  x->write_to(output);
  xx->write_to(output);
  xxxx->write_to(output);
  xpx->write_to(output);
  // spline->write_to(output);
  output.close();


  // read multiple features from file by converting into columns
  std::cout << "\nTEST: Building features from file of columns ... \n";
  std::pair<int,int> dim;
  std::vector<Column> yColumns;
  std::vector<Column> xColumns;
  dim = insert_columns_from_file("/home/bob/C/ranges/column_test.dat", 1, back_inserter(yColumns), back_inserter(xColumns));
  Feature xCol0 (xColumns[0]);
  Feature xCol1 (xColumns[1]);
  std::cout << xCol0 << std::endl;
  std::cout << "TEST: average of feature is "  << xCol0->center()                         << std::endl;
  std::cout << "      frequency of xCol[0]   " << xCol0->attribute_str_value("frequency") << std::endl;
  
  
  delete [] x1;
  return 0;
}
    
