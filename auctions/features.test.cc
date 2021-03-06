#include "features.Template.h"
#include "column.Template.h"
#include "column_stream.Template.h"

#include "smoothing_spline.h"
#include "range_ops.h"
#include "anonymous_iterator.h"
#include "debug.h"

#include <functional>   // std::function
#include <iostream>
#include <fstream>

typedef float Scalar;

std::map<std::string,std::vector<Scalar>>
  make_test_dictionary()
{
  std::map<std::string,std::vector<Scalar>> dict;
  std::vector<Scalar> v (5); v[0]=10.0; v[1]=11.0; v[2]=12.0; v[3]=13.0; v[4]=14.0;
  dict["w1"] = v;
  for(size_t i=0; i<v.size(); ++i)  v[i] += 10;
  dict["w2"] = v;
  for(size_t i=0; i<v.size(); ++i)  v[i] += 10;
  dict["w3"] = v;
  for(size_t i=0; i<v.size(); ++i)  v[i] += 10;
  dict["w4"] = v;
  for(size_t i=0; i<v.size(); ++i)  v[i] += 10;  
  dict["w5"] = v;
  for(size_t i=0; i<v.size(); ++i)  v[i] += 100;  
  dict["OOV"] = v;  
  for(size_t i=0; i<v.size(); ++i) v[i] = std::nanf("missing"); 
  dict["NA"] = v;
  return dict;
}

int 
main ()
{
  const int n (20);
  debugging::debug_init(std::clog, 5);

  // define the base data with name string and vector of numbers
  std::string name1("x1");
  std::string name2("x2");
  std::string ind1("i1");    // mutually exclusive dummy
  std::string ind2("i2");
  Scalar*     x1 = new Scalar[n];
  Scalar*     x2 = new Scalar[n];
  Scalar*     i1 = new Scalar[n];
  Scalar*     i2 = new Scalar[n];
  Scalar*     ii = new Scalar[n];
  for (int i=0; i<n; ++i)
  { x1[i] = (Scalar) i;
    x2[i] = (Scalar) (2 * i);
    i1[i] = (Scalar) (i % 2);
    i2[i] = (Scalar) (1 - i1[i]);
    ii[i] = (Scalar) (n-i-1);
  }
  std::cout << "TEST: X initialized with name1 " << name1 << " and name2 " << name2 << std::endl;  
  
  // make columns
  Column<Scalar>  xColumn1  (name1.c_str(), "first column", n, x1);
  Column<Scalar>  xColumn2  ("duplicate", "second column", n, x1);
  Column<Scalar>  xColumn3  ("has_role_x", "role x", n, x1);
  Column<Scalar>  xColumn4  ("has_role_x2", "  role x ", n, x1);  // blanks were causing empty attribute
  Column<Scalar>  x2Column  (name2.c_str(), "x2 column",n, x2);
  Column<Scalar>  i1Column  ("Dummy_1", "i1", n, i1);
  Column<Scalar>  i2Column  ("Dummy_2", "i2", n, i2);
  Column<Scalar>  iiColumn  ("Dummy_2", "i2", n, ii);
  IntegerColumn indices (iiColumn);
  std::cout << xColumn1 << std::endl << x2Column << std::endl << i1Column << std::endl << i2Column << std::endl << iiColumn << std::endl;
  
  // feature source
  std::vector<Column<Scalar>> colVec;
  colVec.push_back(xColumn1);
  colVec.push_back(xColumn2);
  colVec.push_back(xColumn3);
  colVec.push_back(xColumn4);
  colVec.push_back(x2Column);

  std::cout << "TEST: Making a FeatureSource from vector of columns" << std::endl;
  FeatureSource fs(colVec,0);
  fs.print_summary(std::cout);

  // make a feature from a column, add some attributes
  Feature x   (xColumn1);
  Feature xx2 (x2Column);
  Feature dup (xColumn2);
  x->set_attribute ("test", "value of test attribute");
  x->set_attribute ("test_int", "23423");
  x->set_attribute ("test_scl", "234.235");
  x->set_model_results(true, (Scalar)0.04);
  std::cout << "TEST: average of feature is " << x->center() << std::endl;
  std::cout << "      feature attribute {test}     = " << x->has_attribute("test") << std::endl;
  std::cout << "      feature attribute {test2}    = " << x->has_attribute("test2") << std::endl;
  std::cout << "      feature attribute {test}     = " << x->attribute_str_value("test") << std::endl;
  std::cout << "      feature attribute {test_int} = " << x->attribute_int_value("test_int") << std::endl;
  std::cout << "      feature attribute {test_scl} = " << x->attribute_scalar_value("test_scl") << std::endl;
  
  // dummy variable features
  Feature d1 (i1Column);
  Feature d2 (i2Column);
  d1->set_attribute("parent", "group");
  d2->set_attribute("parent", "group");

  if (true)     // find name in feature vector; check that tracks state
  {
    std::cout << "\nTEST: Testing use of reference to track state of feature.\n";
    std::cout <<   "    : x int attribute is " << x->attribute_str_value("test_int") << std::endl;
    std::vector<Feature> fv;
    fv.push_back(x);
    fv.push_back(xx2);
    fv[0]->set_attribute("test_int"," 777 ");
    std::cout <<   "    : x int attribute is " << x->attribute_str_value("test_int") << std::endl;
  }

  if (false)          // a lag feature
  {
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
  
  if (false)           // unary features
  {
    std::cout << "\nTEST: Now build unary feature... \n";
    std::cout << "  x      " << x   << std::endl;
    Feature xSq ((Function_Utils::Square) Function_Utils::Square(), x);
    std::cout << "  xSq    " << xSq << std::endl << std::endl;
    xSq->write_to(std::cout);
    // a unary feature from a lambda function (compiles, but gives up the useful name/symbol decoration
    Feature xLambda (std::function<Scalar(Scalar)>([](Scalar x) -> Scalar { return x*x; }), x);
    std::cout << "  xLambda    " << xLambda << std::endl << std::endl;
    // make an interaction with the unary feature
    Feature prod (xx2, xSq);
    std::cout << "TEST: interaction of " << xx2->name() << " with " << xSq->name() << " is " << prod << std::endl;
  }
  
  if (false)    // indexed feature
  {
    std::cout << "\nTEST: indexed features (reverse):\n"
	      << make_indexed_feature(x   , indices) << std::endl;
    std::cout << make_indexed_feature(xx2 , indices) << std::endl;
    std::cout << make_indexed_feature(d1  , indices) << std::endl;
    std::cout << std::endl;
  }
  
  if (false)    //  interactions
  {
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
  }

  if (false)      // a linear combination of several
  {
    std::vector<Scalar>      wts (3); wts[0] = (Scalar)0.7; wts[1] = (Scalar)100.0; wts[2] = (Scalar)10000;
    std::vector<Feature> fv (2); fv[0] = dup ; fv[1] = x;
    Feature lc(x->size(), wts, fv);
    std::cout << "TEST: Linear combination feature with center " << lc->center() << std::endl;
    std::cout << "      " << lc << std::endl;			       
    // add unary feature as in logistic predictions
    std::cout << "\n\n\nTEST: Now build unary composition features... \n";
    Feature lOflc ( (Function_Utils::LogisticPos) Function_Utils::LogisticPos(), lc);
    std::cout <<   "      " << lOflc << std::endl;
    lOflc->write_to(std::cout);
  }
  
  if (false)       // and a binary feature
  {  
    std::cout << "\nTEST: Now a binary feature... \n";
    Feature  xpx  (std::plus<Scalar>(), x, x );
    std::cout <<   "      " << xpx << std::endl;
  }
  
  if (false)       // and a spline feature (a very messy unary feature)
  {
    int df = 5;  
    std::cout << "\nTEST: Making spline feature with " << df << " df ... \n";
    SmoothingSpline ss(3, begin(x->range()), begin(x->range()), n);  // 3 df
    Feature spline (ss.spline_operator(), x);
    std::cout <<   "      " << spline << std::endl;
    spline->write_to(std::cout);
  }
  
  if (false)          // write features to a file
  {
    std::ofstream output("test/features.dat");
    x->write_to(output);
    xx2->write_to(output);

    output.close();
  }

  if (true)         // read multiple features from file
  { 
    std::cout << "\nTEST: Building features from file of columns ... \n";
    std::pair<size_t,size_t> dim;
    std::vector<Column<Scalar>> xColumns;
    if (false)      // pick numerical or mapped features
    { std::vector<Column<Scalar>> yColumns;
      dim = insert_numerical_columns_from_file("/home/bob/C/ranges/test.data/column_test.dat", 1, back_inserter(yColumns), back_inserter(xColumns));
    }
    else
    { std::ifstream input ("/home/bob/C/ranges/test.data/column_stream_test.dat");
      size_t minCategorySize = 10;
      // note: dim has different meaning here: number created, number offered (mapped features like eigenwords)
      std::cout << "TEST: Reading mapped features from file... \n";
      dim = insert_columns_from_stream(input, minCategorySize, make_test_dictionary(), back_inserter(xColumns));
      std::cout << "TEST: Constructed " << dim.second << " features from input of " << dim.first << " variables.\n";
    }
    Feature xCol0 (xColumns[0]);
    Feature xCol1 (xColumns[1]);
    std::cout << xCol0 << std::endl << xCol1 << std::endl;
    std::cout << "TEST: attributes of feature "  << xCol0->attributes()                     << std::endl;
    std::cout << "      average of feature is "  << xCol0->center()                         << std::endl;
    std::cout << "      frequency of xCol[0]   " << xCol0->attribute_str_value("frequency") << std::endl;
  }  

  delete [] x1;
  return 0;
}
  
  
