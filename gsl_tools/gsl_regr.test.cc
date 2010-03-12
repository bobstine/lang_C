#include "gsl_regr.h"
#include "gsl_data.h"
#include "gsl_engine.h"

// #include "debug.h"
#include "random.h"
#include "print_utils.h"


// for constant iterator
#include "iterators.h"


#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <time.h>

#define LEN 120
 

double
avg(double *x, bool *b)
{
  double s(0.0);
  int n(0); 
  for (int i=0; i<LEN; ++i, ++b, ++x)
    if (*b) 
    { ++n;
      s += *x;
    }
  return (s/n);
}

double
dot (double *x, double *y, bool *b)
{
  double s(0.0);
  for (int i=0; i<LEN; ++i, ++b, ++x, ++y)
    if (*b) 
      s += (*x) * (*y);
  return (s);
}

int
main (void)
{
  std::pair<double,double> eval;
  
  
  // make response related to first predictor; echo data file to output
  std::cout << "TEST: Making a uniform generator \n";
  MarsagliaGenerator uni1(17);
  MarsagliaGenerator uni2(3837);
 
  // boolean selection vector (like an APL compress operator); set some to be false
  bool b[LEN];
  for (int i=0; i<LEN; ++i)
    b[i]= (uni1() < 0.5) ? true : false;

  double x1[LEN], x2[LEN], x3[LEN];
  double y[LEN];
  double weights[LEN];
  for (int i=0; i<LEN; ++i)
  { weights[i] = (i+1) * 0.10;
    x1[i] = i;                          // x1 is trend
    x2[i] = x1[i]*x1[i];                // x2 is x1^2
    x3[i] = 10.0 * uni2();
    if ( LEN*uni1() < i )  y[i] = 1.0;
    else                   y[i] = 0.0;
  }
  std::cout << std::endl;
  
  // write data to desktop for other applications
  std::ofstream out ("/Users/bob/Desktop/test_gsl.txt");
  out << "y    x1    x2    x3   weights use\n";
  for (int i=0; i<LEN; ++i)
    out << y[i] << " " << x1[i] << " " << x2[i] << " " << x3[i] << " " << weights[i] << " " << b[i] << std::endl;
  
  // properties of y and first predictor
  double yBar(avg(y,b));
  double ySS(dot(y,y,b)-LEN*yBar*yBar);
  std::cout << "TEST: Externally computed mean and SS of y are " << yBar << ", " << ySS << std::endl;
  double x1Bar (avg(x1,b));
  double x1SS  (dot(x1,x1,b) - LEN*x1Bar*x1Bar);
  double x1y   (dot(x1,y,b)-LEN*yBar*x1Bar);
  std::cout << "TEST: mean of x1 " << x1Bar << "   ss " << x1SS << "  xy " << x1y << std::endl;
  
  // Start by building the data.  Then add predictors, one at a time.  Then add all at once.

  int protection = 2;
  { constant_iterator<bool> noSelection(true);
    
    gslData  theData(y, noSelection, weights, LEN, gslRegression_Max_Q);  // no subsetting
    // gslData  theData(y,       b    , weights, LEN, gslRegression_Max_Q);  // subsetting

    int blockSize (20);
    gslRegression< gslData,olsEngine > regr(&theData, protection, blockSize);
    std::cout << regr;
    
    // add predictor, check with usual F test and bennett of Z[0]
    std::cout << "TEST: about to evaluate first predictor.\n";

    regr.prepare_predictor("X1",x1);
    std::cout << "         F " << regr.f_test( )               << std::endl;
    std::cout << "     White " << regr.white_f_test()          << std::endl;
    std::cout << "   Bennett " << regr.Bennett_evaluation(0,1) << std::endl;
    regr.add_current_predictors();
    std::cout << regr;
    
    // add second predictor
    std::cout << "TEST: about to evaluate second predictor.\n";
    regr.prepare_predictor("X2",x2);
    std::cout << "         F " << regr.f_test()                 << std::endl;
    std::cout << "     White " << regr.white_f_test()           << std::endl;
    std::cout << "   Bennett " << regr.Bennett_evaluation(0,1)  << std::endl;
    regr.add_current_predictors();
    std::cout << regr;
    
    // add third predictor
    std::cout << "TEST: about to evaluate third predictor.\n";
    regr.prepare_predictor("X3",x3);
    std::cout << "         F " << regr.f_test()                 << std::endl;
    std::cout << "     White " << regr.white_f_test()           << std::endl;
    std::cout << "   Bennett " << regr.Bennett_evaluation(0,1)  << std::endl;
    regr.add_current_predictors();
    std::cout << regr;
    
    // read predictions; request for fitted values forces update of the fit
    double theFit[LEN];
    regr.fill_with_fitted_values(theFit);   
    
    double const*pFit (gsl_vector_const_ptr(theData.Xb(),0));
    std::cout << "TEST: First 25 fitted values among the estimation sample are \n" ;
    std::copy (pFit, pFit + 25, std::ostream_iterator<double>(std::cout, " \n"));
    std::cout << std::endl;
    
    std::cout << "TEST: ALL fitted values for full sample are \n" ;
    theData.permuted_copy_to_iterator(theData.Xb(), theFit, LEN);
    std::copy (theFit, theFit + LEN, std::ostream_iterator<double>(std::cout, " \n"));
    std::cout << std::endl;

     
    // read SE
    std::vector<double> stdErr (4);
    regr.fill_with_diagonal_XtXinv (stdErr.begin());
    std::cout << "TEST: SE are " << stdErr << std::endl;
    
    // Add three predictors at once  (Note: y in data has been centered.)
    std::cout << "\n\n\n\n =====================\nTEST: Adding all three at once\n";
    gslRegression<gslData,olsEngine> regr3(&theData, protection, blockSize);
    
    // add predictors
    std::vector< std::pair<std::string, double *> > predVec;
    predVec.push_back(std::make_pair("x1", x1));
    predVec.push_back(std::make_pair("x2", x2));
    predVec.push_back(std::make_pair("x3", x3));
    regr3.prepare_predictors(predVec);
    std::cout << "         F " << regr3.f_test()         << std::endl;
    std::cout << "     White " << regr3.white_f_test()   << std::endl;
    if (eval.first)
      regr3.add_current_predictors();
    std::cout << regr;
  }
  return 0;
}

    /*   
    { // Need to use WLS engine for this block
      regr.reweight(weights);
      std::cout << regr;
      regr.fill_with_predictions (preds.begin());
      std::cout << "TEST: Reweighted, first 10 predictions are " ;
      std::copy (preds.begin(), preds.begin() + 10, std::ostream_iterator<double>(std::cout, " "));
      regr.fill_with_se (stdErr.begin());
      std::cout << "TEST: Reweighted, SE are " << stdErr << std::endl;
    }

  
  { // Add three predictors at init
    std::vector<double *> predVec;
    predVec.push_back(x1); predVec.push_back(x2); predVec.push_back(x3);
    gslRegression<olsEngine> regr(y, b, LEN, 3, predVec);
    std::cout << regr;

    // read SE
    std::vector<double> stdErr (3);
    regr.fill_with_se (stdErr.begin());
    std::cout << "TEST: SE are " << stdErr << std::endl;
  }
  */
