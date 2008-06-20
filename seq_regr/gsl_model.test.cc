// $Id: gsl_model.test.cc,v 1.10 2008/01/22 21:15:07 bob Exp $

#include "gsl_model.h"

// for simulation
#include "random.h"
// printing vectors
#include "print_utils.h"
// for constant iterator 
#include "cyclic_iterator.h"

#include <fstream>
#include <iostream>
#include <vector>

#define LEN 100
 
int
main (void)
{
  std::pair<double,double> eval;
  
  std::cout << "TEST: Making uniform generator \n";
  MarsagliaGenerator uni1(17);
  MarsagliaGenerator uni2(3837);
  
  // boolean selection vector (like an APL compress operator); set some to be false
  bool b[LEN];
  for (int i=0; i<LEN; ++i)
    b[i]= (uni1() < 0.5) ? true : false;
  
  // make response related to first predictor; echo data file to output
  double x1[LEN], x2[LEN], x3[LEN];
  double y[LEN];
  double weights[LEN];
  double p1;
  for (int i=0; i<LEN; ++i)
  { weights[i] = (i+1) * 0.10;
    x1[i] = i;                          // x1 is trend
    x2[i] = x1[i]*x1[i];                // x2 is x1^2
    x3[i] = 10.0 * uni2();
    p1 = x1[i]/LEN; 
    p1 = p1 * (1-p1) * 4.0;
    if ( uni1() < p1 )  y[i] = 1.0;
    else                y[i] = 0.0;
  }

  // Use this to fill slot for boolean CV selection  
  constant_iterator<bool> noSelection(true);
  
  { // Test adding a sequence of variables to a linear regression
    
    // Start by building the data.  Then add predictors, one at a time.
    gslData  theData(y, b, weights, LEN, gslRegression_Max_Q);  // subsetting
    // gslData  theData(y, noSelection, weights, LEN, gslRegression_Max_Q);  // no subsetting
    LinearModel <gslData, olsEngine> regr(&theData);
    std::cout << regr;
    
    // add predictor, check with usual F test and bennett of Z[0]
    std::cout << "TEST: Evaluate first predictor.\n";
    regr.evaluate_predictor(x1);
    std::cout << "TEST: " << regr.f_test_evaluation()  << std::endl;
    regr.add_current_predictors();
    std::cout << regr;
    
    // check the calibration of the current model
    std::cout << "TEST: With one predictor, calibration test produces " << regr.check_calibration() << std::endl;
    
    // add second predictor
    std::cout << "TEST: Evaluate second predictor.\n";
    regr.evaluate_predictor(x2);
    std::cout << "TEST: " << regr.f_test_evaluation()  << std::endl;
    regr.add_current_predictors();
    std::cout << regr;
    
    // check the calibration of the current model
    std::cout << "TEST: With two predictors, calibration test produces " << regr.check_calibration() << std::endl;

    // save model with 2 predictors
    gslRegressionState state (regr.state());
    
    // add third predictor
    std::cout << "TEST: Evaluate third predictor.\n";
    regr.evaluate_predictor(x3);
    std::cout << "TEST: " << regr.f_test_evaluation()  << std::endl;
    regr.add_current_predictors();
    std::cout << regr;
    
    // write data to desktop for JMP
    std::ofstream out ("/Users/bob/Desktop/test.dat");
    regr.write_data_to (out);
    
    // read predictions
    std::vector<double> preds (LEN);
    regr.fill_with_predictions (preds.begin());
    std::cout << "TEST: Predictions are \n" ;
    std::copy (preds.begin(), preds.begin() + LEN, std::ostream_iterator<double>(std::cout, "\n"));
    std::cout << std::endl;
    
    // restore smaller model
    regr.restore_state(state);
    std::cout << regr;
    
    
    ////  Now with all 3 at once
    LinearModel <gslData, olsEngine> regr3(&theData);
    std::vector<double*> predictors;
    predictors.push_back(x1);
    predictors.push_back(x2);
    predictors.push_back(x3);
    std::cout << "\nTEST: Evaluate all 3 predictors.\n";
    regr3.add_predictors_if_useful(predictors, 0.5);
    std::cout << regr3 << std::endl;    
 
  }
  
  std::cout << "\n\n\n\n  Next test logistic regression   \n\n\n\n";
  
  { // Test adding a sequence of variables to a logistic regression
    
    // Start by building the data.  Then add predictors, one at a time.
    // Set the initial weights via the constant iterator
    constant_iterator<double> equalWeights (1.0);

    gslData  theData(y, b, equalWeights, LEN, gslRegression_Max_Q); 
    LogisticModel <gslData> regr(&theData);
    std::cout << regr;
    
    // Evaluate a predictor
    std::cout << "TEST: about to evaluate first predictor.\n";
    regr.evaluate_predictor(x2);
    std::cout << "TEST: " << regr.f_test_evaluation() << std::endl;
    regr.add_current_predictors();
    std::cout << "TEST: " << regr.maximize_log_likelihood(1);
    std::cout << regr;
 
    // Evaluate second predictor
    std::cout << "TEST: about to evaluate second predictor.\n";
    regr.evaluate_predictor(x1);
    std::cout << "TEST: " << regr.f_test_evaluation()  << std::endl;
    regr.add_current_predictors();
    std::cout << "TEST: " <<regr.maximize_log_likelihood(1);
    std::cout << regr;
    
    // Evaluate third predictor
    std::cout << "TEST: about to evaluate third predictor.\n";
    regr.evaluate_predictor(x3);
    std::cout << "TEST: " << regr.f_test_evaluation()  << std::endl;
    regr.add_current_predictors();
    std::cout << "TEST: " <<regr.maximize_log_likelihood(1);
    std::cout << regr;

    // read predictions
    std::vector<double> preds (LEN);
    regr.fill_with_predictions (preds.begin());
    std::cout << "TEST: Predictions are \n" ;
    std::copy (preds.begin(), preds.begin() + LEN, std::ostream_iterator<double>(std::cout, "\n"));
    std::cout << std::endl;
    
    
    ////  Now with 2 at once
    LogisticModel <gslData> regr3(&theData);
    std::vector<double*> predictors;
    predictors.push_back(x1);
    predictors.push_back(x2);
    // nan   predictors.push_back(x3);
    std::cout << "\nTEST: Evaluate 2 predictors at once.\n";
    regr3.add_predictors_if_useful(predictors, 0.5);
    std::cout << regr3 << std::endl;    
    
  }
  
  return 0;
}

