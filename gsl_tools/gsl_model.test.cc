// $Id: gsl_model.test.cc,v 1.10 2008/01/22 21:15:07 bob Exp $


/*
 
 Note that a new data object is used for each regression that is constructed.
 The regression object uses the data for its own internal purposes and will
 alter data as it chooses.  Don't try to reuse from one model to another.
 
*/


#include "gsl_model.h"

// for simulation
#include "random.h"
// printing vectors
#include "print_utils.h"
// for constant iterator 
#include "iterators.h"

#include <fstream>
#include <iostream>
#include <vector>

#define LEN  100
#define HALF  50

int
main (void)
{
  std::pair<double,double> eval;
  
  std::cout << "TEST: Making uniform generator \n";
  MarsagliaGenerator uni1(17);
  MarsagliaGenerator uni2(3837);
  
  // boolean selection vector (like an APL compress operator); set half false
  bool b[LEN];
  double setToFalse (HALF);
  int indexTrue[HALF], indexFalse[HALF];
  for (int i=LEN; i; --i) {
    b[i-1] = (uni1() > (setToFalse/i)) ? true : false;
    if (!b[i-1]) setToFalse = setToFalse-1.;
  }
  std::cout << "Set to false should be zero and is " << setToFalse << std::endl;
  for (int i=0,iTrue=0, iFalse=0; i<LEN; ++i) {
    if (b[i]) {
      indexTrue[iTrue] = i;
      ++iTrue;
    } else {
      indexFalse[iFalse]=i;
      ++iFalse;
    }
  }
  // make response related to first predictor; echo data file to output
  // double up the data so that the validation (excluded) values are same
  double x1[LEN], x2[LEN], x3[LEN];
  double y[LEN];
  double weights[LEN];
  double p1;
  for (int i=0; i<HALF; ++i) {
    int j = indexTrue[i];
    int k = indexFalse[i];
    weights[k] = weights[j] = (i+1) * 0.10;
    x1[j] = x1[k] = i;                          // x1 is trend
    x2[j] = x2[k] = x1[i]*x1[i];                // x2 is x1^2
    x3[j] = x3[k] = 10.0 * uni2();
    p1 = x1[j]/LEN; 
    p1 = p1 * (1-p1) * 4.0;
    if ( uni1() < p1 )  y[j] = y[k] = 1.0;
    else                y[j] = y[k] = 0.0;
  }
  // print the data to check that split with duplicates
  if (false)
    for (int i = 0; i<LEN; ++i) {
      std::cout << "DATA: " << y[i] << " " << x1[i] << " " << x2[i] << " " << x3[i] << " ";
      if  (b[i])
	std::cout << "train\n";
      else
	std::cout << "test\n";
    }
  
  // Use this to fill slot for boolean CV selection  
  constant_iterator<bool> noSelection(true);
  // This iterator used in setting initial weights in logistic
  constant_iterator<double> equalWeights (1.0);
  
  // gslData  theData(y, noSelection, weights, LEN, gslRegression_Max_Q);  // no subsetting
  bool protection (3);
  
  // Test adding a sequence of variables to a linear regression
  if (true) {
    gslData  theData(y, b, weights, LEN, gslRegression_Max_Q);            // uses b to subset data
    LinearModel <gslData, olsEngine> regr(&theData, protection);
    std::cout << regr;
      
    // add predictor, check with usual F test and bennett of Z[0]
    std::cout << "TEST: Evaluate first predictor.\n";
    std::cout << "TEST: " << regr.add_predictor_if_useful("x1", x1, 0.05);
    std::cout << regr;
    
    // check the calibration of the current model
    // std::cout << "TEST: With one predictor, calibration test produces " << regr.check_calibration() << std::endl;
    
    // add second predictor
    std::cout << "TEST: Evaluate second predictor.\n";
    std::cout << "TEST: " << regr.add_predictor_if_useful("x2", x2, 0.05);
    std::cout << regr;
    
    // check the calibration of the current model
    // std::cout << "TEST: With two predictors, calibration test produces " << regr.check_calibration() << std::endl;
    
    // save model with 2 predictors
    gslRegressionState state (regr.state());
    
    // add third predictor
    std::cout << "TEST: Evaluate third predictor.\n";
    std::cout << "TEST: " << regr.add_predictor_if_useful("x3", x3, 0.05);
    std::cout << regr;

    
    // write data to desktop for JMP
    std::ofstream out ("/Users/bob/Desktop/test.dat");
    regr.write_data_to (out);
    
    // read predictions from the model
    std::vector<double> preds (LEN);
    regr.fill_with_fit (preds.begin());
    if (false) {
      std::cout << "TEST: Predictions are \n" ;
      std::copy (preds.begin(), preds.begin() + LEN, std::ostream_iterator<double>(std::cout, "\n"));
      std::cout << std::endl;
    }
    // compare MSE in training data to MSE in test data
    double ssTrain (0.0);
    double ssTest  (0.0);
    for (int i=0; i<LEN; ++i) {
      double dev = (preds[i]-y[i]);
      if(b[i]) {
	ssTrain += dev * dev;
      } else {
	ssTest += dev * dev;
      }
    }
    std::cout << "\n\nTEST: SS error in training data: " << ssTrain << "     In test data: " << ssTest << "\n\n";
    
    // restore smaller model
    regr.restore_state(state);
    std::cout << regr;
    
    //  Now with all 3 at once
    LinearModel <gslData, olsEngine> regr3(&theData, protection);
    std::vector< std::pair<std::string, double*> > predictors;
    predictors.push_back(std::make_pair("x1", x1));
    predictors.push_back(std::make_pair("x2", x2));
    predictors.push_back(std::make_pair("x3", x3));
    std::cout << "\nTEST: Evaluate all 3 predictors.\n";
    regr3.add_predictors_if_useful(predictors, 0.5);
    std::cout << regr3 << std::endl;
  }

    
  std::cout << "\n\n\n\nTEST: Testing logistic regression: \n";  
  if (true) {  
    gslData  theData(y, b, equalWeights, LEN, gslRegression_Max_Q); 
    std::cout << "TEST: Add sequence of predictors to a logistic regression.\n\n";
    LogisticModel <gslData> regr(&theData, protection);
    std::cout << regr;
    
    // Evaluate a predictor
    std::cout << "TEST: about to evaluate first predictor.\n";
    regr.add_predictor_if_useful("x1", x1,1.1); 
    std::cout << regr;
 
    // Check that adding predictor again triggers singularity
    std::cout << "TEST: about to evaluate redundant predictor.\n";
    regr.add_predictor_if_useful("x1", x1,1.1);  
    std::cout << regr;
 
    // Evaluate second predictor
    std::cout << "TEST: about to evaluate second predictor.\n";
    regr.add_predictor_if_useful("x2", x2,1.1);  
    std::cout << regr;
    
    // Evaluate third predictor
    std::cout << "TEST: about to evaluate third predictor.\n";
    regr.add_predictor_if_useful("x3", x3,1.1);  
    std::cout << regr;

    // write predictions if need to cut and paste to other application
     std::vector<double> preds (LEN);
     regr.fill_with_fit (preds.begin());
     if (false) {
       std::cout << "TEST: Predictions are \n" ;
       std::copy (preds.begin(), preds.begin() + LEN, std::ostream_iterator<double>(std::cout, "\n"));
       std::cout << std::endl;  
     }
    // compare MSE in training data to MSE in test data
    double ssTrain (0.0);
    double ssTest  (0.0);
    for (int i=0; i<LEN; ++i) {
      double dev = (preds[i]-y[i]);
      if(b[i]) {
	ssTrain += dev * dev;
      } else {
	ssTest += dev * dev;
      }
    }
    std::cout << "\n\nTEST: SS error in training data: " << ssTrain << "     In test data: " << ssTest << "\n\n";

    return 0;
  }

  if (false) {
    std::cout << "\n\nTEST: Add a bundle of 3 predictors at once.\n";
    // Start by building the data.  
    gslData  theData(y, b, equalWeights, LEN, gslRegression_Max_Q); 
    LogisticModel <gslData> regr(&theData, protection);
    std::cout << regr;
    
    // Add predictors, three at a time.
    std::vector<std::pair<std::string, double*> > predictors;
    predictors.push_back(std::make_pair("x1",x1));
    predictors.push_back(std::make_pair("x2",x2));
    predictors.push_back(std::make_pair("x3",x3));
    regr.add_predictors_if_useful(predictors, 0.5);
    std::cout << regr << std::endl;  
  }
  
  return 0;
}

