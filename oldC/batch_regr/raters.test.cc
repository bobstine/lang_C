// $Id: raters.test.cc,v 1.5 2003/06/21 12:23:33 bob Exp $

#include <iostream>
#include <iterator>
#include <string>
#include <algorithm>

#include "datasets.h"
#include "covariance_matrix.h"
#include "sweep_matrix.h"
#include "raters.h"

template<typename T>
std::ostream&
operator<< (std::ostream& os, std::vector<T> x)
{
  std::copy(x.begin(), x.end(), std::ostream_iterator<T>(os," "));
  os << std::endl;
  return os;
}


// std::string fileName("test/covar.dat");
// bool transposed (false);

std::string fileName ("/Users/bob/raslisp/stepwise/C/wind/wind2/6weeks/est.dat");
bool transposed (true);


int main (void)
 {
   NumericDataset data (fileName, transposed);
   int p (data.n_cols()-1);
   CovarianceMatrix<NumericDataset> covMat(data);
   SweepMatrix sm(covMat.cross_products(0), covMat.sums_of_squares(), data.average_vector());
   std::cout << sm;

   // build coding rater
   TwoPartCodingRater coder(sm, data.n_rows());
   std::vector<int> picks ( coder.recommend_predictors(2) );
   std::clog << "Coder picks " << picks << std::endl;

   // find the first predictor using stepwise rater
   double pValue (1.0/p);
   StepwiseRater step(sm, data.n_rows());
   picks = step.recommend_predictors(2, pValue);
   std::clog << "Standard stepwise rater picks " << picks << std::endl;

   // add chosen predictor
   sm.add_predictor(picks[0], covMat.cross_products(picks[0]));
   std::cout << sm;
   std::cout << "Regression has slopes " << sm.slopes() << std::endl;

   
   // adjust p-value
   pValue = 2.0/p;
   
   // find the second predictor, first via usual stepwise
   picks = step.recommend_predictors(2,pValue);
   std::clog << "Standard stepwise rater picks "<< picks << std::endl;

   // then by coder
   picks = coder.recommend_predictors(2);
   std::clog << "Coding rater picks "<< picks << std::endl;
   
   // then by white
   WeightedRater<NumericDataset> white (make_white_rater(sm, data));
   picks =  white.recommend_predictors(2,pValue);
   std::clog << "White rater picks " << picks << std::endl;
   
   // then by binomial
   WeightedRater<NumericDataset> bino (make_binomial_rater(sm, data));
   picks = bino.recommend_predictors(2,pValue);
   std::clog << "Binomial rater picks " << picks << std::endl;
   
   // then by bennett
   BennettRater<NumericDataset> bennett (make_bennett_binomial_rater(sm, data));
   picks = bennett.recommend_predictors(2,pValue);
   std::clog << "Bennett/Binomial rater picks " << picks << std::endl;

   // add chosen predictor
   sm.add_predictor(picks[0], covMat.cross_products(picks[0]));
   std::cout << sm;
   std::cout << "Regression has slopes " << sm.slopes() << std::endl;



   // adjust p-value
   pValue = 3.0/p;
   
   // find the second predictor, first via usual stepwise
   picks = step.recommend_predictors(2,pValue);
   std::clog << "Standard stepwise rater picks "<< picks << std::endl;

   // then by coder
   picks = coder.recommend_predictors(2);
   std::clog << "Coding rater picks "<< picks << std::endl;
   
   // then by white
   picks =  white.recommend_predictors(2,pValue);
   std::clog << "White rater picks " << picks << std::endl;
   
   // then by binomial
   picks = bino.recommend_predictors(2,pValue);
   std::clog << "Binomial rater picks " << picks << std::endl;
   
   // then by bennett
   picks = bennett.recommend_predictors(2,pValue);
   std::clog << "Bennett/Binomial rater picks " << picks << std::endl;

   // add chosen predictor
   sm.add_predictor(picks[0], covMat.cross_products(picks[0]));
   std::cout << sm;
   std::cout << "Regression has slopes " << sm.slopes() << std::endl;

   

   // adjust p-value
   pValue = 4.0/p;
   
   // find the second predictor, first via usual stepwise
   picks = step.recommend_predictors(2,pValue);
   std::clog << "Standard stepwise rater picks "<< picks << std::endl;

   // then by coder
   picks = coder.recommend_predictors(2);
   std::clog << "Coding rater picks "<< picks << std::endl;
   
   // then by white
   picks =  white.recommend_predictors(2,pValue);
   std::clog << "White rater picks " << picks << std::endl;
   
   // then by binomial
   picks = bino.recommend_predictors(2,pValue);
   std::clog << "Binomial rater picks " << picks << std::endl;
   
   // then by bennett
   picks = bennett.recommend_predictors(2,pValue);
   std::clog << "Bennett/Binomial rater picks " << picks << std::endl;

   // add chosen predictor
   sm.add_predictor(picks[0], covMat.cross_products(picks[0]));
   std::cout << sm;
   std::cout << "Regression has slopes " << sm.slopes() << std::endl;


   return 0;
}
    
