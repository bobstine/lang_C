/*
 *  experts.test.cc
 *  auctions
 *
 *  Created by Robert Stine on 1/18/08.
 *  Copyright 2008. All rights reserved.
 *
 */

#include "experts.h"
#include "regression.h"

#include "column.h"
#include "features.h"

#include <iostream>
#include <iomanip>


// for linking to model
typedef     std::pair<std::string, FeatureABC::Iterator>   NamedIterator;
typedef     std::vector<NamedIterator>                     NamedIteratorVector;

int
main()
{
  // build vector of columns from file
  const std::string columnFileName ("/Users/bob/C/gsl_tools/data/bank_post45.dat");
  std::vector<Column> columns;
  insert_columns_from_file(columnFileName, back_inserter(columns));
  std::cout << "TEST: Data file " << columnFileName << " produced vector of " << columns.size() << " columns.\n";
  
  // extract three column features
  std::vector<Feature> yFeatures;
  std::vector<Feature> xFeatures;
  for (unsigned j=0; j<4; ++j)
  { yFeatures.push_back(Feature(columns[j]));
    std::cout << "Y[" << j << "] " << columns[j]->name() << std::endl;
  }
  for (unsigned j=4; j<40;++j)
  { xFeatures.push_back(Feature(columns[j]));
    std::cout << "X[" << j << "] " << columns[j]->name() << std::endl;
  }
  std::cout << "TEST: Initialization converted " << columns.size() << " columns into features.\n";
  
  // build regression model ([0] is br, [2] is other month indicator)
  ValidatedRegression theRegr(columns[0]->name(), columns[0]->begin(), columns[2]->begin(), columns[0]->size());
  
  // construct streams
  typedef FeatureStream< CyclicIterator<FeatureVector, SkipNone>, Identity>                                 FiniteStream;
  typedef FeatureStream< InteractionIterator<FeatureVector, SkipIfRelatedPair>, Identity>                   InteractionStream;
  typedef FeatureStream< CrossProductIterator, Identity >                                                   CrossProductStream;
  typedef FeatureStream< ModelIterator<ValidatedRegression>, BuildCalibrationFeature<ValidatedRegression> > CalibrationStream;

  FiniteStream       finiteStream (make_finite_stream("Columns", xFeatures, SkipNone()));   
  InteractionStream  interStream  (make_interaction_stream("Col interactions", xFeatures, true));  // use squares 
  CrossProductStream cpStream     (make_cross_product_stream("cp", xFeatures, xFeatures));
  CalibrationStream  calStream    (make_calibration_stream("fitted_values", theRegr, 3, 0));       // poly degree, number prefix cases 
				 
  // build several experts
  double alpha (0.05);
  std::vector<Expert> theExperts;
  theExperts.push_back(Expert("finite",        source,   0, alpha, FiniteBidder<FiniteStream>(),          finiteStream));
  theExperts.push_back(Expert("interactions",  source,   0, alpha, UniversalBidder<InteractionStream>(),  interStream));
  theExperts.push_back(Expert("cross product", parasite, 0, alpha, UniversalBidder<CrossProductStream>(), cpStream));
  theExperts.push_back(Expert("calibrator",   calibrate, 0, alpha, FitBidder(0.000005, "Y_hat"),          calStream));
  std::cout << "TEST: the experts are :\n" << theExperts << std::endl;

  // history defined in bidder.h
  std::vector<double> history;
  std::vector<Feature> accepted, rejected;
  BiddingHistory bidHist(history, accepted, rejected);
  
  // loop over rounds, and then loop over experts within each round
  std::vector<std::string> resultNames;
  for (int round=0; round <= 10; ++round)
  { std::cout << "\n\n ========================================\n";
    for (std::vector<Expert>::iterator it = theExperts.begin(); it != theExperts.end(); ++it)
    {
      std::cout << "TEST: Expert before bid in round " << round << ":   " << *it << std::endl;
      if (!(*it)->has_feature())
      {	std::cout << "TEST:  Expert is empty...\n";
	resultNames.push_back("----");
      }
      else
      { history.push_back((*it)->place_bid(bidHist));
	std::vector<Feature> fv ((*it)->feature_vector());
	resultNames.push_back(fv[0]->name());
	if (round % 3 == 0)     // accept every 3rd round
	{ (*it)->bid_accepted();
	  std::cout << "TEST: Feature " << fv[0]->name() << " added.\n";
	  theRegr.add_predictors_if_useful(theExperts[0]->convert_to_model_iterators(fv), 0.10);
	  fv[0]->set_model_results(true,0.05);   // used
	  (*it)->payoff(0.05);
	  std::for_each(theExperts.begin(), theExperts.end(), [](Expert e) {e->model_adds_current_variable();});
	  accepted.push_back(fv[0]);
	}
	else
	{ (*it)->bid_declined();
	  std::cout << "TEST: Feature " << fv[0]->name() << " rejected.\n";
	  fv[0]->set_model_results(false,0.05);  // not used
	  rejected.push_back(fv[0]);
	}
      }
    }
  }
  std::cout << "TEST:  Done, with features...\n";
  unsigned k (0);
  for (unsigned i=0; i<=10; ++i)
  { std::cout << "Round " << i << ":    ";
    for(unsigned j=0; j<theExperts.size(); ++j)
      std::cout << std::setw(20) << resultNames[k++] << "    ";
    std::cout << std::endl;
  }
  std::cout << theRegr << std::endl;
  return 0;
}

