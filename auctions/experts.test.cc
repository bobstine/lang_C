/*
 *  experts.test.cc
 *  auctions
 *
 *  Created by Robert Stine on 1/18/08.
 *  Copyright 2008. All rights reserved.
 *
 */

#include "experts.h"

#include "column.h"
#include "features.h"

#include <iostream>
#include <iomanip>

int
main()
{
  // build vector of columns from file
  const std::string columnFileName ("/Users/bob/C/gsl_tools/data/bank_post45.dat");
  std::vector<Column> columns;
  insert_columns_from_file(columnFileName, back_inserter(columns));
  std::cout << "TEST: Data file " << columnFileName << " produced vector of " << columns.size() << " columns.\n";
  
  // extract three column features
  std::vector<Feature> features;
  int counter (0);
  int limit (30);
  for (std::vector<Column>::const_iterator it = columns.begin(); it != columns.end(); ++it)
  { features.push_back(Feature(*it));
    std::cout << "[" << counter << "] " << (*it)->name() << std::endl;
    if (counter++ == limit) break;
  }
  std::cout << "TEST: Initialization converted " << columns.size() << " columns into features.\n";
  
  // construct streams
  typedef FeatureStream< InteractionIterator<FeatureVector, SkipIfRelatedPair>, Identity> InteractionStream;
  typedef FeatureStream< CyclicIterator<FeatureVector, SkipNone>, Identity>               FiniteStream;
  typedef FeatureStream< CrossProductIterator, Identity >                                 CrossProductStream;
  FiniteStream       finiteStream (make_finite_stream("Columns", features, SkipNone()));   
  InteractionStream  interStream  (make_interaction_stream("Col interactions", features, true));  // use squares 
  CrossProductStream cpStream     (make_cross_product_stream("cp", features, features));
  
  // build several experts
  double alpha (0.05);
  std::vector<Expert> theExperts;
  theExperts.push_back(Expert("finite",        source,   0, alpha, FiniteBidder<FiniteStream>(),          finiteStream));
  theExperts.push_back(Expert("interactions",  source,   0, alpha, UniversalBidder<InteractionStream>(),  interStream));
  theExperts.push_back(Expert("cross product", parasite, 0, alpha, UniversalBidder<CrossProductStream>(), cpStream));
  std::cout << "TEST: the experts are :\n" << theExperts << std::endl;

  // history defined in bidder.h
  std::vector<double> history;
  std::vector<Feature> accepted, rejected;
  BiddingHistory bidHist(history, accepted, rejected);
  
  // loop over rounds, and then loop over experts within each round
  std::vector<std::string> resultNames;
  for (int round=0; round <= 10; ++round)
    for (std::vector<Expert>::iterator it = theExperts.begin(); it != theExperts.end(); ++it)
    {
      std::cout << "\nTEST: Printing expert before bid in round " << round << ":   " << *it << std::endl;
      if (!(*it)->has_feature())
      {	std::cout << "TEST:  Expert is empty...\n";
	resultNames.push_back("----");
      }
      else
      { history.push_back((*it)->place_bid(bidHist));
	std::vector<Feature> features ((*it)->feature_vector());
	resultNames.push_back(features[0]->name());
	if (round % 3 == 0)     // accept every 3rd round
	{ (*it)->bid_accepted();
	  std::cout << "TEST: Feature " << features[0]->name() << " added.\n";
	  features[0]->set_model_results(true,0.05);   // used
	  (*it)->payoff(0.05);
	  std::for_each(theExperts.begin(), theExperts.end(), [](Expert e) {e->model_adds_current_variable();});
	  accepted.push_back(features[0]);
	}
	else
	{ (*it)->bid_declined();
	  std::cout << "TEST: Feature " << features[0]->name() << " rejected.\n";
	  features[0]->set_model_results(false,0.05);  // not used
	  rejected.push_back(features[0]);
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
  return 0;
}

