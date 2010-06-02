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
  int limit (3);
  for (std::vector<Column>::const_iterator it = columns.begin(); it != columns.end(); ++it)
  { features.push_back(Feature(*it));
    std::cout << "[" << counter << "] " << (*it)->name() << std::endl;
    if (counter++ == limit) break;
  }
  std::cout << "TEST: Initialization converted " << columns.size() << " columns into features.\n";
  
  // make regulated streams
  typedef  RegulatedStream< FiniteStream  >                                   FS;
  typedef  RegulatedStream< InteractionStream < std::vector<Feature> > >      IS;
  typedef  RegulatedStream< CrossProductStream< std::vector<Feature>,std::vector<Feature> > > CP;
  FS finiteStream (make_finite_stream("Columns", features));   
  IS interStream  (make_interaction_stream("Col interactions", features, true));  // use squares 

  // build several experts
  double alpha (0.05);
  std::vector<Expert> theExperts;
  theExperts.push_back(Expert(source, 0, alpha, FiniteBidder<FS>(), finiteStream));
  theExperts.push_back(Expert(source, 0, alpha, UniversalBidder<IS>(), interStream));
  theExperts.push_back(Expert(parasite, 0, alpha, UniversalBidder<CP>(), make_cross_product_stream("cp", features, features)));
  std::cout << "TEST: the experts are :\n" << theExperts << std::endl;

  // history defined in bidder.h
  std::vector<double> history;
  std::vector<Feature> accepted, rejected;
  BiddingHistory bidHist(history, accepted, rejected);
  
  // use one for bidding
  double bid;
  Expert e (theExperts[0]);

  std::cout << "\nTEST, printing expert before bid:     " << e << std::endl;
  bid = e->place_bid(bidHist);
  std::cout << "TEST: Bid[0] = " << bid << "   [declined]" << std::endl;
  e->bid_declined();
  
  for (int j=0; j <= 10; ++j)
  { BiddingHistory hist(history, accepted, rejected);
    std::cout << "\nTEST: Printing expert before bid " << j << ":   " << e << std::endl;
    if (!e->has_feature(hist))
    { std::cout << "Empty...\n";
      break;
    }
    else
    { bid = e->place_bid(hist);
      history.push_back(bid);
      std::cout << "TEST: Bid " << bid << "   [accepted, +0.05]" << std::endl;
      e->bid_accepted();
      std::vector<Feature> features (e->feature_vector());
      std::cout << features << std::endl;
      if (j == 0)
      { std::cout << "TEST: Feature " << features[0]->name() << " added.\n";
	features[0]->set_model_results(true,0.05);   // used
	e->payoff(0.05);
	for(std::vector<Expert>::const_iterator it=theExperts.begin(); it != theExperts.end(); ++it)
	  (*it)->model_adds_current_variable();
	accepted.push_back(features[0]);
      }
      else
      { std::cout << "TEST: Feature " << features[0]->name() << " not added.\n";
	features[0]->set_model_results(false,0.05);  // not used
	rejected.push_back(features[0]);
      }
    }
  }
  
  return 0;
}

