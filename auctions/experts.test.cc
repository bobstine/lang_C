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
  
  // extract column features
  std::vector<Feature> features;
  for (std::vector<Column>::const_iterator it = columns.begin(); it != columns.end(); ++it)
    features.push_back(Feature(*it));
  std::cout << "TEST: Initialization converted " << columns.size() << " columns into features.\n";
  
  // make regulated streams
  typedef  RegulatedStream< FiniteStream      < std::vector<Feature> > >      FS;
  typedef  RegulatedStream< InteractionStream < std::vector<Feature> > >      IS;
  typedef  RegulatedStream< CrossProductStream< std::vector<Feature>,std::vector<Feature> > > CP;
  FS finiteStream (make_finite_stream("Columns", features, 1));                   // one pass
  IS interStream  (make_interaction_stream("Col interactions", features, true));  // use squares 

  // build several experts
  int priority (1);
  double alpha (0.05);
  std::vector<Expert> theExperts;
  theExperts.push_back(Expert(priority, alpha, FiniteBidder<FS>(), finiteStream));
  theExperts.push_back(Expert(priority, alpha, UniversalBidder<IS>(), interStream));
  theExperts.push_back(Expert(priority, alpha, UniversalBidder<CP>(), make_cross_product_stream("cp", features, features)));
  std::cout << "TEST: the experts are :\n" << theExperts << std::endl;

  // state of auction defined in bidder.h
  std::vector<double> history;
  std::vector<Feature> accepted, rejected;
  AuctionState state(history, accepted, rejected);
  
  // use one for bidding
  double bid;
  Expert e (theExperts[2]);

  bid = e->place_bid(state);
  std::cout << "TEST: Bid 0 = " << bid << " (bid gets declined) " << std::endl;
  e->bid_declined();

  bid = e->place_bid(state);
  std::cout << "TEST: Bid 1 = " << bid << " (bid gets declined) " << std::endl;
  e->bid_declined();

  bid = e->place_bid(state);
  std::cout << "TEST: Bid 2 = " << bid << " (bid gets accepted, 0.05 payout) " << std::endl;
  e->bid_accepted();
  std::cout << e->feature_vector() << std::endl;
  e->payoff(0.05);
  
  bid = e->place_bid(state);
  std::cout << "TEST: Bid 3 = " << bid << " (bid gets declined) " << std::endl;
  e->bid_declined();
  
  bid = e->place_bid(state);
  std::cout << "TEST: Bid 4 = " << bid << std::endl;
  e->bid_accepted();
  std::cout << e->feature_vector() << std::endl;
  e->payoff(-bid/(1-bid));

  bid = e->place_bid(state);
  std::cout << "TEST: Bid is " << bid << std::endl;
  e->bid_accepted();
  std::cout << e->feature_vector() << std::endl;
  e->payoff(-bid/(1-bid));
  
  return 0;
}

