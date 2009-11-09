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
  const std::string columnFileName ("/Users/bob/C/seq_regr/data/bank_small.dat");
  std::vector<Column> columns;
  insert_columns_from_file(columnFileName, back_inserter(columns));
  std::cout << "TEST: Data file " << columnFileName << " produced vector of " << columns.size() << " columns.\n";
  
  // extract column features
  std::vector<FeatureABC*> features;
  for (std::vector<Column>::const_iterator it = columns.begin(); it != columns.end(); ++it)
    features.push_back(new ColumnFeature(*it));
  std::cout << "TEST: Initialization converted " << columns.size() << " columns into features.\n";
  std::cout << "TEST: Converted columns to vector... \n " << features << std::endl;
  
  typedef  FiniteStream< std::vector<FeatureABC*> >      FS;
  typedef  InteractionStream< std::vector<FeatureABC*> > IS;
  
  FS finiteStream (make_finite_stream("Columns", features));
  IS interStream  (make_interaction_stream("Col interactions", features));
  
  Expert<UniversalBidder, InteractionStream< std::vector<FeatureABC*> > > expert(0.05, UniversalBidder(), interStream);
  
  std::vector<ExpertABC*> theExperts;
  theExperts.push_back(make_expert(0.05, FiniteBidder(0.05/finiteStream.number_remaining()), finiteStream));
  theExperts.push_back(make_expert(0.05, UniversalBidder(), interStream));
  theExperts.push_back(make_expert(0.05, UniversalBidder(), make_cross_product_stream("cp", features, features)));
  std::cout << "TEST: the experts are :\n" << theExperts << std::endl;
                       
  double bid;
  ExpertABC* e (theExperts[2]);
  
  bid = e->place_bid();
  std::cout << "TEST: Bid is " << bid << std::endl;
  e->bid_declined();
  bid = e->place_bid();
  std::cout << "TEST: Bid is " << bid << std::endl;
  e->bid_accepted();
  std::cout << e->features() << std::endl;
  e->payoff(0.05);
  
  bid = e->place_bid();
  std::cout << "TEST: Bid is " << bid << std::endl;
  e->bid_declined();
  bid = e->place_bid();
  std::cout << "TEST: Bid is " << bid << std::endl;
  e->bid_accepted();
  std::cout << e->features() << std::endl;
  e->payoff(-bid/(1-bid));

  bid = e->place_bid();
  std::cout << "TEST: Bid is " << bid << std::endl;
  e->bid_accepted();
  std::cout << e->features() << std::endl;
  e->payoff(-bid/(1-bid));
  
  bid = e->place_bid();
  std::cout << "TEST: Bid is " << bid << std::endl;
  e->bid_accepted();
  std::cout << e->features() << std::endl;
  e->payoff(-bid/(1-bid));
  
  return 0;
}

