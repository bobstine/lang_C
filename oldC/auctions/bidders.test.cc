// $Id: bidders.test.cc,v 1.9 2004/04/21 13:01:57 bob Exp $

/*
   6 Aug 03 ... Created
*/

#include "bidders.h"

#include "column.h"
#include "my_features.h"
#include "feature_factory.h"
#include "recommenders.h"


#include <iostream>
#include <vector>
#include <map>


int
main()
{
  // build vector of columns from file
  const std::string columnFileName ("/Users/bob/C/seq_regr/bank_small.dat");
  std::vector<Column> columns;
  insert_columns_from_file(columnFileName, back_inserter(columns));
  std::cout << "TEST: Data file " << columnFileName << " produced vector of " << columns.size() << " columns.\n";

  // initialize factory and extract initial column features
  FeatureFactory factory(columns);
  std::vector<FeatureABC*> features = factory.features("ColumnFeature");
  std::cout << "TEST: Converted to vector... \n " << features << std::endl;
  
  // build vector of recommenders
  std::vector<RecommenderABC*> recommenders;
  recommenders.push_back(make_sequence_recommender("bank_small", features));
  recommenders.push_back(make_interaction_recommender("Test", &factory, features));
  std::cout << "TEST: Recommenders in auction are: \n";
  for (unsigned int i = 0; i<recommenders.size(); ++i)
    std::cout << "        " << recommenders[i] << std::endl;
  
  // bidders restricted to one per recommender
  std::vector<BidderABC*> bidders;
  bidders.push_back ( new ConstantBidder ("B1", recommenders[0], 0.5    ) );
  bidders.push_back ( new GeometricBidder("B2", recommenders[1], 0.5, .5) );

  for (int round=1; round <=2; ++round)
  {
    BidCollector collector;
    std::cout << "        __________  Round  " << round << " __________ " << std::endl;
    for(std::vector<BidderABC*>::iterator it = bidders.begin(); it != bidders.end(); ++it)
      collector(*it);
    
    BidderABC::Bid       highBid   (collector.high_bid());
    std::cout << "TEST: high bid is " << highBid << std::endl;
    
    BidCollector::BidderVector bidderVector (collector.bidders_on_feature(highBid.first));
    std::cout << "TEST: bidders on " << highBid.first->name() << " are\n" << bidderVector << std::endl;
    for (std::vector<BidderABC*>::iterator it = bidderVector.begin(); it != bidderVector.end(); ++it)
      (*it)->bid_accepted (0.1, 0.01);
    BidCollector::BidderVector passerVector (collector.not_bidders_on_feature(highBid.first));
    std::cout << "TEST: passing on " << highBid.first->name() << " are\n" << passerVector << std::endl;
  }

  return 0;
}  
  
