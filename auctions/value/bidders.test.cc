// $Id: bidders.test.cc,v 3.0 2004/11/19 18:58:36 foster Exp $

/*
   6 Aug 03 ... Created
*/

#include "bidders.h"

#include "column.h"
#include "feature.h"
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

  // convert this vector of columns into a feature source
  FeatureVector featureVector (columns);
  
  // build vector of recommenders
  std::vector<RecommenderABC*> recommenders;
  recommenders.push_back(make_sequence_recommender("bank_small", featureVector));
  recommenders.push_back(make_interaction_recommender("bank_small", featureVector));
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
    std::cout << "TEST: bidders on " << highBid.first.name() << " are\n" << bidderVector << std::endl;
    for (std::vector<BidderABC*>::iterator it = bidderVector.begin(); it != bidderVector.end(); ++it)
      (*it)->bid_accepted (0.1, 0.01);
    BidCollector::BidderVector passerVector (collector.not_bidders_on_feature(highBid.first));
    std::cout << "TEST: passing on " << highBid.first.name() << " are\n" << passerVector << std::endl;
  }

  return 0;
}  
  
