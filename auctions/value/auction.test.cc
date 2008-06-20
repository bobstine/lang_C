// $Id: auction.test.cc,v 3.0 2004/11/19 18:58:36 foster Exp $
 
/*
  23 Mar 04 ... Revised to use the anonymous ranges and other objects; logistic regression.
  13 Aug 03 ... Ready for trying with some real data; using alpha spending formulation.
   1 Aug 03 ... Created
*/

#include "auction.h"

#include "column.h"
#include "log_regr.h"

#include <iostream>
#include <fstream>
#include <vector>

int
main(void)
{
  // build vector of columns from file
  const std::string columnFileName ("/Users/bob/C/seq_regr/bank_small.dat");
  //  const std::string columnFileName ("/Users/bob/C/seq_regr/bank_post45.dat");
  std::vector<Column> yColumns;
  std::vector<Column> xColumns;
  insert_columns_from_file(columnFileName, 1, back_inserter(yColumns), back_inserter(xColumns));
  std::cout << "TEST: Data file " << columnFileName << " produced vector of " << xColumns.size() << " columns.\n";

  // convert to a feature source
  FeatureVector features(xColumns);
  
  // initialize the model
  LogisticRegression regr("Small Bank Model", make_anonymous_range(yColumns[0].range()));
  std::cout << " *************  Initialized Model  *************** \n" << regr << std::endl;
  
  // build vector of column-based recommenders
  std::vector<RecommenderABC*> recommenders;
  recommenders.push_back(make_sequence_recommender   ("R1", features));
  recommenders.push_back(make_permutation_recommender("R2", features));
  recommenders.push_back(make_interaction_recommender("R3", features));
				 
  // build vector of bidders to represent recommenders (one recommender per bidder)
  std::vector<BidderABC*> bidders;
  bidders.push_back ( new GeometricBidder("B1", recommenders[0], 0.20, .5) );
  bidders.push_back ( new ConstantBidder ("B2", recommenders[1], 0.20    ) );
  bidders.push_back ( new ConstantBidder ("B3", recommenders[2], 0.20    ) );
  
  // create auction 
  Auction theAuction(bidders, regr);

  // add model-based bidder/recommenders
  recommenders.push_back(make_interaction_recommender("R4", AuctionModelFeatures(theAuction)));
  bidders.push_back     (  new   GeometricBidder     ("B4", recommenders[3], 0.20, .2) );
  theAuction.add_bidder (bidders.back());

  std::cout << "TEST: Initial recommenders: \n";
  for (unsigned int i=0; i<recommenders.size(); ++i)
    std::cout << "        " << recommenders[i] << std::endl;
  
  // run the auction with output to file
  std::ofstream alphaStream ("alpha.dat");
  for (int round=0; round<15 && theAuction.has_active_bidder(); ++round)
  { double result (theAuction.auction_next_feature());
    theAuction.write_alphas_to(alphaStream);
    if (result)
      std::cout << "TEST: @@@ Auction adds predictor @@@" << std::endl;
    std::cout << theAuction << std::endl;
    if (0 == round%5)
      theAuction.print_features_to(std::cout);
    std::cout << std::endl;
  }
  // write model to a file
  std::ofstream output ("test/auction.model");
  theAuction.write_model_to(output);
  output.close();
  // write model data to file
  std::ofstream dataOut ("test/auction.model.txt");
  theAuction.write_model_data_to(dataOut);
  dataOut.close();
  
  std::cout << "TEST: Done; now disposing objects.\n";
  std::cout << "Done.\n";
  return 0;  
}
