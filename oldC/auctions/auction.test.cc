// $id: Auction.test.cc,v 1.41 2004/05/26 02:46:57 bob Exp $

/*
  Run using commands in the Makefile to get the data setup properly (eg, make auction_test)
  These execute code as
  
          auction.test.exec -f filename -p path -r rounds -c calibration_df

  where
        -r  number of rounds for the auction (default is 50)
        -f  path for input data              (default is est.dat)
	-m  path for output model results    (default is model)
	-c  calibration df                   (default is no calibration)

   2 Aug 04 ... Force logistic model to always have a spline smooth added on to calibrate (rather than recommender).
  23 Mar 04 ... Revised to use the anonymous ranges and other objects; logistic regression.
  13 Aug 03 ... Ready for trying with some real data; using alpha spending formulation.
   1 Aug 03 ... Created
*/

#include "auction.h"

#include "range.h"
#include "range_ops.h"
#include "column.h"
#include "log_regr.h"

#include <iostream>
#include <fstream>
#include <vector>


void
parse_arguments(int argc, char** argv, std::string& inputDataFile, std::string& outputPath, int &nRounds, int &df);

int
main(int argc, char** argv)
{
  // build vector of columns from file
  std::string columnFileName ("est.dat");
  std::string outputPath     ("model");
  int         numberRounds   (50);
  int         df             (0);
  parse_arguments(argc,argv, columnFileName, outputPath, numberRounds, df);
  std::cout << "AUCT: Arguments    -f " << columnFileName << " -p " << outputPath << " -r "
	    << numberRounds << " -c " << df << std::endl;
  std::vector<Column> yColumns;
  std::vector<Column> xColumns;
  insert_columns_from_file(columnFileName, 1, back_inserter(yColumns), back_inserter(xColumns));
  std::cout << "TEST: Data file " << columnFileName << " produced vector of " << xColumns.size() << " columns.\n";
  int nRows (end(yColumns[0].range())-begin(yColumns[0].range()));
  std::cout << "      Y column is " << yColumns[0] << " holding " << nRows << " rows.\n";

  // initialize factory and extract initial column features
  FeatureFactory factory(xColumns);
  std::vector<FeatureABC*> features = factory.features("ColumnFeature");
  std::cout << "TEST: Converted to feature vector with " << features.size() << " features.\n";
  
  // initialize the model
  LogisticRegression regr(yColumns[0].name(), make_anonymous_range(yColumns[0].range()), nRows);
  std::cout << " *************  Initialized Logistic Regression  *************** \n" << regr << std::endl;
  
  // build vector of static recommenders
  std::vector<RecommenderABC*> recommenders;
  recommenders.push_back(make_sequence_recommender   ("R0",           features));
  recommenders.push_back(make_permutation_recommender("R1",           features));
  recommenders.push_back(make_interaction_recommender("R2", &factory, features));

  // add bidders to represent recommenders (one recommender per bidder)
  std::vector<BidderABC*> bidders;
  bidders.push_back ( new GeometricBidder("B0", recommenders[0], 0.025, .25) );
  bidders.push_back ( new ConstantBidder ("B1", recommenders[1], 0.025    ) );
  bidders.push_back ( new ConstantBidder ("B2", recommenders[2], 0.025    ) );
  
  // create auction 
  Auction theAuction(bidders, regr, df, &factory);

  // add bidder/recommenders that depend on the auction and the model
  recommenders.push_back(make_interaction_recommender  ("R3", &factory, AuctionModelFeatures(theAuction)));
  bidders.push_back     (  new   GeometricBidder       ("B3", recommenders[3], 0.025, .2) );
  theAuction.add_bidder (bidders.back());
  recommenders.push_back(make_cross_product_recommender("R4", &factory, features, AuctionModelFeatures(theAuction)));
  bidders.push_back     (  new   GeometricBidder       ("B4", recommenders[4], 0.025, .2) );
  theAuction.add_bidder (bidders.back());
  
  /* These recommenders were a type of calibration adjustment
    recommenders.push_back(make_xb_recommender           ("R4", &factory, AuctionModelFeatures(theAuction), theAuction.model()));
    bidders.push_back (      new   GeometricBidder       ("B4", recommenders[4], 0.025, .2) );
    theAuction.add_bidder (bidders.back());
    recommenders.push_back(make_model_fit_recommender    ("R5", &factory, AuctionModelFeatures(theAuction), theAuction.model()));
    bidders.push_back (      new   GeometricBidder (      "B5", recommenders[5], 0.025, .2) );
    theAuction.add_bidder (bidders.back());
  */

  std::cout << "TEST: Initial recommenders: \n";
  for (unsigned int i=0; i<recommenders.size(); ++i)
    std::cout << "        " << recommenders[i] << std::endl;
  
  // run the auction with output to file
  std::string fileName (outputPath + "/alpha.dat");
  std::ofstream alphaStream (fileName.c_str());
  for (int round=0; round<numberRounds && theAuction.has_active_bidder(); ++round)
  { double result (theAuction.auction_next_feature());
    theAuction.write_alphas_to(alphaStream);
    if (result)
    { std::cout << "TEST: @@@ Auction adds predictor @@@" << std::endl;
      std::cout << theAuction << std::endl;
    }
    if (0 == round%25)
      theAuction.print_features_to(std::cout);
    std::cout << std::endl;
  }

  std::cout << "\n         ------- Auction Completed ------ \n\n" << theAuction << std::endl;
  
  // write model to a file
  fileName = outputPath + "/auction.model"; 
  std::cout << "TEST: Writing model to file " << fileName << std::endl;
  std::ofstream output (fileName.c_str());
  theAuction.write_model_to(output);
  output.close();

  // write model data to file
  fileName = outputPath + "/auction.model.txt"; 
  std::cout << "TEST: Writing data to file " << fileName << std::endl;
  std::ofstream dataOut (fileName.c_str());
  theAuction.write_model_data_to(dataOut);
  dataOut.close();
  
  std::cout << "TEST: Done; now disposing objects.\n";

  return 0;  
}


void
parse_arguments(int argc, char** argv,
		std::string& inputFile,
		std::string& outputPath,
		int & nRounds,
		int & nDF)
{
  for (int i=1; i<argc; i=i+2)
  {
    char key(argv[i][0]);
    if ('-' == key)
    { 
      switch (argv[i][1])
      {
      case 'c' :
	{ std::istringstream is(argv[i+1]);
	  is >> nDF;
	  break;
	}
      case 'f' :                                    
	{ std::string name(argv[i+1]);
	  inputFile = name;
	  break;
	}
      case 'p' :  
	{ std::string name(argv[i+1]);
	  outputPath = name;
	  break;
	}
      case 'r' :
	{ std::istringstream is(argv[i+1]);
	  is >> nRounds;
	  break;
	}
      default  :
	{
	  std::clog << "Option " << argv[i][0] << " not recognized." << std::endl;
	  break;
	}
      }
    }
  }
}  
  
