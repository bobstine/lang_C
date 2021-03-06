// $Id: auction.test.cc,v 1.1 2008/01/20 19:55:40 bob Exp $

/*
  Run using commands in the Makefile to get the data setup properly (eg, make auction_test)
  Then execute code as
  
          auction.test.exec -f filename -p path -r rounds -c calibration_df

  where
        -r  number of rounds for the auction (default is 50)
        -f  path for input data              (default is est.dat)
	      -m  path for output model results    (default is model)
	      -c  calibration df                   (default is no calibration)

  14 Oct 04 ... (dpf) added make_20_geometric bidders
   2 Aug 04 ... Force logistic model to have a spline smooth to calibrate (rather than recommender).
  23 Mar 04 ... Revised to use the anonymous ranges and other objects; logistic regression.
  13 Aug 03 ... Ready for trying with some real data; using alpha spending formulation.
   1 Aug 03 ... Created
*/

#include "auction.h"

// from ranges
#include "range.h"
#include "range_ops.h"
#include "anonymous_iterator.h"
// for constant iterator 
#include "cyclic_iterator.h"

// from sequential regression 
#include "column.h"
#include "gsl_model.h"


#include <iostream>
#include <fstream>
#include <vector>
#include <getopt.h>


void
parse_arguments(int argc, char** argv, 
                std::string& inputDataFile, 
                std::string& outputPath, 
                int &nRounds, int &df);

// build the data structure used by the model
gslData*
build_model_data(std::vector<Column> const& y)
{
  bool useSubset (y.size() == 2);
  constant_iterator<double> equalWeights (1.0);  
  int nRows (end(y[0].range())-begin(y[0].range()));
  std::cout << "TEST: Y column " << y[0] << " holds " << nRows << " rows.\n";
  if (useSubset)  // use all data for fitting
  { std::cout << "TEST: Using subset of cases defined by " << y[1] << std::endl;
    return new gslData(y[0].memory(), y[1].memory(), equalWeights, nRows, gslRegression_Max_Q);
  } 
  else
  { constant_iterator<bool>   noSelection(true);
    return new gslData(y[0].memory(),  noSelection , equalWeights, nRows, gslRegression_Max_Q);  
  } 
}


//  Can also run from this script as a build phase
/*  
Shell: 
/bin/tcsh
Script:
$BUILT_PRODUCTS_DIR/auction  --rounds 100 -c 5 -f $SRCROOT/test/bank_post45.dat
cat $SRCROOT/test/bank_post45.n_rows $SRCROOT/test/bank_post45.rows > $SRCROOT/model/test.dat 
*/
 
int
main(int argc, char** argv)
{ 
  // build vector of columns from file
  double      total_alpha_to_spend (0.5);
  //  std::string columnFileName       ("/Users/bob/C/seq_regr/data/bank_small.dat");   
  std::string columnFileName       ("/Users/bob/raslisp/stepwise/C/firstfunds/ds2.rows.dat"); 
  std::string outputPath           ("/Users/bob/C/auctions/test/model/");   // default parameter values
  int         numberRounds         (250); 
  int         useSubset            (1);  // 1 indicates that second variable in data in boolean selector
  int         splineDF             (0);
  std::cout << "AUCT: $Id: auction.test.cc,v 1.1 2008/01/20 19:55:40 bob Exp $" << std::endl;

	std::cout << "AUCT: Parsing arguments ..." << std::endl;
  // parse arguments from command line
  // parse_arguments(argc,argv, columnFileName, outputPath, numberRounds, df);
  std::cout << "AUCT: Arguments    --input-file=" << columnFileName
    << " --output-path=" << outputPath << " -r "
    << numberRounds << " --calibrator-df=" << splineDF<< std::endl;
  std::cout << "AUCT: total_alpha_to_spend = " << total_alpha_to_spend << std::endl;
  std::string alphaFileName  (outputPath + "alpha.dat");
  std::string outputFileName (outputPath + "auction.model.pretty_print"); 
  std::string modelFileName  (outputPath + "auction.model"); 
  std::string dataFileName   (outputPath + "auction.model.txt");

  std::cout << "AUCT Output going to:" << std::endl;
  std::cout << "           alpha  --> " << alphaFileName << std::endl;
  std::cout << "           output --> " << outputFileName << std::endl;
  std::cout << "           model  --> " << modelFileName << std::endl;
  std::cout << "        model.txt --> " << dataFileName << std::endl;

  /* 
    Read columns from a file. The file is laid out with one column of values per row.
    The reading is done by a FileColumnStream that allocates the space for the columns
    as the data are read.  A column is basically a named range of doubles that learns a
    few properties of the data when it's read in (min, max, unique values). The space used
    by columns is allocated on reading in the function FileColumnStream.getNextColumn.
  */
  std::vector<Column> yColumns;
  std::vector<Column> xColumns;
  insert_columns_from_file(columnFileName, 1+useSubset, back_inserter(yColumns), back_inserter(xColumns));
  std::cout << "TEST: Data file " << columnFileName << " produced vector of " << xColumns.size() << " x columns.\n";
  
  // initialize factory and extract initial column features
  FeatureFactory factory(xColumns);
  std::vector<FeatureABC*> featureVector (factory.features("ColumnFeature"));
  std::cout << "TEST: Converted data to feature vector with " << featureVector.size() << " features.\n";
  
  // initialize the model
  gslData *theData (build_model_data(yColumns));
  LinearModel <gslData, olsEngine> theRegr(theData);               // LogisticModel <gslData> theRegr(&theData);  
  std::cout << "TEST: Initial model is\n" << theRegr << std::endl;
  
  // build vector of recommenders that work directly from input variables
  std::cout << "TEST: Creating static recommenders "  << std::endl;
  std::vector<RecommenderABC*> recommenders;
  recommenders.push_back(make_sequence_recommender   ("R0",           featureVector));
  recommenders.push_back(make_permutation_recommender("R1",           featureVector));
  recommenders.push_back(make_interaction_recommender("R2", &factory, featureVector));

  // add bidders to represent recommenders (each bidder can represent only one recommender)
  // but several bidders can represent the same recommender.
  std::cout << "TEST: Creating bidders "  << std::endl;
  std::vector<BidderABC*> bidders;
  bidders.push_back ( new ConstantBidder ("B0", recommenders[0], total_alpha_to_spend * 0.2) );
  bidders.push_back ( new ConstantBidder ("B1", recommenders[1], total_alpha_to_spend * 0.2) );
  bidders.push_back ( new ConstantBidder ("B2", recommenders[2], total_alpha_to_spend * 0.2) );
  
  // create auction 
  std::cout << "TEST: Firing up the auction "  << std::endl;
  Auction<  LinearModel <gslData, olsEngine> > theAuction(bidders, theRegr, splineDF, &factory);
  
  // add bidder/recommenders that depend on the auction and the model
  // recommenders.push_back(make_fit_trash_recommender("RT", &factory, theAuction));
  
  std::cout << "TEST: Making dynamic bidders that watch the auction "  << std::endl;
  
  recommenders.push_back(make_interaction_recommender  ("R3", &factory, make_auction_model_features(theAuction)));
  theAuction.add_bidder (new UniversalBidder ("B3", recommenders[3], total_alpha_to_spend * 0.2));
  
  recommenders.push_back(make_cross_product_recommender("R4", &factory, featureVector, make_auction_model_features(theAuction)));
  theAuction.add_bidder (new UniversalBidder ("B4", recommenders[4], total_alpha_to_spend * 0.2) );

  // --- These recommenders were a type of calibration adjustment
  //  recommenders.push_back(make_xb_recommender ("R4", &factory, AuctionModelFeatures(theAuction), theAuction.model()));
  //  bidders.push_back (      new   GeometricBidder ("B4", recommenders[4], 0.025, .2) );
  //  theAuction.add_bidder (bidders.back());
  //  recommenders.push_back(make_model_fit_recommender ("R5", &factory, AuctionModelFeatures(theAuction), theAuction.model()));
  //  bidders.push_back (      new   GeometricBidder ("B5", recommenders[5], 0.025, .2) );
  //  theAuction.add_bidder (bidders.back());

  std::cout << "TEST: Initial recommenders: \n";
  for (unsigned int i=0; i<recommenders.size(); ++i)
    std::cout << "        " << recommenders[i] << std::endl;
  
  // run the auction with output to file
  std::ofstream alphaStream (alphaFileName.c_str());
  for (int round=0; round<numberRounds && theAuction.has_active_bidder(); ++round)
  { double result (theAuction.auction_next_feature());
    theAuction.write_alphas_to(alphaStream);
    if (result)
    { std::cout << "TEST: @@@ Auction adds predictor @@@" << std::endl;
      std::cout << theAuction << std::endl;
    }
    // theAuction.print_features_to(std::cout);
    std::cout << std::endl;
  }
  std::cout << "\n         ------- Auction Completed ------ \n\n" << theAuction << std::endl;
  
  { // pretty print model to a file
    std::ofstream output (outputFileName.c_str());
    output << theAuction << std::endl;
    output.close();
  }

  { // write model to a file
    std::cout << "TEST: Writing model to file " << modelFileName << std::endl;
    std::ofstream output (modelFileName.c_str());
    theAuction.write_model_to(output);
    output.close();
  }
  
  { // write model data to file
    std::cout << "TEST: Writing data to file " << dataFileName << std::endl;
    std::ofstream dataOut (dataFileName.c_str());
    theAuction.write_model_data_to(dataOut);
    dataOut.close();
  }

  std::cout << "TEST: Done; disposing objects.\n";
  return 0;  
}


void
parse_arguments(int argc, char** argv,
		std::string& inputFile,
		std::string& outputPath,
		int & nRounds,
		int & nDF)
{
  int key;
  while (1)                                  // read until empty key causes break
    {
      int option_index = 0;
      static struct option long_options[] = {
	  {"calibrator-df",     1, 0, 'c'},  // has arg,
	  {"input-file",        1, 0, 'f'},  // has	arg,
	  {"output-path",       1, 0, 'o'},  // has arg,
	  {"rounds",            1, 0, 'r'},  // has arg,
	  {"help",              0, 0, 'h'},  // no  arg, 
	  {0, 0, 0, 0}                       // terminator 
	};
	key = getopt_long (argc, argv, "c:f:o:r:h", long_options, &option_index);
	if (key == -1)
	  break;
	std::cout << "Option key " << char(key) << " with option_index " << option_index << std::endl;
	switch (key)
	  {
	  case 'c' : 
	    {
	      std::istringstream is(optarg);
	      is >> nDF;
	      break;
	    }
	  case 'f' :                                    
	    {
	      std::string name(optarg);
				std::cout << "Read name from optional args..." << name << std::endl;
	      inputFile = name;
	      break;
	    }
	  case 'o' :  
	    {
	      outputPath = optarg;
	      break;
	    }
	  case 'r' :
	    {
	      std::istringstream is(optarg);
	      is >> nRounds;
	      break;
	    }
	  case 'h' :
	    {
	      std::cout << "switches:" << std::endl << std::endl;
	      std::cout << "      --calibrator-df=#    degrees of freedom in spline calibrator" << std::endl;
	      std::cout << "      -c4" << std::endl << std::endl;
	      std::cout << "      --input-file=foo       input file" << std::endl;
	      std::cout << "      -ifoo" << std::endl << std::endl;
	      std::cout << "      --output-path=/home/.../   path for output files" << std::endl;
	      std::cout << "      -o/home/.../" << std::endl << std::endl;
	      std::cout << "      --rounds=#      maximum number of rounds of auction" << std::endl;
	      std::cout << "      -r#" << std::endl << std::endl;
	      std::cout << "      --help      generates this message" << std::endl;
	      std::cout << "      -h" << std::endl << std::endl;
	      exit(0);
	      break;
	    }
	  }
    }
}

/*
 std::vector<BidderABC*>
 make_20_geometric_bidders(const std::string& base_name,
                           RecommenderABC* r,
                           double alpha_to_spend)
 {
   std::vector<BidderABC*> result;
   bool do_only_one = false;
   if(do_only_one)
     result.push_back ( new GeometricBidder(base_name, r, alpha_to_spend, .01) );
   else {
     double alpha_spending_rate = .5;
     for(char label = 'a'; label < 'a' + 20;++label) 
     { result.push_back ( new GeometricBidder(base_name + label,
                                              r,
                                              alpha_to_spend/25,
                                              alpha_spending_rate) );
       if((label - 'a') % 3 == 0)
         alpha_spending_rate = alpha_spending_rate / 2.5;
       else
         alpha_spending_rate = alpha_spending_rate / 2.0;
     } 
   }
   return result;
 }
 
 
*/ 