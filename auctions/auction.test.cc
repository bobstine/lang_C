// $Id: auction.test.cc,v 3.28 2008/02/22 19:39:47 bob Exp $
  
/*
  Run using commands in the Makefile to get the data setup properly (eg, make auction_test)
  Then execute code as
  
          auction.test -f filename -o path -r rounds -c calibration_df -v

  where
        -r  number of rounds for the auction (default is 50)
        -f  path for input data              (default is est.dat)
	-o  path for output model results    (default is model)
	-c  calibration df                   (default is no calibration)
        -v  treat second var (0/1) as CV indicator

   5 Sep 08 ... Add the validation option.
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

// from utils
#include "column.h"

// from gsl_tools
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

gslData*
build_model_data(std::vector<Column> const& y);

bool
data_has_selector(std::string const& dataFileName);


int
main(int argc, char** argv)
{
  // open file stream for all log information
  
  // build vector of columns from file; set default parameter values
  double      total_alpha_to_spend (0.5);
  std::string columnFileName       ("/Users/bob/C/gsl_tools/data/bank_post45.dat");   
  std::string outputPath           ("/Users/bob/C/auctions/test/log/"); 
  int         numberRounds         (300); 
  int         splineDF             (0);
  std::clog << "AUCT: $Id: auction.test.cc,v 3.28 2008/08/13 bob Exp $" << std::endl;
  std::clog << "AUCT: Parsing arguments ..." << std::endl;
  // parse arguments from command line  (pass in at main)
  parse_arguments(argc,argv, columnFileName, outputPath, numberRounds, splineDF);
  std::clog << "AUCT: Arguments    --input-file=" << columnFileName << " --output-path=" << outputPath
	    << " -r " << numberRounds << " --calibrator-df=" << splineDF
	    << std::endl;

  // need to fix issues surrouding the calibration adjustment
  if (splineDF != 0)
  { splineDF = 0;
    std::clog << "AUCT: Calibration DF set to 0 in current implementation to avoid problems\n";
  }
  
  std::clog << "AUCT: total_alpha_to_spend = " << total_alpha_to_spend << std::endl;
  std::string alphaFileName  (outputPath + "alpha.dat");
  std::string outputFileName (outputPath + "auction.model.pretty_print"); 
  std::string modelFileName  (outputPath + "auction.model"); 
  std::string dataFileName   (outputPath + "auction.model.txt");

  std::clog << "AUCT Output going to:" << std::endl;
  std::clog << "           alpha  --> " << alphaFileName << std::endl;
  std::clog << "           output --> " << outputFileName << std::endl;
  std::clog << "           model  --> " << modelFileName << std::endl;
  std::clog << "        model.txt --> " << dataFileName << std::endl;

  /* 
    Read columns from a file. The file is laid out with one column of values per row.
       Line 1: gives the number of cases
       Line 2: name of the first variable (the response)
       Line 3: data for the response
       Line 4: name of the second variable  (X_1)
       Line 5: data for the second variable
       Line 6: name of the third variable (X_2) 
       ...
    The reading is done by a FileColumnStream that allocates the space for the columns
    as the data are read.  A column feature provides a named range of doubles that learns a
    few properties of the data as it's read in (min, max, unique values). The space used
    by columns is allocated on reading in the function FileColumnStream.getNextColumn.
  */
  int numberYColumns = 1;
  if (data_has_selector(dataFileName))
    numberYColumns = 2;
  std::vector<Column> yColumns;
  std::vector<Column> xColumns;
  insert_columns_from_file(columnFileName, numberYColumns, back_inserter(yColumns), back_inserter(xColumns));
  std::clog << "AUCT: Data file " << columnFileName << " produced vector of " << yColumns.size() << " Y columns.\n";
  std::clog << "AUCT: Data file " << columnFileName << " produced vector of " << xColumns.size() << " X columns.\n";
  
  // form column features
  std::vector<FeatureABC*> columnFeatures;
  for (std::vector<Column>::const_iterator it = xColumns.begin(); it != xColumns.end(); ++it)
    columnFeatures.push_back(new ColumnFeature(*it));
  std::clog << "AUCT: Initialization converted " << xColumns.size() << " columns into features.\n";
  
  // build data object
  gslData *theData (build_model_data(yColumns));

  // build model and associated auction
  LinearModel <gslData, olsEngine> theRegr(theData);
  Auction<  LinearModel <gslData, olsEngine> > theAuction(theRegr, splineDF);

  // build logisitic model and auction// LogisticModel <gslData> theRegr(theData);
  // Auction<  LogisticModel <gslData> > theAuction(theRegr, splineDF);
  
  std::clog << "AUCT: Initial model is\n" << theRegr << std::endl;
  
    
  // build vector of experts that work directly from input variables
  std::clog << "AUCT: Creating experts"  << std::endl;
  double alphaShare (total_alpha_to_spend/5);

  typedef  FiniteStream      < std::vector<FeatureABC*> >      FStream;
  typedef  InteractionStream < std::vector<FeatureABC*> > IStream;
  typedef  CrossProductStream< std::vector<FeatureABC*>, std::vector<FeatureABC*> > CPStream;
  typedef  PolynomialStream  < std::vector<FeatureABC*> > PolyStream;
  
  // main column expert
  theAuction.add_expert(make_expert(alphaShare, 
                                    FiniteBidder<FStream>(), 
                                    make_finite_stream("Columns", columnFeatures)
				    ));

  // two bidders on the main collection of interactions
  theAuction.add_expert(make_expert(alphaShare, 
                                    FiniteBidder<IStream>(),
                                    make_interaction_stream("Column interactions", columnFeatures)
				    ));
  
  theAuction.add_expert(make_expert(alphaShare/10, 
                                    UniversalBidder<IStream>(), 
                                    make_interaction_stream("Column interactions", columnFeatures)
				    ));

  // parasitic experts betting on winners
  theAuction.add_expert(make_expert(alphaShare/5,
				    UniversalBidder<CPStream>(),
				    make_cross_product_stream("Used-feature interactions", columnFeatures, theAuction.model_features())
				    ));
                           
  theAuction.add_expert(make_expert(alphaShare/10, 
                                    UniversalBidder<CPStream>(),
                                    make_cross_product_stream("Skipped-feature interactions", columnFeatures, theAuction.skipped_features())
				    ));
  
  theAuction.add_expert(make_expert(alphaShare/5, 
                                    UniversalBidder<PolyStream>(),
                                    make_polynomial_stream("Skipped-feature polynomial", theAuction.skipped_features(), 3)     // poly degree
				    ));
  
  /*
    Principle component type features are temp turned off
    theAuction.add_expert(make_expert(alphaShare, 
    UniversalBidder(),
    make_subspace_stream("Principal components", 
    theAuction.skipped_features(), 
    20,                                    // bundle size
    gslPrincipalComponents(0, true)        // num components (0 means use rule), standardize
    )));
    theAuction.add_expert(make_expert(alphaShare, 
    UniversalBidder(),
    make_subspace_stream("RKHS components", 
    theAuction.skipped_features(), 
    20,                                    // bundle size
    gslRKHS<RadialKernel>(5, true)         // num components (0 means use rule), standardize
    )));                                   // WARNING: cannot return more than 25 x's in subspace
  */
  
  // run the auction with output to file
  std::ofstream alphaStream (alphaFileName.c_str());
  if (!alphaStream)
  { std::cerr << "AUCT: Cannot open output file for alpha stream " << alphaFileName << std::endl;
    return -1;
  }
  for (int round=0; round<numberRounds && theAuction.has_active_expert(); ++round)
  { double result (theAuction.auction_next_feature());
    theAuction.write_alphas_to(alphaStream);
    if (result)
    { std::clog << "AUCT: @@@ Auction adds predictor @@@" << std::endl;
      std::clog << theAuction << std::endl;
    }
    // theAuction.print_features_to(std::clog);
    std::clog << std::endl;
  }
  std::clog << "\n         ------- Auction Completed ------ \n\n" << theAuction << std::endl;
  
  // pretty print model to a file
  {
    std::ofstream output (outputFileName.c_str());
    output << theAuction << std::endl;
    output.close();
  }
  
  // write model to a file
  {
    std::clog << "AUCT: Writing model to file " << modelFileName << std::endl;
    std::ofstream output (modelFileName.c_str());
    if (! output)
    { std::cerr << "AUCT: Cannot open output file for model " << modelFileName << std::endl;
      return 1;
    }
    theAuction.write_model_to(output);
    output.close();
  }

  // write model data to file
  {
    std::clog << "AUCT: Writing model data to file " << dataFileName << std::endl;
    std::ofstream output (dataFileName.c_str());
    if (! output)
    { std::cerr << "AUCT: Cannot open output file for model data " << modelFileName << std::endl;
      return 2;
    }
    theAuction.write_model_data_to(output);
    output.close();
  }
  
  std::clog << "AUCT: Done; disposing objects.\n";
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
	  {"input-file",        1, 0, 'f'},  // has arg,
	  {"output-path",       1, 0, 'o'},  // has arg,
	  {"rounds",            1, 0, 'r'},  // has arg,
	  {"help",              0, 0, 'h'},  // no  arg, 
	  {0, 0, 0, 0}                       // terminator 
	};
	key = getopt_long (argc, argv, "c:f:o:r:v:h", long_options, &option_index);
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

// reads in response, initialized data object
gslData*
build_model_data(std::vector<Column> const& y)
{
  bool                      useSubset    (y.size() == 2);
  constant_iterator<double> equalWeights (1.0);  
  int                       nRows        (y[0].end()-y[0].begin());
  
  std::clog << "AUCT: Response has " << nRows << " rows.\n";
  if (useSubset)  // leading column is indicator of which cases to use in fitting
  {
    std::clog << "AUCT: Subset of cases defined by " << y[0] << "; response variable is " << y[1] << std::endl;
    return new gslData(y[1].begin(), y[0].begin(), equalWeights, nRows, gslRegression_Max_Q);
  } 
  else            // use all data for fitting
  {
    constant_iterator<bool>   noSelection(true);
    std::clog << "AUCT: Response variable is " << y[0] << std::endl;
    return new gslData(y[0].begin(),  noSelection , equalWeights, nRows, gslRegression_Max_Q);  
  } 
}


bool
data_has_selector(std::string const& dataFileName)
{
  std::ifstream input (dataFileName.c_str());
  int i,j;
  std::string firstVarName;
  input >> i >> j >> firstVarName;
  return (firstVarName == "[in/out][in]");
}
