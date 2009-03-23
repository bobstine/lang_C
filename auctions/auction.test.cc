/*
  Run using commands in the Makefile to get the data setup properly (eg, make auction_test)
  Then execute code as
  
          auction.test -f filename -o path -r rounds -c calibration_df -v

  where
        -r  number of rounds for the auction (default is 50)
        -f  path for input data              (default is est.dat)
	-o  path for output model results    (default is model)
	-c  calibration df                   (default is no calibration)

  22 Mar 09 ... Remove validation option by recognizing file pattern; add debugging from Dean
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

// from utils; debug has the printing facility
#include "column.h"
#include "debug.h"

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
build_model_data(std::vector<Column> const& y, std::ostream&);

bool
data_has_selector(std::string const& dataFileName, std::ostream&);


int
main(int argc, char** argv)
{
  // build vector of columns from file; set default parameter values
  double      total_alpha_to_spend (0.5);
  std::string columnFileName       ("/Users/bob/C/gsl_tools/data/bank_post45.dat");   
  std::string outputPath           ("/Users/bob/C/auctions/test/log/"); 
  int         numberRounds         (300); 
  int         splineDF             (0);
  
  // parse arguments from command line  (pass in at main, open file for printing messages)
  parse_arguments(argc,argv, columnFileName, outputPath, numberRounds, splineDF);

  // initialize bugging stream (write to clog if debugging is on, otherwise to auction.log file)
  using namespace debugging;
  std::string   debugFileName (outputPath + "progress.log");
  std::ofstream logStream     (debugFileName.c_str());
#ifdef NDEBUG
  debugging::debug_init(logStream, -1);
#else
  debugging::debug_init(std::clog, -1);
#endif

  // echo startup options to log file
  debug("AUCT",3) << "Version build 0.50 (23 Mar 09)\n";
  debug("AUCT",3) << "Arguments    --input-file=" << columnFileName << " --output-path=" << outputPath
		  << " -r " << numberRounds << " --calibrator-df=" << splineDF
		  << std::endl;
  debug("AUCT",3) << " Total alpha available to spend in auction is " << total_alpha_to_spend << std::endl;

  // need to fix issues surrouding the calibration adjustment
  if (splineDF != 0)
  { splineDF = 0;
    debug("AUCT",-1) << "Calibration DF set to 0 in current implementation to avoid problems\n";
  }  

  // open additional files for output
  std::string alphaFileName      (outputPath + "alpha.dat");
  std::string modelHTMLFileName  (outputPath + "model.html"); 
  std::string modelTextFileName  (outputPath + "model.txt"); 
  std::string modelDataFileName  (outputPath + "model_data.csv");
  debug("AUCT",-1) << "AUCT Output going to:\n"
		   << "           alpha  --> " << alphaFileName  << std::endl
#ifdef NDEBUG
                   << "             log  --> " << debugFileName  << std::endl
#endif
		   << "      model data  --> " << modelDataFileName << std::endl
		   << "       model.txt  --> " << modelTextFileName << std::endl;

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
  if (data_has_selector(columnFileName, debug("AUCT",0)))
    numberYColumns = 2;
  std::vector<Column> yColumns;
  std::vector<Column> xColumns;
  insert_columns_from_file(columnFileName, numberYColumns, back_inserter(yColumns), back_inserter(xColumns));
  debug("AUCT",0) << "Data file " << columnFileName << " produced " << yColumns.size() << " Y columns and "
		  << xColumns.size() << " X columns.\n";
  
  // form column features
  std::vector<FeatureABC*> columnFeatures;
  for (std::vector<Column>::const_iterator it = xColumns.begin(); it != xColumns.end(); ++it)
    columnFeatures.push_back(new ColumnFeature(*it));
  debug("AUCT",0) << "Initialization converted " << xColumns.size() << " columns into features.\n";
  
  // build data object
  gslData *theData (build_model_data(yColumns, debug("AUCT",1)));

  // build model and initialize auction with stream for log
  LinearModel <gslData, olsEngine> theRegr(theData);
  Auction<  LinearModel <gslData, olsEngine> > theAuction(theRegr, splineDF, debug(0));

  // build logisitic model and auction// LogisticModel <gslData> theRegr(theData);
  // Auction<  LogisticModel <gslData> > theAuction(theRegr, splineDF);
  debug("AUCT",0) << "Initial model in the auction is\n" << theRegr << std::endl;
  
    
  // build vector of experts that work directly from input variables
  debug("AUCT",-1) << "Creating experts"  << std::endl;
  double alphaShare (total_alpha_to_spend/5);
  typedef  FiniteStream      < std::vector<FeatureABC*> > FStream;
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
    { debug("AUCT",3) << "@@@ Auction adds predictor @@@" << std::endl
		      << theAuction << std::endl << std::endl;
    }
    // theAuction.print_features_to(std::clog);
  }
  debug("AUCT",3) << "\n         ------- Auction Completed ------ \n\n" << theAuction << std::endl;
  
  // write model in HTML to a file
  {
    std::ofstream output (modelHTMLFileName.c_str());
    if (! output)
    { std::cerr << "AUCT: Cannot open output HTML file for writing model " << modelHTMLFileName << std::endl;
      return 1;
    }
    theAuction.write_html_model_to(output);
    output.close();
  }
  
  // write model to a file
  {
    debug("AUCT",0) << "Writing model to file " << modelTextFileName << std::endl;
    std::ofstream output (modelTextFileName.c_str());
    if (! output)
    { std::cerr << "AUCT: Cannot open output text file for writing model " << modelTextFileName << std::endl;
      return 1;
    }
    theAuction.write_model_to(output);
    output.close();
  }
  
  // write model data to file
  {
    debug("AUCT",0) << "Writing model data to file " << modelDataFileName << std::endl;
    std::ofstream output (modelDataFileName.c_str());
    if (! output)
    { std::cerr << "AUCT: Cannot open output file for model data " << modelDataFileName << std::endl;
      return 2;
    }
    theAuction.write_model_data_to(output);
    output.close();
  }
  
  debug("AUCT",-1) << "Done; disposing objects.\n";
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
	// std::cout << "Option key " << char(key) << " with option_index " << option_index << std::endl;
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
	      // std::cout << "Read name from optional args..." << name << std::endl;
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
build_model_data(std::vector<Column> const& y, std::ostream& os)
{
  bool                      useSubset    (y.size() == 2);
  constant_iterator<double> equalWeights (1.0);  
  int                       nRows        (y[0].end()-y[0].begin());
  
  os << " Response has " << nRows << " rows.\n";
  if (useSubset)  // leading column is indicator of which cases to use in fitting
  {
    os << " Subset of cases defined by " << y[0] << "; response variable is " << y[1] << std::endl;
    return new gslData(y[1].begin(), y[0].begin(), equalWeights, nRows, gslRegression_Max_Q);
  } 
  else            // use all data for fitting
  {
    constant_iterator<bool>   noSelection(true);
    os << " Response variable is " << y[0] << std::endl;
    return new gslData(y[0].begin(),  noSelection , equalWeights, nRows, gslRegression_Max_Q);  
  } 
}


bool
data_has_selector(std::string const& dataFileName, std::ostream& os)
{
  std::ifstream input (dataFileName.c_str());
  if (input)
  { os << "Peeking at data file  " << dataFileName << " finds ";
    int i,j;
    std::string firstVarName;
    input >> i >> j >> firstVarName;
    os << "nrow=" << i << " ncol=" << j <<". Name of first variable is " << firstVarName << std::endl;
    return (firstVarName == "[in/out][in]");
  }
  else
  { std::cerr << "AUCT: *** Error ***  Could not open data file " << dataFileName << std::endl;
    return false;
  }
}
