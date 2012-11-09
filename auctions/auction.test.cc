/*
  Run using commands in the Makefile to get the data setup properly (eg, make auction__test)
  Then execute code as
                    
           ./auction.test ... { options } 
	   
  where options are
	-a  total alpha to distribute           (default is 0.1... might ought to be less)
	-b  blocksize for white                 (default 0 is OLS; 1 for white; larger for corr)
	-c  calibration df                      (default is no calibration)
        -d  debug level of output
	-f  path for input data                 (default is est.dat)
	-k  max number of X columns written out (default 0 implies to write none)
	-o  path for output model results       (default is model)
	-p  level of protection                 (default is level 3)
        -r  number of rounds for the auction    (default is 50)
	-s  shrink the estimates                (default is 0; set to 1 to shrink)
	-x  number of leading extra cases       (default is 0; used for lagging)
	
  10 Mar 11 ... Lots of tweaks, including shrinkage parameter, calibration control.
  27 Nov 10 ... New stream types, with threads.
  21 Mar 10 ... More types of input information, neighborhoods and the context stream.	
   2 Mar 10 ... Look at 'dynamically' funding experts via tax on bids and earnings
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
#include "feature_streams.h"

// for constant iterator 
#include "iterators.h"

// from utils; debug has the printing facility
#include "column.h"
#include "debug.h"
#include "read_utils.h"     

#include "regression.h"
#include "eigen_svd.h"

#include <iostream>
#include <fstream>
#include <vector>
#include <set>
#include <map>
#include <getopt.h>
#include <time.h> 
#include <assert.h>
   

class FiniteCauchyShare
{
private:
  double mTotalAlpha;
  int    mCount;
  double mSum;

public:
  FiniteCauchyShare (double alpha, int count) :  mTotalAlpha(alpha), mCount(count), mSum(0.0)
  { for (int j=0; j<mCount; ++j)
      mSum = mSum + p(j);
  }

  double operator()(int j)
  { assert(j >= 0);
    assert(j < mCount);
    return mTotalAlpha * p(j) / mSum;
  }
  
private:
  double p(int j)
  { return 1.0/(double)((j+1)*(j+1)); }
};


double
time_since(time_t const& start)
{
  return  double(clock() - start)/CLOCKS_PER_SEC;
}

void
parse_arguments(int argc, char** argv,
		std::string& inputDataFile, 
		std::string& outputPath,
		int &protection, bool &useShrinkage,
		int &blockSize, int &nRounds, double &totalAlpha,
		int &calibrationGap, int &prefixCases, int &debugLevel,
		int &maxNumXCols);

std::pair< std::pair<int,double>, std::pair<int,double> >
initialize_sums_of_squares(std::vector<Column> y);

ValidatedRegression  build_regression_model(Column y, Column inOut, int prefixRows, int blockSize, bool shrink, std::ostream& os);
int                  parse_column_format(std::string const& dataFileName, std::ostream&);
Column               identify_cv_indicator(std::vector<Column> const& columns, int prefixCases);
void                 round_elements_into_vector(Column const& c, std::vector<int>::iterator b);


int
main(int argc, char** argv)
{
  using debugging::debug;
  typedef std::vector<Feature> FeatureVector;

  // build vector of columns from file; set default parameter values
  double        totalAlphaToSpend    (0.1);
  std::string   inputName            ("");                                  // empty implies cin
  std::string   outputPath           ("/Users/bob/C/auctions/test/log/"); 
  int           protection           (3);
  bool          useShrinkage         (false);
  int           shrink               (0);
  int           blockSize            (0);                                   // no blocking implies standard testing
  int           numberRounds         (200);
  int           numOutputColumns     (0);
  // int           splineDF             (0);
  int           calibrationGap       (0);                                   // 0 means no calibration; otherwise gap between models offered calibration
  int           prefixCases          (0);
  int           debugLevel           (3);
     

  parse_arguments(argc,argv, inputName, outputPath, protection, useShrinkage,
		  blockSize, numberRounds, totalAlphaToSpend,
		  calibrationGap, prefixCases, debugLevel, numOutputColumns);
  if(useShrinkage) shrink = 1;
  
  // initialize bugging stream (write to clog if debugging is on, otherwise to auction.log file)
  std::string   debugFileName (outputPath + "progress.log");
  std::ofstream logStream     (debugFileName.c_str());
#ifdef NDEBUG
  debugging::debug_init(logStream, debugLevel);
#else
  debugging::debug_init(std::clog, debugLevel);
#endif
  debug("AUCT",0) << "Version build 1.5 (1 Apr 2011)\n";
   
  // echo startup options to log file
  debug("AUCT",0) << "Echo of arguments...    --input-name=" << inputName << " --output-path=" << outputPath << " --debug-level=" << debugLevel
		  << " --protect=" << protection << " --shrinkage=" << shrink << " --blocksize=" << blockSize << " --rounds=" << numberRounds
		  << " --output-cols=" << numOutputColumns  
		  << " --alpha=" << totalAlphaToSpend << " --calibration-gap=" << calibrationGap << " --extra-cases=" << prefixCases
		  << std::endl;
  
  // open additional files for output
  std::string modelHTMLFileName  (outputPath + "model.html"); 
  std::string modelTextFileName  (outputPath + "model.txt");
  std::string modelDataFileName  (outputPath + "model_data.csv");
  debug("AUCT",2) << "Output going to these files:\n"
#ifdef NDEBUG
		  << "             log  --> " << debugFileName  << std::endl
#endif
		  << "      model data  --> " << modelDataFileName << std::endl
		  << "       model.txt  --> " << modelTextFileName << std::endl;  
  /* 
     Read columns from a file. The file is laid out with one column of values per row.
     Line 1: gives the number of cases
     Line 2: name of the first variable (the response)
          3: description of first variable     
          4: data for the response
     Line 5: name of the second variable  (X_1)
          6: description of second variable (its property list)
	  7: data for the second variable
     Line 6: name of the third variable (X_2) 
     ...
     
     The reading is done by a FileColumnStream.  A column feature provides a named range
     of doubles that learns a few properties of the data as it's read in (min, max, unique
     values). The space used by columns is allocated on reading in the function
     FileColumnStream.  Spaced is managed within each column.
  */

  NamedColumnInsertMap insertMap;
  std::vector<Column> yColumns;  insertMap.insert(std::make_pair("y",      std::back_inserter(yColumns)));
  std::vector<Column> xColumns;  insertMap.insert(std::make_pair("x",      std::back_inserter(xColumns)));
  std::vector<Column> cColumns;  insertMap.insert(std::make_pair("context",std::back_inserter(cColumns))); 

  if (inputName.empty())
    insert_columns_from_stream(std::cin, insertMap);
  else
  { debug("AUCT",3) << "Preparing to read columns from input file stream " << inputName << std::endl;
    std::ifstream inputFileStream(inputName.c_str());
    if (!inputFileStream)
    { std::clog << "Error: No input supplied; cannot open " << inputName << std::endl;
      return 2;
    }
    insert_columns_from_stream(inputFileStream, insertMap);
  }
   
  std::string message;
  if (inputName.empty())
    message = "Standard input cin";
  else
    message = "Input stream " + inputName;
  debug("MAIN",1) << message << " produced "
		  << yColumns.size() << " Ys, "
		  << xColumns.size() << " Xs, and "
		  << cColumns.size() << " context columns.\n";
  if (yColumns.empty())
  { debug("MAIN",-1) << "ERROR:  Data do not include variable with role y to be the response. Terminating.\n";
    return -1;
  }

  // check the cross validation indicator
  Column inOut;
  if(!cColumns.empty())
    inOut = identify_cv_indicator(cColumns, prefixCases); 

  // organize data into feature streams
  FeatureSource featureSrc (xColumns, prefixCases);
  featureSrc.print_summary(debug("MAIN",1));

  // initialize progress file to hold incremental round-by-round results
  std::string progressCSVFileName (outputPath + "progress.csv");
  std::ofstream progressStream (progressCSVFileName.c_str());
  if (!progressStream)
  { std::cerr << "AUCT: *** Error ***  Cannot open file to write expert status stream " << progressCSVFileName << std::endl;
    return -1;
  }

  // set up calibration options
  bool yIsBinary  (yColumns[0]->is_dummy());
  debug("AUCT",1) << "Response variable " << yColumns[0]->name() << " is binary; will truncate calibration estimates." << std::endl;
  std::string calibrationSignature ("Y_hat_");

  // build model and initialize auction with csv stream for tracking progress
  ValidatedRegression  theRegr = build_regression_model (yColumns[0], inOut, prefixCases, blockSize, useShrinkage, debug("MAIN",2));
  Auction<  ValidatedRegression > theAuction(theRegr, featureSrc, calibrationGap, calibrationSignature, blockSize, progressStream);
  
  // create the experts that control bidding in the auction
  debug("AUCT",3) << "Assembling experts"  << std::endl;
  int nContextCases (featureSrc.number_skipped_cases());
  typedef FeatureStream< CyclicIterator<FeatureVector, SkipIfInModel>, Identity>                             FiniteStream;
  typedef FeatureStream< InteractionIterator<FeatureVector, SkipIfRelatedPair>, Identity>                    InteractionStream;
  typedef FeatureStream< CrossProductIterator<SkipIfRelatedPair>, Identity>                                  CrossProductStream;
  typedef FeatureStream< DynamicIterator<FeatureVector, SkipIfDerived>, BuildPolynomialFeatures >            PolynomialStream;
  typedef FeatureStream< DynamicIterator<FeatureVector, SkipIfDerived>,BuildNeighborhoodFeature>             NeighborhoodStream;
  typedef FeatureStream< ModelIterator<ValidatedRegression>, BuildCalibrationFeature<ValidatedRegression> >  CalibrationStream;
  typedef FeatureStream< BundleIterator<FeatureVector, SkipIfInBasis>, EigenAdapter<PCA> >                   PCAStream;
  typedef FeatureStream< BundleIterator<FeatureVector, SkipIfInBasis>, EigenAdapter<RKHS<Kernel::Radial> > > RKHSStream;
  
  // scavenger experts
  /*
    theAuction.add_expert(Expert("In*Out", parasite, nContextCases, 0,
			       UniversalBidder<CrossProductStream>(),
			       make_cross_product_stream("accept x reject", theAuction.model_features(), theAuction.rejected_features()) ));

  
  theAuction.add_expert(Expert("Poly", parasite, nContextCases, 0,
			       UniversalBidder< PolynomialStream >(),
			       make_polynomial_stream("Skipped-feature polynomial", theAuction.rejected_features(), 3)     // poly degree
			       ));
  */

  // if find neighborhood index, then build a neighborhood stream
  IntegerColumn indices();
  for(unsigned int i=0; i<cColumns.size(); ++i)
  { if (cColumns[i]->name() == "Pop_Neighbor")
    { debug("MAIN",1) << "Data include a neighborhood context variable.\n";
      IntegerColumn indices(cColumns[i]);
      theAuction.add_expert(Expert("Neighborhood", parasite, nContextCases, 0,
				   UniversalBidder< NeighborhoodStream >(),
				   make_neighborhood_stream("Neighborhood", theAuction.rejected_features(), indices)
				   ));
    }
  }

  // build a source and interaction expert for each stream with role=x, but not for the locked stream
  std::vector<std::string> streamNames (featureSrc.stream_names());
  FeatureVector lockedStream;
  for(std::vector<std::string>::iterator it = streamNames.begin(); it!=streamNames.end(); ++it)
  { if (*it == "LOCKED")
    { debug("MAIN",4) << "Note that the locked stream is not a bidding stream.\n";
      streamNames.erase(it);
      lockedStream =  featureSrc.features_with_attribute("stream", "LOCKED");
      break;
    }
  }
  debug("MAIN",1) << "Found " << streamNames.size() << " bidding streams; locked stream has " << lockedStream.size() << " features." << std::endl;
  // allocate alpha for main, interaction and cross-product with locked input source streams
  std::vector< FeatureVector> featureStreams(streamNames.size());
  { bool     hasLockStream (lockedStream.size() > 0);
    double   alphaShare    (totalAlphaToSpend/streamNames.size());
    double   alphaMain     (alphaShare * (hasLockStream ? 0.40 : 0.60 ));  // percentage of alpha to features as given
    double   alphaInt      (alphaShare * (hasLockStream ? 0.31 : 0.40 ));  //                        interactions of given
    double   alphaCP       (alphaShare * (hasLockStream ? 0.29 : 0    ));  //                        cross products
    for (int s=0; s < (int)streamNames.size(); ++s)
    { debug("MAIN",1) << "Allocating alpha $" << alphaShare << " to source experts for stream " << streamNames[s] << std::endl;	
      featureStreams[s] = featureSrc.features_with_attribute("stream", streamNames[s]);
      theAuction.add_expert(Expert("Strm["+streamNames[s]+"]", source, nContextCases, alphaMain,
				   UniversalBoundedBidder<FiniteStream>(), 
				   make_finite_stream(streamNames[s],featureStreams[s], SkipIfInModel())));
      theAuction.add_expert(Expert("Interact["+streamNames[s]+"]", source, nContextCases, alphaInt,                  // less avoids tie 
				   UniversalBoundedBidder<InteractionStream>(),
				   make_interaction_stream("within " + streamNames[s],
							   featureStreams[s], true)                                  // true means to include squared terms
				   ));
      if (hasLockStream)                                                                                             // cross with locked stream
	theAuction.add_expert(Expert("CrossProd["+streamNames[s]+" x Lock]", source, nContextCases, alphaCP, 
				     UniversalBoundedBidder<CrossProductStream>(),
				     make_cross_product_stream("CP[" + streamNames[s] + " x Lock]",
							       featureStreams[s], lockedStream )                     
				     ));
    }
  }
   
  //  Calibration expert
  if(calibrationGap > 0)
    theAuction.add_expert(Expert("Calibrator", calibrate, nContextCases, 100,                                        // endow with lots of money
				 FitBidder(0.000005, calibrationSignature),                  
				 make_calibration_stream("fitted_values", theRegr, calibrationGap, calibrationSignature,
							 nContextCases, yIsBinary)));

  //   Principle component type features
  theAuction.add_expert(Expert("PCA", source, nContextCases, totalAlphaToSpend/6,                                    // kludge alpha share
			       UniversalBidder<PCAStream>(),
			       make_subspace_stream("PCA", 
						    theAuction.rejected_features(),
						    EigenAdapter<PCA>(PCA(0, true), "PCA", nContextCases),           // # components, standardize? (0 means sing values)
						    30))) ;                                                          // bundle size

  //   RKHS stream
  theAuction.add_expert(Expert("RKHS", source, nContextCases, totalAlphaToSpend/6,
			       UniversalBidder<RKHSStream>(),
			       make_subspace_stream("RKHS", 
						    theAuction.rejected_features(),
						    EigenAdapter<RKHS<Kernel::Radial> >(RKHS<Kernel::Radial>(0, true), "RKHS", nContextCases),
						    30)));
  
  // ----------------------   run the auction with output to file  ---------------------------------
  int round = 0;
  {
    FeatureVector lockIn = featureSrc.features_with_attribute ("stream", "LOCKED");
    if(lockIn.size() > 0)
    { theAuction.add_initial_features(lockIn);
      debug("AUCT",1) << theAuction << std::endl << std::endl;
    }
    theAuction.prepare_to_start_auction();
    const int minimum_residual_df = 10;                          // make sure don't try to fit more vars than cases
    double totalTime (0.0);
    while(round<numberRounds && !theAuction.is_terminating() && theAuction.model().residual_df()>minimum_residual_df)
    { ++round;
      clock_t start;
      start = clock();
      if (theAuction.auction_next_feature())                     // true when adds predictor; show the current model
      	debug("AUCT",1) << theAuction << std::endl << std::endl;
      double time = time_since(start);
      totalTime += time;
      debug("AUCT",0) << "Round " << round <<  " used " << time << std::endl;
      progressStream << std::endl;                               // ends lines in progress file in case abrupt exit
    }
    std::cout << "\n      -------  Auction ends after " << round << "/" << numberRounds
	      << " rounds; average time " << totalTime/round << " per round \n\n" << theAuction << std::endl;
    { std::vector<std::string> names (theAuction.purged_expert_names());
      std::cout << "\n During the auction, there were " << names.size() << " purged experts: \n";
      for(unsigned int i=0; i<names.size(); ++i)
	std::cout << "  [" << i+1 << "]  " << names[i] << std::endl;
      std::cout << std::endl;
    }
  }
  // ----------------------   write summary and data to various files  ---------------------------------
  // write model in HTML to a file
  {
    std::ofstream output (modelHTMLFileName.c_str());
    if (! output)
    { std::cerr << "AUCT: Cannot open output HTML file for writing final model " << modelHTMLFileName << std::endl;
      return 1;
    }
    theAuction.print_model_to(output, true);  // true -> use HTML
    output.close();
  }
  
  // write model to a file
  {
    debug(2) << "Writing model to file " << modelTextFileName << std::endl;
    std::ofstream output (modelTextFileName.c_str());
    if (! output)
    { std::cerr << "AUCT: Cannot open output text file for writing model " << modelTextFileName << std::endl;
      return 1;
    }
    theAuction.print_model_to(output);
    output.close();
  }
  
  // write model data to file
  {
    debug(2) << "Writing model data to file " << modelDataFileName << std::endl;
    std::ofstream output (modelDataFileName.c_str());
    if (! output)
    { std::cerr << "AUCT: Cannot open output file for model data " << modelDataFileName << std::endl;
      return 2;
    } 
    theAuction.write_model_data_to(output, numOutputColumns);
    output.close();
  }
  debug("AUCT",3) << "Exiting; final clean-up done by ~ functions.\n";
  return 0;  
} 
  


void
parse_arguments(int argc, char** argv,
		std::string& inputFile,
		std::string& outputPath,
		int    &protection,
		bool   &shrink,
		int    &blockSize,
		int    &nRounds,
		double &totalAlpha,
		int    &gap,        int    &prefixCases,
		int    &debugLevel, int    &numOutputColumns)
{
  int key;
  while (1)                                  // read until empty key causes break
    {
      int option_index = 0;
      static struct option long_options[] = {
	  {"alpha",             1, 0, 'a'},  // has arg,
	  {"blocksize",         1, 0, 'b'},  // has arg,
	  {"calibration-gap",   1, 0, 'c'},  // has arg,
	  {"debug-level",       1, 0, 'd'},  // has arg,
	  {"input-file",        1, 0, 'f'},  // has arg,
	  {"output-columns",    1, 0, 'k'},  // has arg
	  {"output-path",       1, 0, 'o'},  // has arg,
	  {"protection",        1, 0, 'p'},  // has arg,
	  {"rounds",            1, 0, 'r'},  // has arg,
	  {"shrinkage",         1, 0, 's'},  // has arg
	  {"extra-cases",       1, 0, 'x'},  // has arg
	  {"help",              0, 0, 'h'},  // no  arg, 
	  {0, 0, 0, 0}                       // terminator 
	};
	key = getopt_long (argc, argv, "a:b:c:d:f:k:o:p:r:s:x:h", long_options, &option_index);
	if (key == -1)
	  break;
	// std::cout << "Option key " << char(key) << " with option_index " << option_index << std::endl;
	switch (key)
	  {
	  case 'a' : 
	    {
	      totalAlpha = read_utils::lexical_cast<double>(optarg);
	      break;
	    }
	  case 'b' :
	    {
	      blockSize = read_utils::lexical_cast<int>(optarg);
	      break;
	    }
	  case 'c' : 
	    {
	      gap = read_utils::lexical_cast<int>(optarg);
	      break;
	    }
	  case 'd' : 
	    {
	      debugLevel = read_utils::lexical_cast<int>(optarg);
	      break;
	    }
	  case 'f' :                                    
	    {
	      std::string name(optarg);
	      inputFile = name;
	      break;
	    }
	  case 'k' : 
	    {
	      numOutputColumns = read_utils::lexical_cast<int>(optarg);
	      break;
	    }
	  case 'o' :  
	    {
	      outputPath = optarg;
	      break;
	    }
	  case 'p' :
	    {
	      protection = read_utils::lexical_cast<int>(optarg);
	      break;
	    }
	  case 'r' :
	    {
	      nRounds = read_utils::lexical_cast<int>(optarg);
	      break;
	    }
	  case 's' :
	    {
	      int shk;
	      shk = read_utils::lexical_cast<int>(optarg);
	      if (shk) shrink = true; else shrink = false;
	      break;
	    }
	  case 'x' :
	    {
	      prefixCases = read_utils::lexical_cast<int>(optarg);
	      break;
	    }
	  case 'h' :
	    {
	      std::cout << "switches:" << std::endl << std::endl;
	      std::cout << "      --calibration-gap=#      gap between calibration attempts" << std::endl;
	      std::cout << "      -c4" << std::endl <<  std::endl;
	      std::cout << "      --input-file=foo         input file" << std::endl;
	      std::cout << "      -ifoo" << std::endl << std::endl;
	      std::cout << "      --output-path=/home/.../ path for output files" << std::endl;
	      std::cout << "      -o/home/.../" << std::endl << std::endl;
	      std::cout << "      --alpha=#                total alpha for experts " << std::endl;
	      std::cout << "      -a0.5" << std::endl << std::endl;
	      std::cout << "      --protect=#              protection level against false positive" << std::endl;
	      std::cout << "      -p#" << std::endl << std::endl;
	      std::cout << "      --rounds=#               maximum number of rounds of auction" << std::endl;
	      std::cout << "      -r#" << std::endl << std::endl;
	      std::cout << "      --extra-cases=#          extra cases used in building features" << std::endl;
	      std::cout << "      -s#" << std::endl << std::endl;
	      std::cout << "      --shrinkage=#            nonzero value means to use shrinkage" << std::endl;
	      std::cout << "      -x#" << std::endl << std::endl;
	      std::cout << "      --debug-level=#          0 for little, 5 for copious" << std::endl;
	      std::cout << "      -d#" << std::endl << std::endl;
	      std::cout << "      --help                   generates this message" << std::endl;
	      std::cout << "      -h" << std::endl << std::endl;
	      exit(0);
	      break;
	    }
	  }
    }
}

  
Column
identify_cv_indicator(std::vector<Column> const& columns, int prefixCases)
{ // have some weird allocation/bad pointer issues around assigning an empty column; problem was here; avoid by returning Column()
  debug("MAIN",3) << "Checking for CV indicator variable among " << columns.size() << " columns. "
		  << " First column is named " << columns[0]->name() << " with size = " << columns[0]->size() << std::endl
		  << columns[0] << std::endl;
  if (columns.empty())
  { debug("MAIN",0) << "Data lack CV indicator.\n";
    return Column();
  }
  if ((columns[0]->name() != "[in/out][in]") && (columns[0]->name() != "cv.indicator[in]"))
  { debug("MAIN",0) << "First context column is not in/out indicator; found '" << columns[0]->name()
		    << "' instead. Using all cases for estimation.\n";
    return Column();
  }
  // check name of the first context column, verify its a dummy variable
  if (columns[0]->is_dummy())
  { double sum (0.0);
    for (double *b (columns[0]->begin() + prefixCases); b != columns[0]->end() ; ++ b)
      sum += *b;
    debug("MAIN",0) << "CV indicator variable is " << columns[0]->name() << " with sum " << sum
		    << " estimation cases after skipping " << prefixCases << " leading cases.\n";
    return columns[0];
  }
  else // explain why its not a dummy variable
  { debug("MAIN",0) << "ERROR: CV indicator variable '" << columns[0]->name() << "' is not a dummy variable. Use all cases.\n";
    columns[0]->print_to(debug("MAIN",0));
    debug("MAIN",0) << std::endl << std::endl;
    return Column();
  }
}

 
// reads in response, initialized data object
ValidatedRegression
build_regression_model(Column y, Column inOut, int prefixRows, int blockSize, bool useShrinkage, std::ostream& os)
{
  bool                      useSubset    (0 != inOut->size());
  constant_iterator<double> equalWeights (1.0);
  int                       nRows        ((int)y->size()-prefixRows);
  
  os << "Building regression with " << y->size() << "-" << prefixRows << "=" << nRows << " cases; response is " << y;
  if (useShrinkage)
    os << " with shrinkage." << std::endl;
  else
    os << " without shrinkage." << std::endl;
  if (useSubset)
  { os << "        Validation cases identified by " << inOut << std::endl;
    return ValidatedRegression(y->name(), y->begin()+prefixRows, inOut->begin()+prefixRows, nRows, blockSize, useShrinkage);
  } 
  else
  { os << "        No validation.\n";
    constant_iterator<bool>   noSelection(true);
    return ValidatedRegression (y->name(), y->begin()+prefixRows, noSelection             , nRows, blockSize, useShrinkage);  
  } 
}

