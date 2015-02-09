/* 
  Builds the auction using parameters from the input parameter file.  The first
  line of the parameters file uses these options:
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
	
  18 Jan 14 ... Parameter file used to configure auction.
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
   
#include "auction.Template.h"
#include "build_helper.h"

// from ranges
#include "range.h" 
#include "range_ops.h"
#include "anonymous_iterator.h"

// templates
#include "features.Template.h"
#include "feature_streams.Template.h"
#include "feature_iterators.Template.h"
#include "feature_predicates.Template.h"
#include "light_threads.Template.h"
#include "experts.Template.h"
#include "bidders.h"

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

void
parse_arguments(int argc, char** argv,
		std::string& yFileName, std::string& cFileName, std::string& xFileName, std::string& outputPath,
		int    &protection, bool   &shrink, int    &nRounds,
		double &totalAlpha, int    &gap,    int    &debugLevel, int    &maxNumOutputPredictors);


ValidatedRegression  build_regression_model(Column y, Column inOut, int prefixRows, int blockSize, bool shrink, std::ostream& os);
int                  parse_column_format(std::string const& dataFileName, std::ostream&);
Column               identify_cv_indicator(std::vector<Column> const& columns);
void                 round_elements_into_vector(Column const& c, std::vector<int>::iterator b);
 

int
main(int argc, char** argv)
{
  using debugging::debug;
  using std::string;

  debug("AUCT",0) << "Version build 2.0 (18 Jan 2015)\n";

  // Parse command line options
  
  double   totalAlphaToSpend    (0.1);
  string   responseFileName     ("Y");
  string   contextFileName      ("cv_indicator");
  string   xFileName            ("x.dat");
  string   outputPath           ("/home/bob/C/auctions/test/log/"); 
  int      protection           (  3);
  bool     useShrinkage       (false);
  int      numberRounds         (200);
  int      maxNumOutputPredictors    (0);
  int      calibration            (0);      // 0 means no calibration; otherwise gap between models offered calibration
  int      debugLevel             (3);

  // lock these options

  const int  nPrefixCases = 0;
  const int  nContextCases = 0;
  const int  blockSize   = 1;

  parse_arguments(argc,argv, responseFileName, contextFileName, xFileName, outputPath,
		  protection, useShrinkage, numberRounds, totalAlphaToSpend,
		  calibration, debugLevel, maxNumOutputPredictors);
  if (outputPath[outputPath.size()-1] != '/') outputPath += "/";
  
  // initialize log stream (write to clog if debugging is on, otherwise to auction.log file)
  
  string   debugFileName (outputPath + "progress.log");
  std::ofstream logStream     (debugFileName.c_str());
#ifdef NDEBUG
  debugging::debug_init(logStream, debugLevel);
#else
  debugging::debug_init(std::clog, debugLevel);
#endif 
  {
    string msg  = "auction --y_file=" + responseFileName + " --c_file=" + contextFileName + " --x_file=" + xFileName
      + " --output-path=" + outputPath + " --debug-level=" + std::to_string(debugLevel)
      + " --protect=" + std::to_string(protection) + " --rounds=" + std::to_string(numberRounds) + " --output-x="
      + std::to_string(maxNumOutputPredictors) + " --alpha=" + std::to_string(totalAlphaToSpend);
    if (useShrinkage) msg += " --shrink";
    debug("AUCT",0)   << msg << std::endl;
  }
  
  // open additional files for output
  
  string progressCSVFileName (outputPath + "progress.csv");
  std::ofstream progressStream (progressCSVFileName.c_str());
  if (!progressStream)
  { std::cerr << "AUCT: *** Error ***  Cannot open file to write expert status stream " << progressCSVFileName << std::endl;
    return -1;
  }
  string modelHTMLFileName  (outputPath + "model.html"); 
  string modelTextFileName  (outputPath + "model.txt");
  string modelDataFileName  (outputPath + "model_data.csv");
  debug("AUCT",2) << "Output going to these files:\n"
#ifdef NDEBUG
		  << "             log  --> " << debugFileName  << std::endl
#endif
		  << "      model data  --> " << modelDataFileName << std::endl
		  << "       model.txt  --> " << modelTextFileName << std::endl;  
  /* XF
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
     FileColumnStream.  Space is managed within each column.
  */

  // Read response and associated control variables; read returns <n,k>
    
  typedef std::vector<Column> ColumnVector;
  ColumnVector yColumns, xColumns, cColumns;
  {
    std::pair<int,int> dim;
    dim = insert_columns_from_file (responseFileName, std::back_insert_iterator<ColumnVector>(yColumns));
    debug("MAIN",2) << "Y file returns dimension " << dim.first << "x" << dim.second << std::endl;
    if ((dim.first==0) || (dim.second==0)) return -1;
    dim = insert_columns_from_file (contextFileName,  std::back_insert_iterator<ColumnVector>(cColumns));
    debug("MAIN",2) << "Context file returns dimension " << dim.first << "x" << dim.second << std::endl;
    if ((dim.first==0) || (dim.second==0)) return -2;
    dim = insert_columns_from_file (xFileName,        std::back_insert_iterator<ColumnVector>(xColumns));
    debug("MAIN",2) << "X file returns dimension " << dim.first << "x" << dim.second << std::endl;
    if ((dim.first==0) || (dim.second==0)) return -3;
    debug("MAIN",1) << "Input files produced "
		    << yColumns.size() << " Ys, "
		    << xColumns.size() << " Xs, and "
		    << cColumns.size() << " context columns.\n";
  }

  // Parse binary Y and partition X data into feature streams  (const settings lock out other features)
  
  if( yColumns[0]->is_dummy() )
    debug("AUCT",1) << "Response " << yColumns[0]->name() << " is binary; truncating calibration estimates." << std::endl;

  // build model and initialize auction with csv stream for tracking progress

  ValidatedRegression  theRegr = build_regression_model (yColumns[0], cColumns[0], nPrefixCases, blockSize, useShrinkage, debug("MAIN",2));
  const string calibrationSignature ("Y_hat_");
  Auction<  ValidatedRegression > theAuction(theRegr, calibration, calibrationSignature, blockSize, progressStream);
  
  // open input data stream
  
  FeatureSource featureSource (xColumns, nPrefixCases);
  featureSource.print_summary(debug("MAIN",1));
  std::vector<string> streamNames (featureSource.stream_names());
  {
    FeatureVector lockedFeatures;
    for(auto it = streamNames.begin(); it!=streamNames.end(); ++it)                // remove locked stream
    { if (*it == "LOCKED")
      { debug("MAIN",4) << "Found locked stream; it is not a bidding stream.\n";
	streamNames.erase(it);
	lockedFeatures = featureSource.features_with_attribute("stream", "LOCKED");
	break;
      }
    }
    debug("MAIN",1) << "Found " << streamNames.size() << " bidding streams; " << lockedFeatures.size() << " features are locked." << std::endl;
    if(lockedFeatures.size()>0) 
    { theAuction.add_initial_features(lockedFeatures);
      debug("AUCT",1) << theAuction << std::endl << std::endl;
    }
  }

  debug("AUCT",3) << "Assembling experts"  << std::endl;
  double   alphaShare     (totalAlphaToSpend/(double)streamNames.size());
  double   alphaMain      (alphaShare * 0.60);
  double   alphaInt       (alphaShare * 0.40);
  typedef FeatureStream< CyclicIterator      <FeatureVector, SkipIfInModel    >, Identity>  FiniteStream;
  typedef FeatureStream< InteractionIterator <FeatureVector, SkipIfRelatedPair>, Identity>  InteractionStream;
  //  typedef FeatureStream< CrossProductIterator<               SkipIfRelatedPair>, Identity>  CrossProductStream;
  //  double   alphaCP        (alphaShare * (hasLockFeatures ? 0.29 : 0    ));  //                        cross products

  std::vector<FeatureVector> featureVectors(streamNames.size());   // treat this guy with respect... lots of const refs to its elements
  
  for (int s=0; s < (int)streamNames.size(); ++s)
  { debug("MAIN",1) << "Allocating alpha $" << alphaShare << " to source experts for stream " << streamNames[s] << std::endl;	
    featureVectors[s] = featureSource.features_with_attribute("stream", streamNames[s]);
    theAuction.add_expert(Expert("Strm["+streamNames[s]+"]", source, nContextCases, alphaMain,
				 UniversalBoundedBidder<FiniteStream>(), 
				 make_finite_stream(streamNames[s], featureVectors[s], SkipIfInModel())));
    theAuction.add_expert(Expert("Interact["+streamNames[s]+"]", source, nContextCases, alphaInt,                  // less avoids tie 
				 UniversalBoundedBidder<InteractionStream>(),
				 make_interaction_stream("within " + streamNames[s], featureVectors[s], true)      // true implies include squared terms
				 ));
  }
    /*
      if (hasLockFeatures)                                                                                           // cross with locked stream
	theAuction.add_expert(Expert("CrossProd["+streamNames[s]+" x Lock]", source, nContextCases, alphaCP, 
				     UniversalBoundedBidder<CrossProductStream>(),
				     make_cross_product_stream("CP[" + streamNames[s] + " x Lock]",
							       featureVectors[s], lockedFeatures) 
				     ));
       }
    */

  
  // ----------------------   run the auction with output to file  ---------------------------------
  int round = 0;
  {
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
    { std::vector<string> names (theAuction.purged_expert_names());
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
    { std::cerr << "AUCT: Cannot open output file `" << modelDataFileName << "'for model data.\n";
      return 2;
    } 
    theAuction.write_model_data_to(output, maxNumOutputPredictors);  
    output.close();
  }
  debug("AUCT",3) << "Exiting; final clean-up done by ~ functions.\n";
  return 0;  
} 
  

void
parse_arguments(int argc, char** argv,
		std::string & yFileName,
		std::string & cFileName,
		std::string & xFileName,
		std::string & outputPath,
		int         &protection,
		bool        &shrink,
		int         &nRounds,
		double      &totalAlpha,
		int         &calibrate,
		int         &debugLevel,
		int         &maxNumOutputPredictors)
{
  int key;
  while (1)                                  // read until empty key causes break
    {
      int option_index = 0;
      static struct option long_options[] = {
	  {"y_file",            1, 0, 'Y'},  // has arg,
	  {"c_file",            1, 0, 'C'},  // has arg,
	  {"x_file",            1, 0, 'X'},  // has arg,
	  {"alpha",             1, 0, 'a'},  // has arg,
	  {"calibration",       1, 0, 'c'},  // has arg,
	  {"debug-level",       1, 0, 'd'},  // has arg,
	  {"output-x",          1, 0, 'k'},  // has arg
	  {"output-path",       1, 0, 'o'},  // has arg,
	  {"protection",        1, 0, 'p'},  // has arg,
	  {"rounds",            1, 0, 'r'},  // has arg,
	  {"shrinkage",         1, 0, 's'},  // has arg
	  {"help",              0, 0, 'h'},  // no  arg, 
	  {0, 0, 0, 0}                       // terminator 
	};
	key = getopt_long (argc, argv, "Y:C:X:a:c:d:k:o:p:r:s:h", long_options, &option_index);
	if (key == -1)
	  break;
	// std::cout << "Option key " << char(key) << " with option_index " << option_index << std::endl;
	switch (key)
	  {
	  case 'Y' : { yFileName = optarg;                                         break; }
	  case 'C' : { cFileName = optarg;                                         break; }
	  case 'X' : { xFileName = optarg;                                         break; }
	  case 'a' : { totalAlpha = read_utils::lexical_cast<double>(optarg);      break; }
	  case 'c' : { calibrate = read_utils::lexical_cast<int>(optarg);          break; }
	  case 'd' : { debugLevel = read_utils::lexical_cast<int>(optarg);         break; }
	  case 'k' : { maxNumOutputPredictors = read_utils::lexical_cast<int>(optarg);break; }
	  case 'o' : { outputPath = optarg;                                        break; }
	  case 'p' : { protection = read_utils::lexical_cast<int>(optarg);         break; }
	  case 'r' : { nRounds = read_utils::lexical_cast<int>(optarg);            break; }
	  case 's' : { int shk;shk = read_utils::lexical_cast<int>(optarg);
	               if (shk) shrink = true; else shrink = false;                break; }
	  case 'h' :
	    {
	      std::cout << "switches:" << std::endl << std::endl;
	      std::cout << "      --calibration=#      gap between calibration attempts" << std::endl;
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


 
