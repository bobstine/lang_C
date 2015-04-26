/* 
  18 Apr 15 ... Lots of changes while at Yahoo: map features, rand proj
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


     Read columns from a file. The source file is laid out with one column of values per row.
     Line 1: gives the number of cases
     Line 2: name of the first variable
          3: description of first variable  (its property list)    
          4: data for the first variable
     Line 5: name of the second variable
          6: description of second variable
	  7: data for the second variable
     Line 6: name of the third variable      ...
     
     The reading is done by a FileColumnStream.  A column feature provides a named range
     of Scalars that learns a few properties of the data as it's read in (min, max, unique
     values). The space used by columns is allocated on reading in the function
     FileColumnStream.  Space is managed within each column.

*/

#include "build_helper.h"
#include "auction.Template.h"

// from ranges
#include "range.h" 
#include "range_ops.h"
#include "anonymous_iterator.h"

// templates
#include "features.Template.h"
#include "feature_transformations.Template.h"
#include "feature_predicates.Template.h"
#include "feature_iterators.Template.h"
#include "feature_streams.h"
#include "bidders.h"
#include "experts.Template.h"
 
// for constant iterator 
#include "iterators.h"

// from utils; debug has the printing facility
#include "debug.h"
#include "read_utils.h"     
#include "column.Template.h"
#include "column_stream.Template.h"
#include "light_threads.Template.h"
#include "simple_eigenword_dictionary.h"

#include "validated_regression.h"
#include "eigen_svd.h"

#include <iostream>
#include <fstream>
#include <vector>
#include <set>
#include <map>
#include <getopt.h>
#include <time.h> 
#include <assert.h>

inline
std::ostream&
operator<< (std::ostream& os, std::map<std::string, std::vector<std::string>> map)
{
  os << "[" << map.size() << "] ";
  for (auto p : map)
  { os << "{" << p.first << ", ";
    for (std::string n :p.second) os << n << " ";
    os << "} ";
  }
  return os;
}

void
parse_arguments(int argc, char** argv,
		std::string& yFileName, std::string& cFileName,
		std::string& vocabFileName, std::string& dictionaryFileName, int &dictionaryDim, 
		std::size_t &minCategorySize, std::string& xFileName,
		std::string& outputPath,
		int    &protection, bool   &shrink, int    &nRounds,
		SCALAR &totalAlpha, int    &gap,    int    &debugLevel, int    &maxNumOutputPredictors);

//     main     main     main     main     main     main     main     main     main     main     main     main     main

int
main(int argc, char** argv)
{
  using debugging::debug;
  using std::string;

  // Parse command line options
  Scalar   totalAlphaToSpend    ((Scalar)2.0);
  string   responseFileName     ("auction_data/Y_to");
  string   contextFileName      ("auction_data/cv_indicator");
  string   vocabFileName        ("vocabulary.txt");
  string   dictFileName         ("eigenwords.txt");
  int      dictDim              (200);
  size_t   minCategorySize      (2000);
  string   xFileName            ("auction_data/Xpipe_to");
  string   outputPath           ("auction_temp/to"); 
  int      protection           (  3);
  bool     useShrinkage       (false);
  int      numberRounds         (200);
  int      maxNumOutputPredictors (0);
  int      calibrationGap         (25);      // 0 means no calibration; otherwise gap between models offered calibration
  int      debugLevel             (1);

  // Lock these options
  const int  nPrefixCases = 0;
  const int  nContextCases = 0;
  const int  blockSize   = 1;

  parse_arguments(argc,argv, responseFileName, contextFileName, vocabFileName,
		  dictFileName, dictDim, minCategorySize, xFileName, outputPath,
		  protection, useShrinkage, numberRounds, totalAlphaToSpend,
		  calibrationGap, debugLevel, maxNumOutputPredictors);
  
  // Initialize log stream (write to clog if debugging is on, otherwise to auction.log file)
  if (outputPath[outputPath.size()-1] != '/') outputPath += "/";
  string   debugFileName (outputPath + "progress.log");
#ifdef NDEBUG
  std::ofstream logStream     (debugFileName.c_str());
  debugging::debug_init(logStream, debugLevel);
#else
  debugging::debug_init(std::clog, debugLevel);
#endif
  debug("AUCT",0) << "Version build 2.5 (17 Apr 2015) Fast regression with gradient, NLP features from mapped input.\n";

  // Write configuration and record to file
  string configuration;
  {
    configuration = "auction --y_file=" + responseFileName + " --c_file=" + contextFileName
      + " --vocabulary=" + vocabFileName + " --dictionary=" + dictFileName + " --dict_dim=" + std::to_string(dictDim)
      + " --min_cat_size=" + std::to_string(minCategorySize) + "  --x_file=" + xFileName
      + " --output-path=" + outputPath + " --cal_gap=" + std::to_string(calibrationGap)
      + " --debug-level=" + std::to_string(debugLevel) + " --protect=" + std::to_string(protection)
      + " --rounds=" + std::to_string(numberRounds) + " --output-x=" + std::to_string(maxNumOutputPredictors)
      + " --alpha=" + std::to_string(totalAlphaToSpend);
    if (useShrinkage) configuration += " --shrink";
    debug("AUCT",0) << configuration << std::endl;
    std::ofstream os (outputPath + "configuration");
    os << configuration << std::endl;
  }
  // open additional files for output  
  string        progressFileName (outputPath + "progress.txt");
  std::ofstream progressStream   (progressFileName.c_str());
  if (!progressStream)
  { std::cerr << "AUCT: *** Error ***  Cannot open file to write expert status stream " << progressFileName << std::endl;
    return -1;
  }
  string modelHTMLFileName  (outputPath + "model.html"); 
  string modelTextFileName  (outputPath + "model.txt");
  string modelDataFileName  (outputPath + "model_data.txt");
  debug("AUCT",2) << "Output going to these files:\n"
#ifdef NDEBUG
		  << "             log  --> " << debugFileName  << std::endl
#endif
		  << "      model data  --> " << modelDataFileName << std::endl
		  << "       model.txt  --> " << modelTextFileName << std::endl;  

  // Read response and associated control variables; read returns <n,k>
  typedef std::vector<Column<SCALAR>> ColumnVector;
  ColumnVector yColumns, cColumns;
  {
    std::pair<int,int> dim;
    dim = insert_numerical_columns_from_file (responseFileName, std::back_insert_iterator<ColumnVector>(yColumns));
    debug("MAIN",2) << "Y file returns dimension " << dim.first << "x" << dim.second << std::endl;
    if ((dim.first==0) || (dim.second==0)) return -1;
    dim = insert_numerical_columns_from_file (contextFileName,  std::back_insert_iterator<ColumnVector>(cColumns));
    debug("MAIN",2) << "Context file returns dimension " << dim.first << "x" << dim.second << std::endl;
    if ((dim.first==0) || (dim.second==0)) return -2;
    debug("MAIN",1) << "Input files produced " << yColumns.size() << " Ys and " << cColumns.size() << " context columns.\n";
  } 

  // build model and initialize auction with tab-delimited stream for tracking progress
  typedef Auction<Regression>  RegressionAuction;
  Regression theRegr = build_regression_model (yColumns[0], cColumns[0], nPrefixCases, blockSize, useShrinkage, debug("MAIN",2));
  const string calibrationSignature ("Y_hat_");
  RegressionAuction theAuction(theRegr, calibrationGap, calibrationSignature, blockSize, progressStream);
   
  // convert input columns into features arranged in streams
  ColumnVector xColumns;
  { 
    const bool downcase = false;
    debug("MAIN",3) << "Building vocabularty and dictionary.\n";
    Text::SimpleVocabulary vocab = Text::make_simple_vocabulary(vocabFileName, downcase);
    Text::SimpleEigenwordDictionary dict = Text::make_simple_eigenword_dictionary(dictFileName, dictDim, vocab, downcase);
    std::pair<int,int> dim;
    dim = insert_columns_from_file (xFileName, minCategorySize, dict, std::back_insert_iterator<ColumnVector>(xColumns));
    debug("MAIN",2) << "X column file produced " << dim.second << " features from " << dim.second << " variables." << std::endl;
    if ((dim.first==0) || (dim.second==0)) return -3;
  } 

  FeatureSource featureSource (xColumns, nPrefixCases);                            // holds *all* features constructed from input X
  featureSource.print_summary(debug("MAIN",0));
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
    debug("MAIN",1) << "Found " << streamNames.size() << " input source streams; " << lockedFeatures.size() << " features are locked." << std::endl;
    if(lockedFeatures.size()>0) 
    { theAuction.add_initial_features(lockedFeatures);
      debug("AUCT",1) << theAuction << std::endl << std::endl;
    }
  }

  debug("AUCT",3) << "Assembling experts"  << std::endl;
  const bool purgable = true;
  Scalar     alphaShare     (totalAlphaToSpend/(Scalar)streamNames.size());
  Scalar     alphaMain      (alphaShare * (Scalar)0.60);
  Scalar     alphaInt       (alphaShare * (Scalar)0.40);
  typedef FeatureStream< CyclicIterator      <FeatureVector, SkipIfInModel               >, Identity>  FiniteStream;
  //  typedef FeatureStream< InteractionIterator <FeatureVector, SkipIfIndicatorsOfSameParent>, Identity>  InteractionStream;
  typedef FeatureStream< CrossProductIterator<               SkipIfRelatedPair>           , Identity>  CrossProductStream;

  for (size_t s=0; s<streamNames.size(); ++s)
  { string streamName = streamNames[s];
    debug("MAIN",1) << "Allocating alpha $" << alphaShare << " to expert for stream " << streamName << std::endl;	
    FeatureVector fv = featureSource.features_with_attribute("stream", streamName);
    theAuction.add_expert(Expert("Strm["+streamNames[s]+"]", source, !purgable, nContextCases, alphaMain,
				 UniversalBoundedBidder<FiniteStream>(), 
				 make_cyclic_stream(streamName, fv, SkipIfInModel())));
  }

  if (true)  // cross streams with matching name prefix (eg: WR1_WORD, WR1_POS)
  { char delim = '_';
    std::map<string,std::vector<string>> prefixMap;
    for(string s : streamNames)
    { size_t pos = s.find(delim);
      if (pos != string::npos)
      { string prefix = s.substr(0,pos);
	prefixMap[prefix].push_back(s);
	debug("MAIN",4) << "TEST: prefix = " << prefix << " identified in stream with name " << s << std::endl;
      }
    }
    debug("MAIN",3) << "Prefix map used to build cross-products is " << prefixMap << std::endl;
    for (auto p : prefixMap)
    { if(p.second.size() == 2)
      { string streamName1 = (p.second)[0];
	string streamName2 = p.second[1];
	string name = streamName1 + "x" + streamName2;
	debug("MAIN",1) << "Making cross-product expert with names " << name << std::endl << std::endl;
	FeatureVector fv1 = featureSource.features_with_attribute("stream", streamName1);
	FeatureVector fv2 = featureSource.features_with_attribute("stream", streamName2);
	theAuction.add_expert(Expert("Cross["+name+"]", source, !purgable, nContextCases, alphaInt,       // less avoids tie 
				     UniversalBoundedBidder<CrossProductStream>(),
				     make_cross_product_stream(name, fv1, fv2)   ));
      }
    }
  }
  
  if (false)
  { // beams don't yet recognize the new feature structure??? 
    const int gap = 3;
    typedef FeatureStream< BeamIterator <RegressionAuction>, BeamConstructor<RegressionAuction> > BeamStream;
    theAuction.add_expert(Expert("Beam", beam, purgable, nContextCases, alphaInt,
				 UniversalBoundedBidder<BeamStream>(),			       
				 make_beam_stream("streams", theAuction, streamNames, gap)));
  }

  //  Calibration expert
  if(calibrationGap > 0)
  { bool yIsBinary  (yColumns[0]->is_dummy());
    if(yIsBinary) debug("AUCT",2) << "Response variable " << yColumns[0]->name() << " is binary; will truncate calibration estimates." << std::endl;
    theAuction.add_expert(Expert("Calibrator", calibrate, !purgable, nContextCases, 100,                                        // endow with lots of money
				 FitBidder((SCALAR)0.000005, calibrationSignature),                  
				 make_polynomial_calibration_stream("fitted_values", theRegr, calibrationGap, calibrationSignature,
								    nContextCases, yIsBinary))); 
  }

  
  // ----------------------   run the auction with output to file  ---------------------------------
  int round = 0;
  {
    theAuction.prepare_to_start_auction();
    const int minimum_residual_df = 10;                          // make sure don't try to fit more vars than cases
    Scalar totalTime              = 0.;
    std::pair<Scalar,Scalar> rss0{theAuction.model().sums_of_squares()};      // resid ss, cv ss
    const int fullOutputPeriod    = 25;
    int       fullOutputTimer     = fullOutputPeriod;
    bool      cvssCheck           = true;
    while(round<numberRounds && cvssCheck && !theAuction.is_terminating() && theAuction.model().residual_df()>minimum_residual_df)
    { ++round;
      clock_t start;
      start = clock();
      if (theAuction.auction_next_feature())                     // true when adds predictor; show the current model
      { --fullOutputTimer;
	if (fullOutputTimer == 0)
	{ fullOutputTimer = fullOutputPeriod;
	  debug("AUCT",1) << theAuction << std::endl << std::endl;
	}
	else
	  theAuction.print_to(debug("AUCT",2), true); 
      }
      Scalar time = (Scalar)time_since(start);
      totalTime += time;
      progressStream << std::endl;                               // ends lines in progress file in case abrupt exit
      std::pair<Scalar,Scalar> rss {theAuction.model().sums_of_squares()};
      if( 1.1*rss0.second < rss.second)
      { std::clog << "AUCT: *** Error *** CVSS grew by more than 10% in last round from " << rss0.second
		  << " to " << rss.second << "; exiting after writing model and full set of Xs.\n";
	cvssCheck = false;
	maxNumOutputPredictors = 2000;
      }
      else rss0 = rss;
      debug("AUCT",0) << "Round " << round <<  " used " << time
		      << "; current ss are (" << rss.first << "," << rss.second << ")"
		      << std::endl << std::endl;
    }
    std::cout << "\n      -------  Auction ends after " << round << "/" << numberRounds
	      << " rounds; average time " << totalTime/(Scalar)round << " per round \n\n" << theAuction << std::endl;
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
    output << configuration << std::endl << std::endl;
    theAuction.print_model_to(output);
    output << std::endl;
    std::vector<SCALAR> b = theAuction.model().beta();
    output  << "Beta[" << b.size() << "]" << std::endl;
    for(size_t i=0; i<b.size(); ++i)
    { if (0 == (i+1)%10) output << std::endl;
      output << " " << b[i];
    }
    output << std::endl;
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
		std::string & yFileName,         std::string & cFileName,
		std::string & vocabFileName,     std::string & dictFileName, int & dictDim,
		size_t & minCategorySize, std::string & xFileName,
		std::string & outputPath,
		int         &protection,	 bool        &shrink,           int         &nRounds,
		Scalar      &totalAlpha,         int         &calibrate,        int         &debugLevel,
		int         &maxOutputPredictors)
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
	  {"cal_gap",           1, 0, 'c'},  // has arg,
	  {"dictionary",        1, 0, 'D'},  // has arg,
	  {"dict_dim",          1, 0, 'd'},  // has arg,
	  {"output_x",          1, 0, 'k'},  // has arg
	  {"min_cat_size",      1, 0, 'm'},  // has arg
	  {"output_path",       1, 0, 'o'},  // has arg,
	  {"protection",        1, 0, 'p'},  // has arg,
	  {"rounds",            1, 0, 'r'},  // has arg,
	  {"shrinkage",         1, 0, 's'},  // has arg
	  {"vocabulary",        1, 0, 'V'},  // has arg	
	  {"debug",             1, 0, 'w'},  // has arg,
	  {"help",              0, 0, 'h'},  // no  arg, 
	  {0, 0, 0, 0}                       // terminator 
	};
	key = getopt_long (argc, argv, "Y:C:X:a:c:D:d:k:m:o:p:r:s:V:w:h", long_options, &option_index);
	if (key == -1)
	  break;
	// std::cout << "Option key " << char(key) << " with option_index " << option_index << std::endl;
	switch (key)
	  {
	  case 'Y' : { yFileName = optarg;                                         break; }
	  case 'C' : { cFileName = optarg;                                         break; }
	  case 'X' : { xFileName = optarg;                                         break; }
	  case 'a' : { totalAlpha = read_utils::lexical_cast<Scalar>(optarg);      break; }
	  case 'c' : { calibrate = read_utils::lexical_cast<int>(optarg);          break; }
	  case 'D' : { dictFileName = optarg;                                      break; }
	  case 'd' : { dictDim = read_utils::lexical_cast<int>(optarg);            break; }
	  case 'k' : { maxOutputPredictors = read_utils::lexical_cast<int>(optarg);break; }
	  case 'm' : { minCategorySize = read_utils::lexical_cast<size_t>(optarg); break; }
	  case 'o' : { outputPath = optarg;                                        break; }
	  case 'p' : { protection = read_utils::lexical_cast<int>(optarg);         break; }
	  case 'r' : { nRounds = read_utils::lexical_cast<int>(optarg);            break; }
	  case 's' : { int shk;shk = read_utils::lexical_cast<int>(optarg);
	               if (shk) shrink = true; else shrink = false;                break; }
	  case 'V' : { vocabFileName = optarg;                                     break; }
	  case 'w' : { debugLevel = read_utils::lexical_cast<int>(optarg);         break; }
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


 
