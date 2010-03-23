#define LINEAR_MODEL

/*
  Run using commands in the Makefile to get the data setup properly (eg, make auction__test)
  Then execute code as
  
          auction.test -f filename -o path -r rounds -c calibration_df -v

  where
        -r  number of rounds for the auction (default is 50)
        -f  path for input data              (default is est.dat)
	-o  path for output model results    (default is model)
	-a  total alpha to distribute        (default is 0.1... might ought to be less)
	-p  level of protection              (default is level 3)
	-c  calibration df                   (default is no calibration)


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

// for constant iterator 
#include "iterators.h"

// from utils; debug has the printing facility
#include "column.h"
#include "debug.h"
#include "read_utils.h"

// from gsl_tools
#include "gsl_model.h"

#include "smoothing_spline.h"

#include <iostream>
#include <fstream>
#include <vector>
#include <set>
#include <map>
#include <getopt.h>



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


void
parse_arguments(int argc, char** argv,
		std::string& inputDataFile, 
		std::string& outputPath,
		int &protection, int &blockSize,
		int &nRounds, double &totalAlpha,
		int &df, int &extraCases);

std::pair< std::pair<int,double>, std::pair<int,double> >
initialize_sums_of_squares(std::vector<Column> y);

gslData*
build_model_data(std::vector<Column> const& y, int skip, std::ostream& os);

int
parse_column_format(std::string const& dataFileName, std::ostream&);




int
main(int argc, char** argv)
{
  using debugging::debug;
  typedef std::vector<Feature> FeatureVector;
  
  // build vector of columns from file; set default parameter values
  double      totalAlphaToSpend    (0.1);
  std::string columnFileName       ("/Users/bob/C/gsl_tools/data/bank_post45.dat");   
  std::string outputPath           ("/Users/bob/C/auctions/test/log/"); 
  int         protection           (3);
  int         blockSize            (1);
  int         numberRounds         (200); 
  int         splineDF             (0);
  int         extraCases           (0);
  
  // parse arguments from command line  (pass in at main, open file for printing messages)
  parse_arguments(argc,argv, columnFileName, outputPath, protection, blockSize,
		  numberRounds, totalAlphaToSpend,
		  splineDF, extraCases);
  
  // initialize bugging stream (write to clog if debugging is on, otherwise to auction.log file)
  std::string   debugFileName (outputPath + "progress.log");
  std::ofstream logStream     (debugFileName.c_str());
#ifdef NDEBUG
  debugging::debug_init(logStream, -1);
#else
  debugging::debug_init(std::clog, -1);
#endif
  
  // echo startup options to log file
  debug("AUCT",4) << "Version build 0.90 (27 May 2009)\n";
  debug("AUCT",4) << "Arguments    --input-file=" << columnFileName << " --output-path=" << outputPath
		  << " --protect=" << protection << " --blocksize=" << blockSize << " --rounds=" << numberRounds
		  << " --alpha=" << totalAlphaToSpend << " --calibrator-df=" << splineDF << " --extra-cases=" << extraCases
		  << std::endl;
  
  // open additional files for output
  std::string modelHTMLFileName  (outputPath + "model.html"); 
  std::string modelTextFileName  (outputPath + "model.txt");
  std::string modelDataFileName  (outputPath + "model_data.csv");
  debug("AUCT",3) << "Output going to these files:\n"
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
  int numberYColumns (parse_column_format (columnFileName, debug("MAIN",0)));   // 1 no validation, 2 if there's an in/out indicator
  std::vector<Column> yColumns;
  std::vector<Column> xColumns;
  insert_columns_from_file(columnFileName, numberYColumns, back_inserter(yColumns), back_inserter(xColumns));
  debug("AUCT",1) << "Data file " << columnFileName << " produced " << yColumns.size() << " Ys and "  << xColumns.size() << " Xs.\n";
		      
  // initialize data object held in underlying model [y and optional selector]
  gslData *theData (build_model_data(yColumns, extraCases, debug("MAIN",1)));
  
  // organize data into feature streams
  FeatureSource featureSrc (xColumns, extraCases);
  featureSrc.print_summary(debug("AUCT",1));
    
  // --- build model and initialize auction with stream for log
#ifdef LINEAR_MODEL
  LinearModel <gslData, olsEngine> theRegr(theData, protection, blockSize);
  Auction<  LinearModel <gslData, olsEngine> > theAuction(theRegr, featureSrc, splineDF, blockSize, debug(0));
#else
  // --- build logisitic model and auction
  LogisticModel <gslData> theRegr(theData, protection, blockSize);
  Auction<  LogisticModel <gslData> > theAuction(theRegr, featureSrc, splineDF, blockSize, debug(0));
#endif


  // build vector of experts that work directly from input variables
  debug("AUCT",0) << "Assembling experts"  << std::endl;

  typedef  CrossProductStream< FeatureVector, FeatureVector > CPStream;
  // parasitic experts
  theAuction.add_expert(Expert(parasite, featureSrc.number_skipped_cases(), 0,
			       UniversalBidder<CPStream>(),
			       make_cross_product_stream("Skipped-feature interactions",
							 featureSrc.features_with_attribute("stream", "main"), theAuction.rejected_features())
			       ));
  
  theAuction.add_expert(Expert(parasite, featureSrc.number_skipped_cases(), 0,
			       UniversalBidder< PolynomialStream<FeatureVector> >(),
			       make_polynomial_stream("Skipped-feature polynomial", theAuction.rejected_features(), 3)     // poly degree
			       ));

  // add a source column and interaction expert for each column stream *unless* its the context stream (used for neighbors)
  { std::vector<std::string> streamNames (featureSrc.stream_names());
    int numberBiddingStreams (0);
    for(std::vector<std::string>::const_iterator it = streamNames.begin(); it!=streamNames.end(); ++it)
      if(*it != "context")
	++numberBiddingStreams;
    FiniteCauchyShare alphaShare (totalAlphaToSpend, numberBiddingStreams);   // spreads wealth among streams
    typedef  FiniteStream      < FeatureVector > FStream;
    typedef  InteractionStream < FeatureVector > IStream;
    for (int s=0; s < (int) streamNames.size(); ++s)
    { if (streamNames[s] != "context")
      { debug("AUCT",2) << "Allocating alpha $" << alphaShare(s) << " to the source experts for stream " << streamNames[s] << std::endl;	
	theAuction.add_expert(Expert(source, featureSrc.number_skipped_cases(), alphaShare(s) * 0.52,      // priority, alpha
				     UniversalBoundedBidder<FStream>(), 
				     make_finite_stream("Columns of " + streamNames[s],
							featureSrc.features_with_attribute("stream", streamNames[s]), 2) // 2 cycles through these features
				     ));
	theAuction.add_expert(Expert(source, featureSrc.number_skipped_cases(), alphaShare(s) * 0.48,     // slightly less to avoid tie 
				     UniversalBoundedBidder<IStream>(),
				     make_interaction_stream("Column interactions of " + streamNames[s],
							     featureSrc.features_with_attribute("stream",streamNames[s]), false)  // skip squared terms
				     ));
      }
    }
  }

  
  /*  old version: one expert for all interactions; new code adds when variable is added (see auction.Template.h
    theAuction.add_expert(Expert(source, 0,
			       UniversalBidder<CPStream>(),
			       make_cross_product_stream("Used-feature interactions", featureVectorMap["main"], theAuction.accepted_features())
			       ));
  */
  // calibration expert
  if(splineDF > 0)
  { std::string signature("Y_hat_");
    theAuction.add_expert(Expert(calibrate, 0, 100,        // no skipping, lots of alpha
				 FitBidder(4, signature),  // delay between bursts
				 make_fit_stream(theRegr, signature)));
  }
  
    
  /*
    Principle component type features
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
  

  // set up file for writing state of auction
  std::string progressCSVFileName (outputPath + "progress.csv");
  std::ofstream progressStream (progressCSVFileName.c_str());
  if (!progressStream)
    { std::cerr << "AUCT: *** Error ***  Cannot open file to write expert status stream " << progressCSVFileName << std::endl;
      return -1;
    }

  
  // run the auction with output to file
  {
    int round = 0;
    const int minimum_residual_df = 10;
    while(round<numberRounds && theAuction.has_active_expert() && theAuction.model().residual_df()>minimum_residual_df)
    {
      ++round;
      if (theAuction.auction_next_feature(progressStream)) // true when adds predictor
      { debug(3) << " @@@ Auction adds predictor @@@" << std::endl;
	debug(3) << theAuction << std::endl << std::endl;
      }
      progressStream << std::endl;                        // ends lines in progress file
    }
    debug(4) << "\n      -------  Auction ends after " << round << "/" << numberRounds << " rounds.   ------ \n\n" << theAuction << std::endl;
  }

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
    theAuction.write_model_data_to(output);
    output.close();
  }
   
  debug(0) << "Auction is completed; disposing GSL data objects.\n";
  delete (theData);
  debug(0) << "Exiting; final clean-up done by ~ functions.\n";
  
  return 0;  
}




void
parse_arguments(int argc, char** argv,
		std::string& inputFile,
		std::string& outputPath,
		int    &protection,
		int    &blockSize,
		int    &nRounds,
		double &totalAlpha,
		int    &nDF,
		int    &extraCases)
{
  int key;
  while (1)                                  // read until empty key causes break
    {
      int option_index = 0;
      static struct option long_options[] = {
	  {"alpha",             1, 0, 'a'},  // has arg,
	  {"calibrator-df",     1, 0, 'c'},  // has arg,
	  {"input-file",        1, 0, 'f'},  // has arg,
	  {"output-path",       1, 0, 'o'},  // has arg,
	  {"protection",        1, 0, 'p'},  // has arg,
	  {"blocksize",         1, 0, 'b'},  // has arg,
	  {"rounds",            1, 0, 'r'},  // has arg,
	  {"extra-cases",       1, 0, 'x'},  // has arg
	  {"help",              0, 0, 'h'},  // no  arg, 
	  {0, 0, 0, 0}                       // terminator 
	};
	key = getopt_long (argc, argv, "a:c:f:o:p:b:r:x:h", long_options, &option_index);
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
	  case 'c' : 
	    {
	      nDF = read_utils::lexical_cast<int>(optarg);
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
	  case 'p' :
	    {
	      protection = read_utils::lexical_cast<int>(optarg);
	      break;
	    }
	  case 'b' :
	    {
	      blockSize = read_utils::lexical_cast<int>(optarg);
	      break;
	    }
	  case 'r' :
	    {
	      nRounds = read_utils::lexical_cast<int>(optarg);
	      break;
	    }
	  case 'x' :
	    {
	      extraCases = read_utils::lexical_cast<int>(optarg);
	      break;
	    }
	  case 'h' :
	    {
	      std::cout << "switches:" << std::endl << std::endl;
	      std::cout << "      --calibrator-df=#        degrees of freedom in spline calibrator" << std::endl;
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
	      std::cout << "      -x#" << std::endl << std::endl;
	      std::cout << "      --help                   generates this message" << std::endl;
	      std::cout << "      -h" << std::endl << std::endl;
	      exit(0);
	      break;
	    }
	  }
    }
}

// reads in response, initialized data object
gslData*
build_model_data(std::vector<Column> const& y, int skip, std::ostream& os)
{
  int                       nyCols       (y.size());
  bool                      useSubset    (nyCols == 2);
  constant_iterator<double> equalWeights (1.0);
  int                       nRows        ((int)y[0]->size()-skip);
  
  if (useSubset)  // leading column is indicator of which cases to use in fitting
  { os << " Subset of cases defined by " << y[0] << ";\n      response variable is " << y[1] << std::endl;
    return new gslData(y[1]->begin()+skip, y[0]->begin()+skip, equalWeights, nRows, gslRegression_Max_Q);
  } 
  else            // use all data for fitting
  { constant_iterator<bool>   noSelection(true);
    os << "Response variable is " << y[0] << std::endl;
    return new gslData(y[0]->begin()+skip,  noSelection , equalWeights, nRows, gslRegression_Max_Q);  
  } 
}


int
parse_column_format(std::string const& dataFileName, std::ostream& os)
{
  std::ifstream input (dataFileName.c_str());
  if (input)
  { os << "Peeking at data file  " << dataFileName << " finds ";
    int i,j;
    std::string firstVarName;
    input >> i >> j >> firstVarName;
    os << "nrow=" << i << " ncol=" << j <<". Name of first variable is " << firstVarName << std::endl;
    if (firstVarName == "[in/out][in]")
      return 2;
    else
      return 1;
  }
  else
  { std::cerr << "AUCT: *** Error ***  Could not open data file " << dataFileName << std::endl;
    return 0;
  }
}

/*   No longer used...
std::pair< std::pair<int,double>, std::pair<int,double> >
initialize_sums_of_squares( std::vector<Column> y)
{
  double inSS  (0.0);
  double outSS (0.0);
  int n0 (0), n1 (0);
  if (y.size() == 2) // have validation test sample
  { double avg0 (0.0), avg1 (0.0);
    double *x = y[1]->begin();
    double *b = y[0]->begin();
    for(int i = 0; i<y[0]->size(); ++i)
    { if (*b++)
      {	avg1 += *x++;
	++n1;
      }
      else
      { avg0 += *x++;
	++n0;
      }
    }
    if ((0 == n0) || (0 == n1))
    { std::cout << "ERROR: counts in in-sample/out-of-sample data are " << n0 << " " << n1 << std::endl;
      return std::make_pair( std::make_pair(0,0), std::make_pair(0,0));
    }
    avg0 = avg0 / n0;
    avg1 = avg1 / n1;
    x = y[1]->begin();
    b = y[0]->begin();
    for (int i=0; i<y[0]->size(); ++i)
    {
      double dev;
      if (*b++)
      { dev = *x++ - avg1;
	inSS += dev * dev;
      }
      else
      { dev = *x++ - avg0;
	outSS += dev*dev;
      }
    }
  }
  else
  { double avg = y[0]->average();
    double  *x = y[0]->begin();
    for(int i = 0; i<y[0]->size(); ++i)
    { double dev = *x++ - avg;
      inSS += dev*dev;
    }
  }
  return std::make_pair( std::make_pair(n1, inSS), std::make_pair(n0, outSS) );
}
  
*/
