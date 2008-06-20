// $Id: validate_model.cc,v 1.14 2004/08/25 22:14:40 bob Exp $

/*
  Run as:

     validator -f dataFile -n nameFile -m modelfile -c calibrate -s over_sampling_ratio_of_odds -o output

  The model is read from the model file, and the name file supplies
  the names of the data which is read from stdin/data into features.
  The data file must begin with the number of rows that will follow.
  The output includes the costs associated with several cost ratios,
  as defined in the vector rho. (These presume the predictions are
  calibrated.)  If the model file includes a calibrator, you need to
  set the -c flag to a nonzero value.

  The over-sampling ratio is the ratio of odds of a one in the
  validation and estimation samples.
  
            n_1/n_0    in validation
   s =      -------
            n_1/n_0    in estimation

  For rare things like bankruptcy, this ratio will be quite
  small, such as .001.

  23 May 04 ... Weights for comparison to C4.5.
  27 Apr 04 ... Use row-oriented data file as input.
  22 Apr 04 ... Adjustments for over-sampling.
   5 Apr 04 ... Created as a filter to validate the results of a model.
*/

#include "auction.h"
#include "column.h"
#include "log_regr.h"
#include "range.h"
#include "range_ops.h"

#include <cstdio>
#include <iostream>
#include <fstream>
#include <math.h>

void
run_validation(std::string const& nameFile, std::string const& dataFileName,
	       std::string const& modelFileName, std::string const& outputFileName, bool calibrate, double odds);

void
parse_arguments(int argc, char** argv, std::string* nameFileName, std::string* dataFileName,
		std::string* modelFileName, std::string* outputFileName, bool* calibrate, double *odds);


//  main  main  main  main  main  main  main  main  main  main  main  main  main  main  main  

int
main(int argc, char** argv)
{
  // set default arguments, parse for options
  std::string dataFileName   ("stdin");
  std::string modelFileName  ("test/auction.model");
  std::string outputFileName ("test/auction.model.val");
  std::string nameFileName   ("test/validation.names");
  bool        calibrate      (false);
  double      odds           (1.0);
  parse_arguments(argc, argv, &nameFileName, &dataFileName, &modelFileName, &outputFileName, &calibrate, &odds);
  std::cout << "\n\nVALD: Validate with names from " << nameFileName << ", data from " << dataFileName
	    << " model from " << modelFileName << " with calibration "
	    << calibrate <<  ".\n      Results written to " << outputFileName << std::endl;
  // check for specified file streams
  bool haveDataFile (true);
  if (dataFileName != "stdin")
  { std::ifstream dataStream (dataFileName.c_str());
    if (not dataStream) haveDataFile = false;
    dataStream.close();
  }
  std::ifstream modelStream (modelFileName.c_str());
  std::ifstream nameStream  ( nameFileName.c_str());
  if (haveDataFile && modelStream &&  nameStream)
  { 
    nameStream.close();
    modelStream.close();
    run_validation(nameFileName, dataFileName, modelFileName, outputFileName, calibrate, odds);
    return 0;
  }
  else
  { std::cout << "TEST: Could not open input files.\n";
    return 1;
  }
}

// run_validation  run_validation  run_validation  run_validation  run_validation  run_validation

namespace{

  class Type1ErrorCounter : public std::binary_function<double,double,int>  // bother innocent
  {
    double mThreshold;

  public:
    Type1ErrorCounter (double rho)
      : mThreshold(1/(1.0 + rho)) {}
    
    int operator()(double y, double yhat) const
    { 
      if (y == 0)
	if (yhat < mThreshold)
	  return 0;
	else
	  return 1;
      else return 0;
    }
  };
  
  
  class Type2ErrorCounter : public std::binary_function<double,double,int> // miss bankrupt
  {
    double mThreshold;

  public:
    Type2ErrorCounter (double rho)
      : mThreshold(1/(1.0 + rho)) {}
    
    int operator()(double y, double yhat) const
    { 
      if (y == 1)
	if (yhat > mThreshold)
	  return 0;
	else
	  return 1;
      else return 0;
    }
  };

}

void
run_validation(std::string const& nameFile, std::string const& dataFileName,
	       std::string const& modelFileName, std::string const& outputFileName, bool calibrate, double odds)
{
  // set up vector of pairs for costs of type 1, type 2 errors
  const int numberOfCosts (9);
  std::vector<double> rho (numberOfCosts);
  rho[0] =  1; rho[1] =  2; rho[2] =  4; rho[3] = 6; rho[4] = 9;
  rho[5] = 19; rho[6] = 49; rho[7] = 99; rho[8] = 199;
  std::vector< std::pair<int,int> > errors;
  for (int i=0; i<numberOfCosts; ++i)
    errors.push_back( std::make_pair(0,0) );
  // limit number of rows read in at a time to avoid using too much memory
  const int maxNumberRows (250000);
  std::cout << "VALD: Begin validation of model " << modelFileName
	    << " using names " << nameFile << "  data from " << dataFileName
	    << ".\n      Calibrate = " << calibrate << " odds = " << odds << std::endl;
  int nRows (0);
  FILE *input;
  if (dataFileName == "stdin")
    input = stdin;
  else
    input = fopen(dataFileName.c_str(),"r");
  fscanf(input, "%d", &nRows);
  int numberValidationRows (nRows);
  std::cout << "VALD: Expecting to read a total of " << nRows << " rows for validation.\n";
  // build vector of columns and convert X's to feature vector (check size)
  double logLike (0.0);
  double sse     (0.0);
  while(nRows > 0)
  { int n ((nRows > maxNumberRows) ? maxNumberRows : nRows);
    nRows -= n;
    std::vector<Column> columns;
    int nColsRead (insert_columns_from_stream(input, nameFile, n, std::back_inserter(columns)));
    if (columns.size() == 0) return;
    // y is assumed in first column
    Column yColumn (columns[0]);
    std::cout << "VALD: Data file " << dataFileName << " produced " << nColsRead << " columns with (e-b) from range "
	      << end(yColumn.range())-begin(yColumn.range()) << std::endl;
    std::cout << "      Response has dummy property " << yColumn.is_dummy()
	      << "   " << yColumn << std::endl;
    // initialize factory and extract initial column features
    FeatureFactory factory(columns);
    std::vector<FeatureABC*> features = factory.features("ColumnFeature");
    std::cout << "VALD: Converted columns into " << features.size() << " features.\n";
    // read model parameters
    std::ifstream modelStream (modelFileName.c_str());
    std::string temp;
    std::getline(modelStream, temp); std::cout << "VALD: Model file header is " << temp << std::endl;  // name line
    int fitN, q;
    modelStream >> fitN >> q;
    std::vector<double> beta(q+1);
    for(int j=0; j<=q; ++j)
      modelStream >> beta[j];
    std::cout << "VALD: Read beta as " << make_range(beta) << std::endl; 
    // adjust intercept for over-sampling
    beta[0] += log(odds);
    // extract calibration function
    SmoothingSplineOperator splineOp;
    if (calibrate)
    { std::getline(modelStream, temp);
      std::getline(modelStream, temp);
      std::cout << "VALD: Found calibration operator... " << temp << std::endl;
      splineOp = SmoothingSplineOperator(modelStream);
    }
    // find number of validation rows and build the features used in the model
    std::vector<FeatureABC*> xFeatures;
    factory.append_features_from_stream(modelStream, features, std::back_inserter(xFeatures));
    modelStream.close();
    // build xb component of fit 
    double xb[n];
    for (int i=0; i<n; ++i)
      xb[i] = beta[0];
    for (int j=0; j<q; ++j)
    { double b  (beta[j+1]);
      range_traits<FeatureABC::Range>::const_iterator x (begin(xFeatures[j]->range()));
      for (int i=0; i<n; ++i, ++x)
	xb[i] += b * *x;
    }
    // accumulate sse, log like  (see code in log_regr.cc)
    double fit[n], res[n];
    range_ops::transform(make_range(xb, xb+n)  ,                  fit, Function_Utils::LogisticNeg());
    if (calibrate)  // alter xb as well for log-like calc
    {
      for (int i=0; i<3; ++i)
      { std::cout << "Calibrating fit " << fit[i] << " -> " << splineOp(fit[i]) << " and xb from "
		  << xb[i] << " to " << Function_Utils::Logit()((splineOp(fit[i]))) << std::endl;
      }
      range_ops::transform(make_range(fit, fit+n), fit, splineOp);
      range_ops::transform(make_range(fit, fit+n), xb , Function_Utils::Logit());
    }
    range_ops::transform(make_range(fit, fit+n), yColumn.range(), res, Function_Utils::AXPY(-1.0)   );
    logLike += range_ops::accumulate(make_binary_range
				     (Function_Utils::LogisticLikeTerm(), yColumn.range(), make_range(xb,xb+n)), 0.0);
    sse     += range_ops::accumulate(make_unary_range
				     (Function_Utils::Square()           , make_range(res, res+n)), 0.0);
    for (unsigned int k=0; k<rho.size(); ++k)
    {
      errors[k].first  += range_ops::accumulate(make_binary_range
						(Type1ErrorCounter(rho[k]), yColumn.range(), make_range(fit,fit+n)), 0);
      errors[k].second += range_ops::accumulate(make_binary_range
						(Type2ErrorCounter(rho[k]), yColumn.range(), make_range(fit,fit+n)), 0);
    }
    if (nRows > 0)
      std::cout << "VALD: With " << nRows << " remaining rows, SSE = " << sse << "   LL = " << logLike << std::endl;
  }
  // write cumulative to output
  std::ofstream outputStream (outputFileName.c_str());
  outputStream << "Validation of " << modelFileName << " using " << numberValidationRows << " cases. " << std::endl
	       << "SSE = " << sse << " LL = " << logLike << std::endl;
  for (unsigned int k=0; k<rho.size(); ++k)
    outputStream << rho[k] << "    "
		 << errors[k].first << " " << errors[k].second << " "
		 << errors[k].first + rho[k] * errors[k].second << std::endl;
      
  /*
    double *y (begin(yColumn.range()));
    for (int i=0; i<n; ++i)
    std::cout << y[i] << " " << fit[i] << std::endl;
  */
}


//  parse_arguments  parse_arguments  parse_arguments  parse_arguments  parse_arguments  parse_arguments

void
parse_arguments(int argc, char** argv, std::string* nameFileName, std::string* dataFileName,
		std::string* modelFileName, std::string* outputFileName, bool *calibrate, double *odds)
{
  for (int i=1; i<argc; i=i+2)
  {
    char key(argv[i][0]);
    if ('-' == key)
    { 
      switch (argv[i][1])  // first letter after -
      {
      case 'c' :
	{ int c;
	  std::istringstream is(argv[i+1]);
	  is >> c;
	  *calibrate = (c) ? true:false; 
	  break;
	}
      case 'f' :
	{ std::string name(argv[i+1]);
	  *dataFileName = name;
	  break;
	}
      case 'm' :   
	{ std::string name(argv[i+1]);
	  *modelFileName = name;
	  break;
	}
      case 'n' :
	{ std::string name(argv[i+1]);
	  *nameFileName = name;
	  break;
	}
      case 'o' :
	{ std::string name(argv[i+1]);
	  *outputFileName = name;
	  break;
	}
      case 's' :
	{ std::stringstream input(argv[i+1]);
	  input >> *odds;
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
  
