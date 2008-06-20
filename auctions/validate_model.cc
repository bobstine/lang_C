// $Id: validate_model.cc,v 3.1 2008/01/16 22:51:38 bob Exp $

/*
  Run as:

     validator -f dataFile -n nameFile -m modelfile -s over_sampling_ratio_of_odds -o output

  The model is read from the model file, and the name file supplies
  the names of the data which is read from stdin/data into features.
  The data file must begin with the number of rows that will follow.
  The output includes the costs associated with several cost ratios, 
  as defined in the vector rho. (These presume the predictions are
  calibrated.)  If the model file includes a calibrator, this version
  assumes that the calibrator is one of the variables in the model.

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
	       std::string const& modelFileName, std::string const& outputFileName,
	       std::string const& fitsFileName, double odds);

void
parse_arguments(int argc, char** argv, std::string* nameFileName, std::string* dataFileName,
		std::string* modelFileName, std::string* outputFileName,
		std::string* fitsFileName, double *odds);


//  main  main  main  main  main  main  main  main  main  main  main  main  main  main  main  

int
main(int argc, char** argv)
{
  // set default arguments, parse for options
  std::string dataFileName   ("stdin");
  std::string modelFileName  ("test/auction.model");
  std::string outputFileName ("test/auction.model.val");
  std::string fitsFileName ("");
  std::string nameFileName   ("test/validation.names");
  double      odds           (1.0);
  parse_arguments(argc, argv, &nameFileName, &dataFileName, &modelFileName,
		  &outputFileName, &fitsFileName, &odds);
  std::cout << "\n\nVALD: Validate with names from " << nameFileName
	    << ", data from " << dataFileName
	    << " model from " << modelFileName
	    << ".\n        Results written to " << outputFileName;
  if(fitsFileName == "")
    std::cout << std::endl;
  else
    std::cout << " and fits written to " << fitsFileName << std::endl;
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
    run_validation(nameFileName, dataFileName, modelFileName, outputFileName, fitsFileName, odds);
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

double
area_under_ROC(const std::vector< std::pair<int,int> > & errors, int n)
{
  std::vector<std::pair<double,double> > roc;
  roc.push_back(std::make_pair(0,0));
  for(std::vector< std::pair<int,int> >::const_iterator i = errors.begin(); i != errors.end();++i)
    roc.push_back(std::make_pair(double(i->first)/n,1 - double(i->second)/n));
  roc.push_back(std::make_pair(1,1));

#ifndef NDEBUG
  std::cout << "ROC CURVE: (" << "n = " << n << ")" << std::endl;
  for(std::vector< std::pair<double,double> >::const_iterator i = roc.begin(); i != roc.end();++i)
    std::cout << "\t" << i->first << " , " << i->second << std::endl;
#endif
  
  double result = 0;
  double previous_x = 0;
  double previous_y = 0;
  for(std::vector< std::pair<double,double> >::const_iterator i = roc.begin(); i != roc.end();++i)
    {
      double x = i->first;
      double y = i->second;
      result += (previous_y + y)/2 * (x - previous_x);
      previous_x = x;
      previous_y = y;
    }
  return result;
}

void
run_validation(std::string const& nameFile, std::string const& dataFileName,
	       std::string const& modelFileName, std::string const& outputFileName,
	       std::string const & fitsFileName, double odds)
{
  // set up vector of pairs for costs of type 1, type 2 errors
  std::vector<double> rho;
  rho.push_back(1./199.);
  rho.push_back(1./99.);
  rho.push_back(1./49.);
  rho.push_back(1./19.);
  rho.push_back(1./9.);
  rho.push_back(1./6.);
  rho.push_back(1./4.);
  rho.push_back(1./3.);
  rho.push_back(1./2.);
  rho.push_back(1);
  rho.push_back(2);
  rho.push_back(3);
  rho.push_back(4);
  rho.push_back(6);
  rho.push_back(9);
  rho.push_back(19);
  rho.push_back(49);
  rho.push_back(99);
  rho.push_back(199);
  std::vector< std::pair<int,int> > errors(rho.size(),std::make_pair(0,0));

  // limit number of rows read in at a time to avoid using too much memory
  const int maxNumberRows (250000);
  std::cout << "VALD: Begin validation of model " << modelFileName
	    << " using names " << nameFile << "  data from " << dataFileName <<  " odds = " << odds << std::endl;
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
  int rows_remaining = nRows;
  while(rows_remaining > 0)
  { int n ((rows_remaining > maxNumberRows) ? maxNumberRows : rows_remaining);
    rows_remaining -= n;
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
    range_ops::transform(make_range(fit, fit+n), yColumn.range(), res, Function_Utils::AXPY(-1.0)   );
    if(fitsFileName != "")
      { // write the fits out to fitsFileName
	std::ofstream outputStream (fitsFileName.c_str());
	std::copy(fit,fit+n,std::ostream_iterator<double>(outputStream,"\n"));
      }
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
    if (rows_remaining > 0)
      std::cout << "VALD: With " << rows_remaining << " remaining rows, SSE = " << sse << "   LL = " << logLike << std::endl;
  }
  // write cumulative to output
  std::ofstream outputStream (outputFileName.c_str());
  outputStream << "Validation of " << modelFileName << " using " << numberValidationRows << " cases. " << std::endl
	       << "SSE = " << sse << " LL = " << logLike << std::endl;
  outputStream << "Area under ROC curve is approximately " << area_under_ROC(errors,nRows) << std::endl;
  for (unsigned int k=0; k<rho.size(); ++k)
    {
      if(rho[k] >= 1.0)
	outputStream << rho[k] << "    "
		     << errors[k].first << " " << errors[k].second << " "
		     << errors[k].first + rho[k] * errors[k].second << std::endl;
      else
	outputStream << "1/" << 1/rho[k] << "    "
		     << errors[k].first << " " << errors[k].second << " "
		     << errors[k].first + rho[k] * errors[k].second << std::endl;
	
    }
      
  /*
    double *y (begin(yColumn.range()));
    for (int i=0; i<n; ++i)
    std::cout << y[i] << " " << fit[i] << std::endl;
  */
}


//  parse_arguments  parse_arguments  parse_arguments  parse_arguments  parse_arguments  parse_arguments

void
parse_arguments(int argc, char** argv, std::string* nameFileName, std::string* dataFileName,
		std::string* modelFileName, std::string* outputFileName,
		std::string* fitsFileName, double *odds)
{
  for (int i=1; i<argc; i=i+2)
  {
    char key(argv[i][0]);
    if ('-' == key)
    { 
      switch (argv[i][1])  // first letter after -
      {
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
      case 'F' :
	{ std::string name(argv[i+1]);
	  *fitsFileName = name;
	  break;
	}
      case 's' :
	{ std::stringstream input(argv[i+1]);
	  input >> *odds;
	  break;
	}
      case 'h' :
	{ 
	  std::cout << argv[0] << "-f file_name" << std::endl;
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
  
