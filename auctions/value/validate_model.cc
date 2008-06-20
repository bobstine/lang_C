// $Id: validate_model.cc,v 3.0 2004/11/19 18:58:36 foster Exp $
/*

  Run as:

   validate_model -m modelfile -d datafile

   5 Apr 04 ... Created as a filter to validate the results of a model.
*/

#include "auction.h"
#include "column.h"
#include "log_regr.h"
#include "range.h"
#include "range_ops.h"

#include <iostream>
#include <fstream>
#include <math.h>

void
run_validation(std::string const& dataFileName, std::string const& modelFileName);

void
parse_arguments(int argc, char** argv, std::string* dataFileName, std::string* modelFileName);


//  main  main  main  main  main  main  main  main  main  main  main  main  main  main  main  

int
main(int argc, char** argv)
{
  // set default arguments, parse for options
  std::string modelFileName ("test/auction.model");
  std::string dataFileName  ("test/validation.dat");
  parse_arguments(argc, argv, &dataFileName, &modelFileName);
  std::cout << "TEST: Reading data from " << dataFileName << " and model from " << modelFileName << std::endl;
  // open file streams
  std::ifstream modelStream (modelFileName.c_str());
  std::ifstream dataStream  (dataFileName.c_str());
  if (modelStream && dataStream)
  { dataStream.close();
    modelStream.close();
    run_validation(dataFileName, modelFileName);
    return 0;
  }
  else
  { std::cout << "Could not open input files.\n";
    return 1;
  }
}

// run_validation  run_validation  run_validation  run_validation  run_validation  run_validation

void
run_validation(std::string const& dataFileName, std::string const& modelFileName)
{
  // build vector of columns and convert X's to feature vector
  std::vector<Column> yColumns;
  std::vector<Column> xColumns;
  insert_columns_from_file(dataFileName, 1, back_inserter(yColumns), back_inserter(xColumns));
  int n (yColumns[0].size());
  std::cout << "TEST: Data file " << dataFileName << " produced " << xColumns.size()<< " X features with n = " << n << std::endl;
  FeatureVector features(xColumns);
  // read model parameters
  std::ifstream modelStream (modelFileName.c_str());
  std::string temp;
  std::getline(modelStream, temp);  std::cout << "TEST: Model file header is " << temp << std::endl;  // name line
  int fitN, q;
  modelStream >> fitN >> q;
  std::vector<double> beta(q+1);
  double se;
  for(int j=0; j<=q; ++j)
    modelStream >> beta[j] >> se;
  // get the features used in the model
  FeatureVector xFeatures(modelStream, features);
  // build fit
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
  range_ops::transform(make_range(xb, xb+n)  , fit                     , Function_Utils::LogisticNeg());
  range_ops::transform(make_range(fit, fit+n), yColumns[0].range(), res, Function_Utils::AXPY(-1.0));
  double logLike = range_ops::accumulate (make_binary_range(Function_Utils::LogisticLikeTerm(), yColumns[0].range()    , make_range(xb,xb+n)), 0.0);
  double sse     = range_ops::accumulate (make_unary_range(Function_Utils::Square()           , make_range(res, res+n)), 0.0);
					  
  // write cumumlative and errors to stdout
  std::cout << "SSE = " << sse << " LL = " << logLike << std::endl;
  double *y (begin(yColumns[0].range()));
  for (int i=0; i<n; ++i)
    std::cout << y[i] << " " << fit[i] << std::endl;
}


//  parse_arguments  parse_arguments  parse_arguments  parse_arguments  parse_arguments  parse_arguments

void
parse_arguments(int argc, char** argv, std::string* dataFileName, std::string* modelFileName)
{
  for (int i=1; i<argc; i=i+2)
  {
    char key(argv[i][0]);
    if ('-' == key)
    { 
      switch (argv[i][1])  // first letter after -
      {
      case 'm' :   
	{ std::string name(argv[i+1]);
	  *modelFileName = name;
	  break;
	}
      case 'd' :                                
	{ std::string name(argv[i+1]);
	  *dataFileName = name;
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
  
