// $Id: estimate_model.cc,v 1.5 2004/08/25 22:14:40 bob Exp $

/*
  Run as:

     estimator -f dataFile -n namesFile -m old_model_file -o new_model_file -c df

  This program refits a model (such as one chosen by the auction) to a
  new, presumably larger data file, formated in row-based order (one
  obs on a row rather than column order as in a sequential search) and
  prefixed with a row count on the first line.  The data is read from
  stdin, prefixed with the number of rows to be read and the names of
  the columns given in a separate file. (Suited for CV.)

  Use the -c option to indicate that a calibration function should be
  applied to the predictions from the model, using df degrees of freedom.

  The new model (which has the same predictors and will or will not be
  calibrated) then gets written to the output file.  The predictors
  are the same, but the coefs and parameters of the calibrator may change.
	    
  26 Apr 04 ... Created to re-estimate a model using a possibly larger data set.
*/

#include "auction.h"
#include "column.h"
#include "log_regr.h"
#include "range.h"
#include "range_ops.h"
#include "print_utils.h"

#include <cstdio>
#include <iostream>
#include <fstream>
#include <math.h>

void
estimate_model(std::string const& nameFileName, std::string const& dataFileName,
	       std::string const& modelFileName, std::string const& outputModelfile, int df);

void
parse_arguments(int argc, char** argv, std::string* nameFileName, std::string* dataFileName,
		std::string* modelFileName, std::string* outputFileName, int* df);


//  main  main  main  main  main  main  main  main  main  main  main  main  main  main  main  

int
main(int argc, char** argv)
{
  // set default arguments, parse for options
  std::string dataFileName     ("stdin");
  std::string modelFileName    ("test/auction.model");
  std::string newModelFileName ("test/auction.new_model");
  std::string nameFileName     ("test/estimation.names");
  int         df               (0);
  parse_arguments(argc, argv, &nameFileName, &dataFileName, &modelFileName, &newModelFileName,&df);
  std::cout << "\n\nESTM: Estimate model in " << modelFileName << " using data from " << dataFileName
	    << " with names in " << nameFileName <<  ".\n       New model will be written to " << newModelFileName;
  bool calibrate (df > 0);
  if (calibrate)
    std::cout << " with " << df << "df calibration.\n";
  else
    std::cout << " with no calibration.\n";
  // check for specified file streams
  bool haveDataFile (true);
  if (dataFileName != "stdin")
  { std::ifstream dataStream (dataFileName.c_str());
    if (not dataStream) haveDataFile = false;
    dataStream.close();
  }
  std::ifstream nameStream      (nameFileName.c_str());
  std::ifstream modelStream     (modelFileName.c_str());
  std::ofstream estimatedStream (newModelFileName.c_str());
  if (haveDataFile && nameStream && modelStream && estimatedStream)
  { modelStream.close();
    nameStream.close();
    estimatedStream.close();
    estimate_model(nameFileName, dataFileName, modelFileName, newModelFileName, df);
    return 0;
  }
  else
  { std::cout << "Could not open input files.\n";
    return 1;
  }
}

// estimate_model  estimate_model  estimate_model  estimate_model  estimate_model  estimate_model  

void
estimate_model(std::string const& nameFileName, std::string const& dataFileName,
	       std::string const& modelFileName, std::string const& newModelFileName, int df)
{
  int nRows (0), nColRead(0);
  FILE *input;
  if (dataFileName == "stdin")
    input = stdin;
  else
    input = fopen(dataFileName.c_str(),"r");
  std::vector<Column> columns;
  // read number of rows from file
  fscanf(input, "%d", &nRows);
  std::cout << "ESTM: Estimating model with data from " << dataFileName << ", expecting " << nRows << " rows.\n";
  // build vector of columns and convert X's to feature vector
  nColRead = insert_columns_from_stream(input, nameFileName, nRows, std::back_inserter(columns));
  std::cout << "ESTM: Got " << nColRead << " columns with " << nRows << " rows.\n";
  // y is assumed in first column
  Column yColumn (columns[0]);
  std::cout << "ESTM: Name file " << nameFileName << " produced " << columns.size()<< " columns.\n";
  std::cout << "      Response has dummy property " << yColumn.is_dummy()
	    << "   " << yColumn << std::endl;
  // initialize the model
  LogisticRegression regr(yColumn.name(), make_anonymous_range(yColumn.range()), nRows);
  std::cout << "ESTM: Initial Model... \n" << regr << std::endl;
  // initialize feature factory
  FeatureFactory factory(columns);
  std::vector<FeatureABC*> features = factory.features("ColumnFeature");
  std::cout << "ESTM: Converted columns to " << features.size() << " features.\n";

  // read model parameters from file, skipping over estimates
  std::ifstream modelStream (modelFileName.c_str());
  std::string temp;
  std::getline(modelStream, temp);  std::cout << "ESTM: Model file header is " << temp << std::endl;  // name line
  int fitN, q;
  modelStream >> fitN >> q;
  std::cout << "ESTM: Building model for q = " << q << " and " << df << " df.\n";
  // read coefs from file (though will reestimate and not use these)
  std::vector<double> beta(q+1);
  for(int j=0; j<=q; ++j)
    modelStream >> beta[j];
  std::cout << "ESTM: Read beta as " << make_range(beta) << std::endl;
  // peel off calibrator from file (not instantiated here)
  if (df > 0) 
  { std::getline(modelStream,temp);    std::getline(modelStream,temp);  // start next line and read header line
    std::cout << "ESTM: Found calibrator... " << temp << std::endl;
    std::getline(modelStream,temp);    std::getline(modelStream,temp);  // skip over coefs and knots
  }
  // read features from file, and force into model
  std::vector<FeatureABC*> xFeatures;
  factory.append_features_from_stream(modelStream, features, std::back_inserter(xFeatures));
  modelStream.close();
  for (std::vector<FeatureABC*>::const_iterator it = xFeatures.begin(); it != xFeatures.end(); ++it)
  { std::pair<double,double>
      result (regr.add_predictor( (*it)->name(), (*it)->range(), (*it)->center(), (*it)->scale(), 1.0));
    std::cout << "      " << result << std::endl;
  }
  // add new calibrator for new fit
  if (df > 0) regr.add_calibrator(df);    
  // print model and write to file
  std::cout << regr << std::endl;
  std::ofstream output (newModelFileName.c_str());
  output << "Re-estimated model: ";
  regr.write_without_calibration_to(output);
  if (df > 0)
    regr.write_calibrator_to(output);
  for (unsigned int j=0; j<xFeatures.size(); ++j)
    xFeatures[j]->write_to(output);
  output.close();
}

//  parse_arguments  parse_arguments  parse_arguments  parse_arguments  parse_arguments  parse_arguments

void
parse_arguments(int argc, char** argv, std::string* nameFileName, std::string* dataFileName,
		std::string* modelFileName, std::string* newFileName, int* df)
{
  for (int i=1; i<argc; i=i+2)
  {
    char key(argv[i][0]);
    if ('-' == key)
    { 
      switch (argv[i][1])  // first letter after -
      {
      case 'c' :
	{ std::istringstream is(argv[i+1]);
	  is >> *df;
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
	  *newFileName = name;
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
  
