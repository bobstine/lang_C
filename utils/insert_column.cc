/* -*-c++-*-

   MAKE:     make insert_column

   EXECUTE:  insert_column -r correlation -j index -o output.dat -f input.dat


   Input
   
   The format of the input should be that produced by the csv_parser:
   a sequence of variables written as name followed on the next line
   by data. The first line of the file must give the number of
   observations (all numerical) and the number of variables in the
   file (again, as produced by csv_parser).

   
   Output

   Echos the input file except that it builds a column that has
   correlation (on average) r with the first column of the input file
   (which is Y). It writes this column as the ith explanatory variable
   in the output file.
   

   26 Feb 09 ... Created for testing auction code in simulations.

*/

#include <cassert>

#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <cstdio>

#include <vector>
#include <set>

#include <iterator>
#include <algorithm>
#include <numeric>
#include <getopt.h>

#include <boost/spirit/core.hpp>
#include <boost/spirit/utility/confix.hpp>
#include <boost/spirit/utility/lists.hpp>
#include <boost/spirit/utility/escape_char.hpp>
#include <boost/lambda/lambda.hpp>

///////////////////////////////////////////////////////////////////////////////


void
write_numerical_column  (std::vector<std::string> const& varNames, StringDataMatrix const& data,
			  int column, int numberMissing, std::ostream& output)
{
  // start by writing the undecorated name to output
  output << varNames[column] << endl;
  // now write the observed data, with any missing value inserted
  int nObs (data.size());
  if (0 == numberMissing)                                // write directly to output
    for(int i=0; i<nObs; ++i)
      output << data[i][column] << " ";
  else {                                                 // insert mean value in missing locations
    double sum (0.0);
    std::vector< int  > missingPos;
    for(int i=0; i<nObs; ++i)
    { if (data[i][column].size() > 0)                    // row i is not missing, so include in the sum
	sum += parse_double(data[i][column]);
      else
	missingPos.push_back(i);
    } 
    assert((numberMissing == (int)missingPos.size()));   // verify number missing found here match those found during scanning
    missingPos.push_back(nObs);                          // sentinel at the end
    double mean (sum/(nObs-numberMissing));
    for(int i=0, j=0; i<nObs; ++i) {
      if(missingPos[j]>i)
	output << data[i][column] << " ";
      else
      {	output << mean << " ";
	++j;
      }
    }
  }
  output << endl;
}

void
write_numerical_data_file (std::vector<std::string> const& varNames, StringDataMatrix const& data,
			   std::vector<int> const& varMissingCount, std::vector<int> const& varNumericCount,
			   std::ostream& output)
// writes columns in streaming fashion
{
  int nVars (varNames.size());
  int nObs  (data.size());

  output << nObs << " " << nVars << endl;
  for (int column=0; column<nVars; ++column)
  { if((varNumericCount[column]+varMissingCount[column])==nObs) // numerical column with possible missing
    { 
      write_numerical_column (varNames, data, column, varMissingCount[column], output);
    } else
    {
      write_categorical_column (varNames, data, column, output);
    }
    if (varMissingCount[column] > 0)
    {
      write_missing_column (varNames, data, column, output);
    }
  }
}


// return number of obs, number of vars written
std::pair<int, int>
insert_column(double corr, int index, std::istream& input, std::ostream& output)
{
  // read nRows and nCols from input source
  int nRows, nCols;

  input >> nRows >> nCols;
  std::clog << "TEST: read nRows = " << nRows << " and nCols " << nCols << std::endl;
  
  // read from input into this string and numbers into this vector
  std::string inputLine;
  std::vector< double > data (nRows);
  std::vector< double > noise (nRows);
  
  // read the response
  std::string responseName;
  getline(input, responseName);
  std::cout << "INSC: Response variable is " << responseName << std::endl;
  getline(input, inputLine);
  std::stringstream ss(inputLine);
  for(int i=0; i<nRows; ++i) 
  double sd = standard_deviation(make_range(

					    b = sqrt((1-rho^2)/
{
    if (parse_variable_names(inputLine.c_str(), StringCatcher( &inputColumnNames ) ))
    { std::clog <<  "\nParser: Read " << inputColumnNames.size() << " variable names from the input data.  These are:\n" << endl;
      for (std::vector<std::string>::iterator it = inputColumnNames.begin(); it != inputColumnNames.end(); ++it)
	std::clog << " |" << *it << "| " << endl;
    }
    else std::clog<< "Parser: Failed to parse CSV variable names.\n" << endl;
  } else std::clog << "Parser: Not able to read input data.\n " << endl;
  
  // set up vectors to count types of data in columns
  int nVars (inputColumnNames.size());
  std::vector< int > numeric (nVars);
  std::vector< int > missing (nVars);
  for (int i=0; i<nVars; ++i)
  { numeric.push_back(0);
    missing.push_back(0);
  }
  
  // iterate through remaining lines of data in order to build the matrix of strings
  StringDataMatrix dataMatrix;

  int lineNumber (0);
  while (getline(input, inputLine))
  { ++lineNumber;
    std::vector<std::string> inputData;
    if( parse_data_line(inputLine.c_str(), StringCatcher( &inputData ))) {
      // cout <<  "\nData from the item parser:" << endl;
      // for (std::vector<std::string>::iterator it = inputData.begin(); it != inputData.end(); ++it)  {
      // cout << " |" << *it << "| " << endl; 
      if ((int)inputData.size() == nVars)
      { dataMatrix.push_back(inputData);
	for(int i=0; i<nVars; ++i)
	{ if(inputData[i].size() == 0)              ++missing[i];
	  else if (can_parse_number(inputData[i]))  ++numeric[i];
	}
      }
      else
      { std::cerr
	  << "Parser: Error. Line " << lineNumber
	  << ". Number of data elements (" << inputData.size()
	  << ") unequal to number of variables (" << nVars << ").\n\t" << inputLine << endl;
      }
    }
    else std::cerr << "Parser: Error. Line " << lineNumber << ". Failed to parse input CSV data.\n\t" << inputLine << endl;
  }
  
  std::clog << "Parser:  nObs = " << dataMatrix.size() << " with nVars = " << nVars << endl;
  std::clog << "Parser: #Numeric: " ;
  for (int i = 0; i<nVars; ++i)
    std::clog << numeric[i] << " ";
  std::clog << "\nParser: #Missing: " ;
  for (int i = 0; i<nVars; ++i)
    std::clog << missing[i] << " ";
  std::clog << endl;
    
  write_numerical_data_file (inputColumnNames, dataMatrix, missing, numeric, output);
  
  return std::make_pair(dataMatrix.size(), nVars);
}

////////////////////////////////////////////////////////////////////////////////


void
parse_arguments(int argc, char** argv, double &corr, int &index, std::string& inputFile, std::string& outputFile );


int
main (int argc, char** argv)
{
  //  set default parameter values
  std::string inputFileName     ("");
  std::string outputFileName    ("");
  int index = 1;
  double  = 0.0;
  
  // parse arguments from command line
  parse_arguments(argc, argv, corr, index, inputFileName, outputFileName);
  std::clog << "INSC: Arguments   --corr=" << corr << " --index=" << index 
	    << " --input-file=" << inputFileName << " --output-file=" << outputFileName
	    << std::endl;

  // 4 call variations
  if (inputFileName.size() == 0)
  { if (outputFileName.size() == 0)
      insert_column(corr, index, std::cin, std::cout);                // A
    else
    { std::ofstream output (outputFileName.c_str());
      if (!output)
      { std::cerr << "CSVP: Error. Cannot open output file " << outputFileName << std::endl;
	return 1;
      }
      csv_parser(std::cin, output);                  // B
    }
  }
  else
  { std::ifstream input (inputFileName.c_str());
    if (!input)
    { std::cerr << "CSVP: Error. Cannot open input file " << inputFileName << std::endl;
      return 2;
    }
    if (outputFileName.size() == 0)
      csv_parser(input, std::cout);                   // C
    else
    { std::ofstream output (outputFileName.c_str());
      if (!output)
      { std::cerr << "CSVP: Error. Cannot open output file " << outputFileName << std::endl;
	return 3;
      }
      csv_parser(input, output);                      // D
    }
  }
  return 0;
}



void
parse_arguments(int argc, char** argv, double &corr, int &index, std::string& inputFile, std::string& outputFile)
{
  int key;
  while (1)                                  // read until empty key causes break
    {
      int option_index = 0;
      static struct option long_options[] = {
	  {"input-file",        1, 0, 'f'},  // has arg,
	  {"output-file",       1, 0, 'o'},  // has arg,
	  {"corr",              1, 0, 'r'},  // has arg,
	  {"index",             1, 0, 'j'},  // has arg,
	  {"help",              0, 0, 'h'},  // no  arg, 
	  {0, 0, 0, 0}                       // terminator 
	};
	key = getopt_long (argc, argv, "f:o:r:j:h", long_options, &option_index);
	if (key == -1)
	  break;
	//	std::cout << "Option key " << char(key) << " with option_index " << option_index << std::endl;
	switch (key)
	  {
	  case 'f' :                                    
	    {
	      std::string name(optarg);
	      inputFile = name;
	      break;
	    }
	  case 'o' :  
	    {
	      std::string name(optarg);
	      outputFile = optarg;
	      break;
	    }
          case 'r' :
	    {
	      std::istringstream is(optarg);
	      is >> corr;
	      break;
	    }
          case 'j' :
	    {
	      std::istringstream is(optarg);
	      is >> index;
	      break;
	    }
	  case 'h' :
	    {
	      std::cout << "switches:" << std::endl << std::endl;
	      std::cout << "      --input-file=foo       input file" << std::endl;
	      std::cout << "      -ifoo" << std::endl << std::endl;
	      std::cout << "      --output-file=out      output file" << std::endl;
	      std::cout << "      -oout" << std::endl << std::endl;
	      std::cout << "      --corr=#      correlation with response" << std::endl;
	      std::cout << "      -r#" << std::endl << std::endl;
	      std::cout << "      --index=#     position in output" << std::endl;
	      std::cout << "      -j#" << std::endl << std::endl;
	      std::cout << "      --help      generates this message" << std::endl;
	      std::cout << "      -h" << std::endl << std::endl;
	      exit(0);
	      break;
	    }
	  }
    }
}
