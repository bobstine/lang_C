/* -*-c++-*-

   MAKE:  make -d csv_parser  # also runs with input from test-data.csv

   EXECUTE:  csv_parser -o output-file-name < input.csv
   
   
   This program uses boost's BNF spirit engine to parse the information in a csv
   file.  The program converts the columns in that file into *numerical*
   columns. Categorical data is converted into indicators, and numerical columns
   that have missing data are expanded into a mean-filled column along with an
   indicator that shows which of the columns were missing. Naming conventions
   denote the columns so that you can recognize the indicators and such and
   perhaps process them as a block.  Bracketed terms in the names of output
   variables identify variables generated during processing (e.g., V1[missing]
   or Location[SC].


   Input
   
   The input data should have the style of named columns. Names can consist of
   characters, numbers, and the symbols ".", "_", and "/".  Others might ought
   to be added to aid in later parsing of variables. For example, an input csv
   file with 3 variables might begin as in

      Var1, a/b, Var.3
       1, 2, 3
       3, 4, 5
       a,  , 5

       
   Output

   The first line of output gives n and k, the number of cases and the number of
   variables to be written
   
   The remaining output is written one column at a time, so the data is written
   out in streaming style, with a variable name on a line followed by data on
   the next line.  The output consists of at least as many columns as in the
   input due to the expansion caused by missing values and categorical
   indicators.

   The presence of a non-numerical symbol (here, the 'a' in the 3rd row) in the
   data for Var1 converts Var1 into a categorical variable. Every unique value
   found in this columns will lead the software to generate an indicator (so,
   you'll get a lot of these if this was an accident and the column really is
   numerical). In this case, you'd get an output column called
   Var1[1],Var1[3],Var1[a].  For the second column, the presence of a missing
   value means that you'd get the two output variables (the mean is 3)
  
      a/b
      2 4 3 
      a/b[missing]
      0 0 1

   Assuming these the only 3 cases. Missing data is denoted by an empty field.

   
    7 Jan 09 ... Debug, formatting, better messages and comments.
   16 Dec 08 ... Created for converting data in CSV format into data suitable for auction models.

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

using namespace std;
using namespace boost::spirit;
using namespace boost::lambda;

///////////////////////////////////////////////////////////////////////////////

typedef std::vector< std::vector<std::string> > StringDataMatrix;

///////////////////////////////////////////////////////////////////////////////

class StringCatcher
{
private:
  typedef std::vector< std::string > StrVec;

  StrVec *mpStrings;

public:
  StringCatcher (StrVec *pStrings) : mpStrings(pStrings) {}
  
  template <class It>
  void operator()(It first, It last) const      { std::string s(first, last); mpStrings->push_back(s); }
};


template <class Op>
bool
parse_variable_names (char const* str, Op f)                   // Op f is bound to parse action
{
  rule<phrase_scanner_t> name_rule, name_item, option_item;    // phrase_scanner type allows space_p in parser
  
  option_item = confix_p('[', *anychar_p , ']');               // bracketed options in var name
  name_item =
     (                                                         // char first in name without quotes
      alpha_p >> *(space_p | alnum_p | ch_p('.') | ch_p('_') | ch_p('/')) >> *option_item 
      |  confix_p('\"', *c_escape_ch_p, '\"')                  // string with quotes around it
     );
  name_rule = list_p(name_item[f], ',');                       // call the operator f for each parsed string

  parse_info<> result = parse(str, name_rule, space_p);        // binding 3rd argument produces a phrase scanner  
  if (result.hit)
  { cout << "Debug: Parsing names from input line: " << endl<< "\t" << str << endl;
    if (!result.full)
    { cout << "ERROR: Incomplete parse ...  Parsing stopped at  " ;
      char const* s = result.stop;
      int limit = 10;
      while(s && limit--)
	cout << *s++;
      cout << endl;
    }
  }
  return result.hit;
}


template <class Op>
bool
parse_data_line (char const* str, Op f)
{
  rule<phrase_scanner_t> data_rule, data_item;

  data_item =
     (       
      (alpha_p >> *(space_p | alnum_p))                    // char first in name without quotes
      |   confix_p('\"', *c_escape_ch_p, '\"')             // string with quotes around it
      |   longest_d[real_p | int_p]                        // numbers
      |   eps_p                                            // no missing values
      );
  data_rule =  list_p( ! data_item[f], ',');               // ! matches 0 or 1 of the following  ... Why do we need it?

  parse_info<> result = parse(str, data_rule, space_p);    // binding 3rd argument produces a phrase scanner  
  if (!result.full) {
    cout << "ERROR: Incomplete data parse ...  Parsing stopped at  " ;
    char const* s = result.stop;
    int limit = 10;
    while(s && limit--)
      cout << *s++;
    cout << endl;
  }
  return result.hit;
}



bool
can_parse_number (std::string const& str)
{
  parse_info<> result = parse(str.c_str(), real_p);
  return result.hit;
}



double
parse_double (std::string str)
{
  if (str.size() == 0)
    return 0.0;
  else
  { double x;
    std::istringstream ss (str);
    ss >> x;
    return x;
  }
}


/*
  These functions write the output data in streaming style, with name on one
  line and the data on the following long line.
*/

void
write_missing_column (std::vector<std::string> const& varNames, StringDataMatrix const& data,
		      int column, std::ostream& output)
{
  int nObs (data.size());
  output << varNames[column] << "[missing]" << endl;
  for(int i=0; i<nObs; ++i) {
    if (data[i][column].size()==0)     output << "1 ";
    else                               output << "0 ";
  }
  output << endl;
}  

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
write_categorical_column (std::vector<std::string> const& varNames, StringDataMatrix const& data,
			  int column, std::ostream& output)
{
  // find the collection of unique values
  int nObs (data.size());
  std::set< std::string > uniqueValues;
  for (int i=0; i<nObs; ++i) {
    if (data[i][column].size() > 0)
      uniqueValues.insert(data[i][column]);
  }
  // write indicators for each label
  for (std::set<std::string>::const_iterator it = uniqueValues.begin(); it != uniqueValues.end(); ++it) {
    output << varNames[column] << "[" << *it << "]" << endl;
    for (int i=0; i<nObs; ++i)
      if (data[i][column] == *it)   output << "1 ";
      else                          output << "0 ";
  output << endl;
  }
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
csv_parser(std::istream& input, std::ostream& output)
{
  // read from input into this string
  std::string inputLine;
  
  // parse names of variables from first input line
  std::vector< std::string > inputColumnNames;
  if (getline(input, inputLine))
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
parse_arguments(int argc, char** argv, std::string& inputFile, std::string& outputFile );


int
main (int argc, char** argv)
{
  //  set default parameter values
  std::string inputFileName     ("");
  std::string outputFileName    ("");

  // parse arguments from command line
  parse_arguments(argc, argv, inputFileName, outputFileName);
  std::clog << "CSVP: Arguments    --input-file=" << inputFileName
	    << " --output-file=" << outputFileName
	    << std::endl;

  // 4 call variations
  if (inputFileName.size() == 0)
  { if (outputFileName.size() == 0)
      csv_parser(std::cin, std::cout);                // A
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
parse_arguments(int argc, char** argv, std::string& inputFile, std::string& outputFile)
{
  int key;
  while (1)                                  // read until empty key causes break
    {
      int option_index = 0;
      static struct option long_options[] = {
	  {"input-file",        1, 0, 'f'},  // has arg,
	  {"output-file",       1, 0, 'o'},  // has arg,
	  {"help",              0, 0, 'h'},  // no  arg, 
	  {0, 0, 0, 0}                       // terminator 
	};
	key = getopt_long (argc, argv, "f:o:h", long_options, &option_index);
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
	  case 'h' :
	    {
	      std::cout << "switches:" << std::endl << std::endl;
	      std::cout << "      --input-file=foo       input file" << std::endl;
	      std::cout << "      -ifoo" << std::endl << std::endl;
	      std::cout << "      --output-file=out      output file" << std::endl;
	      std::cout << "      -oout" << std::endl << std::endl;
	      std::cout << "      --help      generates this message" << std::endl;
	      std::cout << "      -h" << std::endl << std::endl;
	      exit(0);
	      break;
	    }
	  }
    }
}