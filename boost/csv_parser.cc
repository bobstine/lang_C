/* -*-c++-*-

   MAKE:  make -d csv_parser  # also runs with input from test-data.csv

   EXECUTE:  csv_parser < input.csv > output.data
             csv_parser -f input.csv -o output-file-name     # bob_format_1.0  writes into one file
	     cvs_parser -f input.csv -d output-directory     # bob_format_2.0  tar style into one directory

   The first version uses standard io.  The second uses a supplied
   file name and writes the data into the one file, all packed
   together. (bob format 1) The third allows the user to supply a
   directory; the variables are written into that directory along with
   a shell script (index.sh) that can be used to build the dense file.
   All variables will be associated with the main stream unless
   there's more to the header information of the variables.
   
   This program uses boost's BNF spirit engine to parse the
   information in a csv file.  The program converts the columns in
   that file into *numerical* columns. Categorical data is converted
   into indicators, and numerical columns that have missing data are
   expanded into a mean-filled column along with an indicator that
   shows which of the columns were missing. Naming conventions denote
   the columns so that you can recognize the indicators and such and
   perhaps process them as a block.  Bracketed terms in the names of
   output variables identify variables generated during processing
   (e.g., V1[missing] or Location[SC]).

    4 Dec 14 ... Update for revised boost engine.
   16 Dec 08 ... Created for converting data in CSV format into data suitable for auction models.

*/

#include "read_utils.h"

#include <cassert>

#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <cstdio>
#include <cstdlib> // strtod

#include <vector>
#include <map>
#include <set>

#include <iterator>
#include <algorithm>
#include <numeric>
#include <getopt.h>

#include <boost/spirit/include/classic_core.hpp>
#include <boost/spirit/home/classic/utility/confix.hpp>
#include <boost/spirit/home/classic/utility/lists.hpp>
#include <boost/spirit/home/classic/utility/escape_char.hpp>
// new ones
#include <boost/spirit/home/classic/core/non_terminal/rule.hpp>
#include <boost/spirit/home/classic/core/scanner/skipper.hpp>
// #include <boost/filesystem.hpp>


///////////////////////////////////////////////////////////////////////////////

using namespace std;
using namespace boost::spirit;
// using namespace boost::filesystem;

///////////////////////////////////////////////////////////////////////////////

typedef std::vector< std::string > StringVector;

typedef std::map   <int, StringVector> AttributeMap;  // use map since not all parses have attributes

typedef std::vector< StringVector> StringDataMatrix;

///////////////////////////////////////////////////////////////////////////////


std::string
fill_blanks(std::string str)
{
  str = read_utils::trim(str);
  for(std::string::iterator it=str.begin(); it!=str.end(); ++it)
    if(*it == ' ' || *it == '"')
      *it = '_';
  return str;
}


void
peel_quotes_from_string_vector (StringVector & sv)
{
  std::transform(sv.begin(), sv.end(), sv.begin(),
		 [] (std::string const& s) -> std::string
		 { if((s[0]=='"') && (s.size()>2)) return s.substr(1,s.size()-2); else return s; });
}


class StringCatcher
{
private:
  StringVector *mpStrings;

public:
  StringCatcher (StringVector *pStrings) : mpStrings(pStrings) {}
  
  template <class It>
  void operator()(It first, It last) const      { std::string s(first, last); mpStrings->push_back(s); }
};


class MappedStringCatcher   // holds attributes of names in a map
{
private:
  StringVector   *mpStrings;
  AttributeMap   *mpMap;
  
public:
  MappedStringCatcher (StringVector *pStrings, AttributeMap *pMap) : mpStrings(pStrings), mpMap(pMap) {}
  
  template <class It>
  void operator()(It first, It last) const
  { std::string s(first, last);
    int position (mpStrings->size()-1);     // shift by one for index 0 since attaching attribute to parsed name
    (*mpMap)[position].push_back(s);
  }
};


///////////////////////////////////////////////////////////////////////////////

void
check_for_duplicate_names (StringVector *sv)
{
  std::map<std::string, int> counter;
  for(StringVector::iterator it=sv->begin(); it != sv->end(); ++it)
  {
    int k;
    counter[*it] += 1;
    if ((k=counter[*it]) > 1)
    { ostringstream oss;
      oss << k;
      *it = *it + "_" + oss.str();
    }
  }
}


StringVector
set_default_stream_name (std::string name, StringVector const& sv)  // copies it, changing stream attribute
{
  StringVector result;
  bool hasStreamAttr = false;
  for(unsigned int j=0; j < sv.size(); j+=2)
  { if (sv[j] == "stream")
      hasStreamAttr = true;
    result.push_back(sv[j  ]);
    result.push_back(sv[j+1]);
  }
  if (!hasStreamAttr)
  { result.push_back("stream");
    result.push_back(name);
  }
  return result;
}

  

///////////////////////////////////////////////////////////////////////////////

template <class OpName, class OpAttribute>
bool
parse_variable_names (char const* str, OpName f, OpAttribute g)         // Op f is bound to parse action for name, g for attribute
{
  using classic::rule;
  using classic::ch_p;
  using classic::alpha_p;
  using classic::space_p;
  using classic::alnum_p;
  using classic::phrase_scanner_t;
    
  rule<phrase_scanner_t> name_rule, name_item;               // phrase_scanner_t allows space_p in parser
  rule<phrase_scanner_t> attribute_rule, attribute_pair, attribute_term;
  rule<phrase_scanner_t> token;

  name_item =
     (                                                       // char first in name without quotes
      (alpha_p >> *(space_p | alnum_p
			     | ch_p('.') | ch_p('_') | ch_p('/') | ch_p("[") | ch_p("]") | ch_p("=")))
      |  classic::confix_p('\"', *classic::c_escape_ch_p, '\"')                // string with quotes around it
     );

  attribute_term = (alnum_p >> *(alnum_p));
  attribute_pair = (attribute_term[g] >> ch_p("=") >> attribute_term[g]);
  attribute_rule = (ch_p('{') >>  attribute_pair >> *(ch_p(';') >> attribute_pair) >> ch_p('}'));

  token = (name_item[f] >> !attribute_rule);                 // opName operator does the work; zero or one attributes
  
  name_rule = classic::list_p(token, ',');                            // call the operator f for each parsed string

  classic::parse_info<> result = parse(str, name_rule, space_p);      // binding 3rd argument produces a phrase scanner  
  if (result.hit)
  { // clog << "Debug: Parsing names from input line: " << endl<< "\t" << str << endl;
    if (!result.full)
    { cerr << "ERROR: Incomplete parse in list of variable names.  Parsing stopped at  " ;
      char const* s = result.stop;
      int limit = 10;
      while(s && limit--)
	clog << *s++;
      clog << endl;
    }
  }
  return result.hit;
}


template <class OpName>
bool
parse_data_line (char const* str, OpName f)
{
  classic::rule<classic::phrase_scanner_t> data_rule, data_item;

  data_item =
     (       
      (classic::alpha_p >> *(classic::space_p | classic::alnum_p))                    // char first in name without quotes
      |   classic::confix_p('\"', *classic::c_escape_ch_p, '\"')             // string with quotes around it
      |   classic::longest_d[classic::real_p | classic::int_p]                        // numbers
      |   classic::eps_p                                            // no missing values
      );
  data_rule =  classic::list_p( ! data_item[f], ',');               // ! matches 0 or 1 of the following  ... Why do we need it?

  classic::parse_info<> result = parse(str, data_rule, classic::space_p);    // binding 3rd argument produces a phrase scanner  
  if (!result.full) {
    cout << "ERROR: Incomplete data parse ...  Parsing stopped at  " ;
    char const* s = result.stop;
    int limit = 15;
    while(s && limit--)
      cout << *s++;
    cout << endl;
    return false;
  }
  return result.hit;
}



bool
can_parse_number (std::string const& str)
{
  classic::parse_info<> result = parse(str.c_str(), classic::real_p);
  //  cannot return result.hit since leading number will be parsed (ie, 777x can parse leading digits)
  return result.full;
}



double
parse_double (std::string str)
{
  if (str.size() == 0)
    return 0.0;
  else
    return std::strtod(str.c_str(), NULL);
}



//////////////////////////////////////////////////////////////////////////////////////////////////////////


typedef std::pair<std::string, std::string> StringPair;

StringPair
get_prefixes(std::string tarPath)
{
  if(tarPath.size() > 0)
    return make_pair("echo ", "cat ");
  else
    return make_pair("", "");
}


/*
  These functions write the output data in streaming style, with
       - name of the variable on current line
       - data for the variable as a following line (which can be long)
*/

void
write_missing_data (StringDataMatrix const& data, int column, std::ostream& output)
{
  for(int i=0; i<(int)data.size(); ++i)
  { if (data[i][column].size()==0)     output << "1 ";
    else                               output << "0 ";
  }
  output << endl;
}  
  
void
write_missing_column (std::string const& varName, StringVector const& attributes, StringDataMatrix const& data,
		      int column, std::ostream& output, std::string tarPath)
{
  StringPair prefixes (get_prefixes(tarPath));
  // write name with missing appended as leading line plus as an attribute
  std::string name (fill_blanks(varName + "_missing"));
  output << prefixes.first << fill_blanks(name) << endl;
  // write attributes
  output << prefixes.first << "role x parent " << fill_blanks(varName) << " category missing ";
  std::copy(attributes.begin(), attributes.end(), std::ostream_iterator<std::string>(output," "));
  output << endl;
  // write data
  if (tarPath.size() == 0)
    write_missing_data(data, column, output);
  else
  { output << prefixes.second << name << std::endl;
    std::ofstream dataStream ((tarPath+name).c_str());
    write_missing_data(data, column, dataStream);
  }
}  


void
write_numerical_data (StringDataMatrix const& data, int column, int numberMissing, std::ostream& output)
{
  int nObs (data.size());
  if (0 == numberMissing)                                // write directly to output
    for(int i=0; i<nObs; ++i)
      output << data[i][column] << " ";
  else {                                                 // insert mean value in missing locations
    double sum (0.0);
    std::vector<int> missingPos;
    for(int i=0; i<nObs; ++i)
    { if (data[i][column].size() > 0)                    // row i is not missing, so include in the sum
	sum += parse_double(data[i][column]);
      else
	missingPos.push_back(i);
    } 
    assert((numberMissing == (int)missingPos.size()));   // verify number missing found here match those found during scanning
    missingPos.push_back(nObs);                          // sentinel at the end
    double mean (sum/(nObs-numberMissing));
    for(int i=0, j=0; i<nObs; ++i)
    { if(missingPos[j]>i)
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
write_numerical_column  (std::string const& varName, StringVector const& attributes, StringDataMatrix const& data,
			 int column, int numberMissing, std::ostream& output, std::string tarPath)
{
  StringPair prefixes (get_prefixes(tarPath));
  // see if list of assigned attributes includes role
  bool hasRole (false);
  if (attributes.size())
  { std::set<std::string> attrNames;
    std::for_each(attributes.begin(), attributes.end(), [&attrNames](std::string const& s)->void { attrNames.insert(get_word_from_string(s)); });
    hasRole = (attrNames.end() != attrNames.find("role"));
    std::clog << "CSVP: Variable " << varName << " has preassigned role.\n";
  }
  // start by writing the undecorated name to output on top line and as attribute
  std::string name (fill_blanks(varName));
  output << prefixes.first << name << endl;
  // write attributes
  output << prefixes.first;
  if (!hasRole) output << "role x ";
  std::copy(attributes.begin(), attributes.end(), std::ostream_iterator<std::string>(output," "));
  output << endl;
  // now write the observed data, with any missing value inserted
  if (tarPath.size() == 0)
    write_numerical_data(data, column, numberMissing, output);
  else
  { output << prefixes.second << name << std::endl;
    std::ofstream file ((tarPath+name).c_str());
    write_numerical_data(data, column, numberMissing, file);
  }
}



// result is whether the column identifies a leave-out-cases selector
bool
data_has_selection_indicator(StringDataMatrix const& data)
{
  // find the collection of unique values in first column
  int nObs (data.size());
  std::set< std::string > uniqueLabels;
  for (int i=0; i<nObs; ++i)
  { if (data[i][0].size() > 0)
      uniqueLabels.insert(data[i][0]);
  }
  if (uniqueLabels.size() != 2)
    return false;
  else  // check if names match in/out
  { std::set<std::string>::const_iterator iter (uniqueLabels.begin());
    std::string l1 (*iter);     ++iter;
    std::string l2 (*iter);
    // put both in lower case
    std::transform(l1.begin(), l1.end(), l1.begin(), ::tolower);
    std::transform(l2.begin(), l2.end(), l2.begin(), ::tolower);
    return (((l1 == "in")&&(l2 == "out")) || ((l2 == "in")&&(l1=="out")));
  }
}
  



void
write_categorical_data (StringDataMatrix const& data, int column, std::string label, std::ostream& output)
{
  for (unsigned int i=0; i<data.size(); ++i)
    if (data[i][column] == label)   output << "1 ";
    else                            output << "0 ";
  output << endl;
}  

void
write_categorical_column (std::string const& varName, StringVector const& attributes, StringDataMatrix const& data,
			  int column, bool dropLastLabel, std::ostream& output, std::string tarPath)
{
  StringPair prefixes = get_prefixes(tarPath);
  // see if list of assigned attributes includes role
  bool hasRole (false);
  if (attributes.size())
  { std::set<std::string> attrNames;
    std::for_each(attributes.begin(), attributes.end(), [&attrNames](std::string const& s)->void { attrNames.insert(get_word_from_string(s)); });
    hasRole = (attrNames.end() != attrNames.find("role"));
    std::clog << "CSVP: Variable " << varName << " has preassigned role.\n";
  }
  // find the collection of unique values
  int nObs (data.size());
  std::set< std::string > uniqueValues;
  for (int i=0; i<nObs; ++i)
  { if (data[i][column].size() > 0)
      uniqueValues.insert(data[i][column]);
  }
  // avoid last label if indicated
  std::set<std::string>::const_iterator last (uniqueValues.end());
  if (dropLastLabel) --last;
  // write data for each label
  for (std::set<std::string>::const_iterator it = uniqueValues.begin(); it != last; ++it)
  { std::string name (fill_blanks(varName + "[" + *it + "]"));
    // write name, then as attribute
    output << prefixes.first << name << endl;
    // write attributes for each indicator
    output << prefixes.first;
    if (!hasRole) output << "role x ";
    std::copy(attributes.begin(), attributes.end(), std::ostream_iterator<std::string>(output," "));
    output << "parent " << fill_blanks(varName) << " category " << fill_blanks(*it) << " ";
    output << endl;
    // data
    if (tarPath.size()==0)
      write_categorical_data(data, column, *it, output);
    else
    { output << prefixes.second << name << std::endl;
      std::ofstream dataStream ((tarPath+name).c_str());
      write_categorical_data(data, column, *it, dataStream);
    }
  }
}



int
number_output_columns(StringDataMatrix const& data, bool hasSubsetSelector,
		      std::vector<int> const& varMissingCount, std::vector<int> const& varNumericCount)
{
  int nInputCols (varMissingCount.size()); 
  int nObs       (data.size());
  // number of missing indicators
  int nColsWithMissing (0);
  for (int j=0; j<nInputCols; ++j)
    if(varMissingCount[j]>0) ++nColsWithMissing;
  // number of categories in those that are not numeric
  int nColsForCategorical (0);
  std::clog << "Parser: #Categories: ";
  for (int j=0; j<nInputCols; ++j)
  { if(varNumericCount[j]+varMissingCount[j]==nObs)   // numeric
      std::clog << "0 ";
    else // categorical
    { std::set< std::string > uniqueValues;
      for (int i=0; i<nObs; ++i) {
	if (data[i][j].size() > 0)                   // empty -> missing
	  uniqueValues.insert(data[i][j]);
      }
      int nCats (uniqueValues.size());
      std::clog << nCats << " ";
      --nCats;   // already counted 1 for input
      nColsForCategorical += nCats;
    }
  }
  int nOutputCols (nInputCols + nColsWithMissing + nColsForCategorical);
  std::clog << "\nParser: Output " << nInputCols << " input cols + " << nColsWithMissing << " missing indicators + "
	    << nColsForCategorical << " columns for indicators.\n";
  if (hasSubsetSelector)
  { std::clog << "Parser: One column dropped since data have selector.\n";
    -- nOutputCols;
  }
  return nOutputCols;
}



void
write_numerical_data_file (std::vector<std::string> const& varNames, AttributeMap& attributes, StringDataMatrix const& data,
			   bool hasSelector,
			   std::vector<int> const& varMissingCount, std::vector<int> const& varNumericCount,
			   std::ostream& output, std::string tarPath)
{
  // prefixes distinguish tar data
  StringPair prefixes = get_prefixes(tarPath);
  // write leading header with number of obs, number of cols
  if (tarPath.size()>0)    output << "#!/bin/sh\n";
  output << prefixes.first << data.size() <<  endl;

  int nVars (varNames.size());
  int nObs  (data.size());
  
  int column = 0;
  if (hasSelector)
  { StringVector role;
    role.push_back("role context");
    write_categorical_column("cv.indicator", role, data, column, true, output, tarPath);  // true = drop last label
    ++column;
  }
  for (; column<nVars; ++column)
  {  
    if((varNumericCount[column]+varMissingCount[column])==nObs) // numerical column with possible missing
    { 
      write_numerical_column (varNames[column], attributes[column], data, column, varMissingCount[column], output, tarPath);
    }
    else
    {
      write_categorical_column (varNames[column], attributes[column], data, column, false, output, tarPath); // false = use all labels
    }
    if (varMissingCount[column] > 0)
    {
      write_missing_column (varNames[column], attributes[column], data, column, output, tarPath);
    }
  }
}


// return number of obs, number of vars written
std::pair<int, int>
csv_parser(std::istream& input, std::ostream& output, std::string tarPath="")
{
  // read from input into this string
  std::string inputLine;
  // parse names of variables from first input line
  StringVector  inputColumnNames;
  AttributeMap  inputAttributes;
  if (getline(input, inputLine))
  { if (parse_variable_names(inputLine.c_str(),
			     StringCatcher( &inputColumnNames ),
			     MappedStringCatcher( &inputColumnNames, &inputAttributes ) ))
    { std::clog <<  "\nParser: Read " << inputColumnNames.size() << " variable names from the input data.  These are:\n" << endl;
      peel_quotes_from_string_vector(inputColumnNames);
      for (std::vector<std::string>::iterator it = inputColumnNames.begin(); it != inputColumnNames.end(); ++it)
	std::clog << " |" << *it << "| " << endl;
      for (int i=0; i<(int)inputColumnNames.size(); ++i)
	if (!inputAttributes[i].empty())
	{ std::clog << "Attributes[" << inputColumnNames[i] << ", var #" << i << "]  ";
	  for(unsigned int j=0; j<inputAttributes[i].size(); ++j)
	    std::clog << inputAttributes[i][j] << " ";
	  std::clog << endl;
	}
    }
    else
    { std::cerr<< "Parser: ERROR. Failed to parse variable names in first line of input stream.\n" << endl;
      return std::make_pair(0,inputColumnNames.size());
    }
  }
  else
  { std::cerr << "Parser: ERROR. Unable to read first line from input stream.\n " << endl;
    return std::make_pair(0,0);
  }
  // check for duplicated variable names
  check_for_duplicate_names (&inputColumnNames);
  // insert default stream name if attribute not found  ... Auction does this automatically now
  //  for(unsigned int j=0; j<inputColumnNames.size(); ++j)
  //     inputAttributes[j] = set_default_stream_name ("main", inputAttributes[j]);
  // set up vectors to count types of data in columns (# missing in each column, # numbers in each)
  int nVars (inputColumnNames.size());
  std::vector< int > numeric (nVars);
  std::vector< int > missing (nVars);
  // iterate through remaining lines of data in order to build the matrix of strings
  StringDataMatrix dataMatrix;
  int lineNumber (0);
  while (getline(input, inputLine))
  { ++lineNumber;
    // replace ,NA, by ,, so that missing fields are denoted by empty strings
    std::string::size_type iNA;
    if (inputLine.substr(0,3) == "NA,")
      inputLine.replace(0,2,"");
    std::string::size_type len (inputLine.size());
    if (inputLine.substr(len-3,3) == ",NA")
      inputLine.replace(len-2,2,"");    
    while(std::string::npos != (iNA = inputLine.find(",NA,")))
      inputLine.replace(iNA,4,",,");
    // convert the input line into distinct strings
    StringVector inputData;
    if( parse_data_line(inputLine.c_str(), StringCatcher( &inputData )) ) {
      // cout <<  "\nData from the item parser:" << endl;
      // for (std::vector<std::string>::iterator it = inputData.begin(); it != inputData.end(); ++it)  {
      // cout << " |" << *it << "| " << endl; 
      if ((int)inputData.size() == nVars)
      { peel_quotes_from_string_vector(inputData);
	dataMatrix.push_back(inputData);
	for(int i=0; i<nVars; ++i)
	{ if (0 == inputData[i].size())             ++missing[i];
	  else if (can_parse_number(inputData[i]))  ++numeric[i];
	}
      }
      else
      { std::cerr
	  << "Parser: Error parsing data in line " << lineNumber
	  << ". Number of data elements (" << inputData.size()
	  << ") unequal to number of variables (" << nVars << ").\n\t" << inputLine << endl;
      }
    }
    else std::cerr << "Parser: Error parsing data in line " << lineNumber
		   << ". Failed to parse input CSV data.\n\t" << inputLine << endl;
  }
  std::clog << "Parser:  nObs = " << dataMatrix.size() << " with nVars = " << nVars << endl;
  std::clog << "Parser: #Numeric   : " ;
  for (int i = 0; i<nVars; ++i)
    std::clog << numeric[i] << " ";
  std::clog << "\nParser: #Missing   : " ;
  for (int i = 0; i<nVars; ++i)
    std::clog << missing[i] << " ";
  std::clog << endl;

  bool hasSelector = data_has_selection_indicator(dataMatrix);
  int nToWrite = number_output_columns(dataMatrix, hasSelector, missing, numeric);
  std::clog << "Parser: Writing " << nToWrite << " columns to output.\n";
  
  // write data
  write_numerical_data_file (inputColumnNames, inputAttributes, dataMatrix, hasSelector, missing, numeric, output, tarPath);
  
  return std::make_pair(dataMatrix.size(), nVars);
}

////////////////////////////////////////////////////////////////////////////////


void
parse_arguments(int argc, char** argv, std::string& inputFile, std::string& outputName, bool &useTar);


int
main (int argc, char** argv)
{
  //  set default parameter values
  std::string inputFileName     ("");
  std::string outputFileName    ("");
  bool        useTar            (false);

  // parse arguments from command line
  parse_arguments(argc, argv, inputFileName, outputFileName, useTar);
  std::clog << "CSVP: Arguments    --input-file=" << inputFileName;
  if (useTar)
    std::clog << " --directory=" << outputFileName << std::endl;
  else
    std::clog << " --output-file=" << outputFileName << std::endl;

  // input/output variations
  std::ifstream input (inputFileName.c_str());
  if (!input && inputFileName.size() > 0)
  { std::cerr << "CSVP: Error. Cannot open input file " << inputFileName << std::endl;
    return 2;
  }
  if (useTar)
  { std::string outputPath (outputFileName);
    if (outputPath[outputPath.size()-1] != '/')      // make sure ends in /
      outputPath = outputPath + "/";
    // if (!exists(outputPath) create_directory(outputPath);   boost is broken!!!
    std::string   indexFileName (outputPath+"index.sh");
    std::ofstream indexStream   (indexFileName.c_str());
    if (!indexStream)
    { std::clog << "Cannot open output directory; terminating.\n";
      return 1;
    }
    if (input)
      csv_parser(input, indexStream, outputPath);    // presense of last string indicates tar format
    else
      csv_parser(std::cin, indexStream, outputPath);
  }
  else
  {
    if (outputFileName.size() == 0)
    { if (input)
	csv_parser(input, std::cout);
      else
	csv_parser(std::cin, std::cout);
    }
    else
    { std::ofstream output (outputFileName.c_str());
      if (!output)
      { std::cerr << "CSVP: Error. Cannot open output file " << outputFileName << std::endl;
	return 3;
      }
      if (input)
	csv_parser(input, output);
      else
	csv_parser(std::cin, output);
    }
  }
  return 0;
}



void
parse_arguments(int argc, char** argv, std::string& inputFile, std::string& outputName, bool &useTar)
{
  int key;
  while (1)                                  // read until empty key causes break
    {
      int option_index = 0;
      static struct option long_options[] = {
	  {"input-file",        1, 0, 'f'},  // has arg,
	  {"output-file",       1, 0, 'o'},  // has arg,
	  {"directory",         1, 0, 'd'},  // has arg,
	  {"help",              0, 0, 'h'},  // no  arg, 
	  {0, 0, 0, 0}                       // terminator 
	};
	key = getopt_long (argc, argv, "f:o:d:h", long_options, &option_index);
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
	      useTar = false;
	      std::string name(optarg);
	      outputName = optarg;
	      break;
	    }
	  case 'd' :  
	    {
	      useTar = true;
	      std::string name(optarg);
	      outputName = optarg;
	      break;
	    }
	  case 'h' :
	    {
	      std::cout << "switches:" << std::endl << std::endl;
	      std::cout << "      --input-file=foo       input file" << std::endl;
	      std::cout << "      -ifoo" << std::endl << std::endl;
	      std::cout << "      --output-file=out      output file" << std::endl;
	      std::cout << "      -oout" << std::endl << std::endl;
	      std::cout << "      --directory=name       directory name" << std::endl;
	      std::cout << "      -dname" << std::endl << std::endl;
	      std::cout << "      --help      generates this message" << std::endl;
	      std::cout << "      -h" << std::endl << std::endl;
	      exit(0);
	      break;
	    }
	  }
    }
}
