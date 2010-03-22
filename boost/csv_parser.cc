/* -*-c++-*-

   MAKE:  make -d csv_parser  # also runs with input from test-data.csv

   EXECUTE:  csv_parser < input.csv > output.data
             csv_parser -f input.csv -o output-file-name
             csv_parser -s stream < input.csv > output_stream.data

   The first version uses standard io.  The second uses a supplied file name.
   The last allows the user to supply a stream name.  All variables will be
   associated with a stream named 'stream'.
   
   This program uses boost's BNF spirit engine to parse the information in a csv
   file.  The program converts the columns in that file into *numerical*
   columns. Categorical data is converted into indicators, and numerical columns
   that have missing data are expanded into a mean-filled column along with an
   indicator that shows which of the columns were missing. Naming conventions
   denote the columns so that you can recognize the indicators and such and
   perhaps process them as a block.  Bracketed terms in the names of output
   variables identify variables generated during processing (e.g., V1[missing]
   or Location[SC]).  
   

   Input -----------------------------
   
   The input data should have the style of named columns. Names can consist of
   characters, numbers, and the symbols " " (space), ".", "_", "[", "]", and
   "/".  Others might ought to be added the in later parsing of variables.
   Attributes can be assigned to the variable by listing name=value pairs of
   attributes {within curly brackets} and separated by semi-colons. NO spaces
   are allowed within the attributes.  Currently used attributes defined by the
   software are

      parent    attached to a variable derived from another (eg, categorical or missing)
      category  category represented
      stream    identifies a specific input stream  (defaults to main)
      name      repeated name of the variable (non-blank text only)

   For example, an input csv file with 3 variables might begin as follows

      Var1, a/b, Var.3{stream=sub;priority=2;knots=4}
       1, 2, 3
       3, 4, 5
       a,  , 5

   Missing data in the input file is denoted by an empty field.  If the same
   name is used for two (or more columns), the second occurance will have a _2
   appended, then a _3, and so forth for others.  The input stream option is a
   'convenience function' that avoids having to use the bracketed option
   {stream=stream_name} for every variable in the file.

   If the first input column is a list of two or more of the labels

       "in", "out", or "na"

   then the parser will treat this column differently; it will treat
   this column as an indicator of which cases are to be used in
   subsequent analysis.  Rather than generate 2 or 3 indicators (one
   for every symbol), it will only generate 1 or 2.  These boolean
   variables will be placed first in the file sent to the auction for
   modeling. In the auction, only those cases marked "in" will be used
   for estimation.  Those marked "na" have explanatory variables
   available which can be employed in modeling, such as for lagged
   variables or smoothing.  All will be predicted.

   Known limitations:

      No blanks at the end of the line are allowed!
      You need a *mix* of in/out for the leading indicator (not all in).
      
   
   Output -----------------------------

   The first line of output gives n and k, the number of cases and the number of
   variables to be written.
   
   The remaining output is written one column at a time, so the data is written
   out in streaming style, with three lines for each variable:

      (1) the name of the variable (square brackets denote an indicator)
      (2) attribute for this variable as space delimited strings/numbers
      (3) data
     
   As in JMP, square brackets in the name of a variable identify an indicator
   for a category.  If a variable lacks attributes, the second line begins with
   '*'.  The output consists of at least as many columns as in the input due to
   the expansion caused by missing values and categorical indicators.

   The presence of a non-numerical symbol (in the example, the 'a' in the 3rd
   row) in the data for Var1 converts Var1 into a categorical variable. Every
   unique value found in this column will cause the software to generate an
   indicator (so, you'll get a lot of these if this was an accident and the
   column really is numerical).

   In this example, you'd get output columns called Var1[1], Var1[3], Var1[a].
   For the second column, the presence of a missing value means that you'd get
   the two output variables (the mean is 3).  The last variable is assigned to
   stream sub.
  
      Var1[1]
      name Var1[1]  stream main  parent Var1  category 1
      1 0 0
      Var1[3]
      name Var1[3]  stream main  parent Var1  category 3
      0 1 0
      Var1[a]
      name Var1[a]  stream main  parent Var1  category a
      0 0 1
      a/b
      name a/b  stream main
      2 4 3 
      a/b[missing]
      name a/b[missing]  stream main  parent a/b  category missing
      0 0 1
      Var.3
      name Var.3  stream sub
      3 5 5
      
   11 Nov 09 ... Attributes read and written.   
    7 Jan 09 ... Debug, formatting, better messages and comments.
   16 Dec 08 ... Created for converting data in CSV format into data suitable for auction models.

*/

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

#include <boost/spirit/core.hpp>
#include <boost/spirit/utility/confix.hpp>
#include <boost/spirit/utility/lists.hpp>
#include <boost/spirit/utility/escape_char.hpp>
#include <boost/lambda/lambda.hpp>


///////////////////////////////////////////////////////////////////////////////

using namespace std;
using namespace boost::spirit;

///////////////////////////////////////////////////////////////////////////////

typedef std::vector< std::string > StringVector;

typedef std::map   <int, StringVector> AttributeMap;  // use map since not all parses have attributes

typedef std::vector< StringVector> StringDataMatrix;

///////////////////////////////////////////////////////////////////////////////


std::string
fill_blanks(std::string str)
{
  for(std::string::iterator it=str.begin(); it!=str.end(); ++it)
    if(*it == ' ' || *it == '"')
      *it = '_';
  return str;
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
  rule<phrase_scanner_t> name_rule, name_item;               // phrase_scanner_t allows space_p in parser
  rule<phrase_scanner_t> attribute_rule, attribute_pair, attribute_term;
  rule<phrase_scanner_t> token;

  name_item =
     (                                                       // char first in name without quotes
      (alpha_p >> *(space_p | alnum_p | ch_p('.') | ch_p('_') | ch_p('/') | ch_p("[") | ch_p("]") | ch_p("=")))
      |  confix_p('\"', *c_escape_ch_p, '\"')                // string with quotes around it
     );

  attribute_term = (alnum_p >> *(alnum_p));
  attribute_pair = (attribute_term[g] >> ch_p("=") >> attribute_term[g]);
  attribute_rule = (ch_p('{') >>  attribute_pair >> *(ch_p(';') >> attribute_pair) >> ch_p('}'));

  token = (name_item[f] >> !attribute_rule);                 // opName operator does the work; zero or one attributes
  
  name_rule = list_p(token, ',');                            // call the operator f for each parsed string

  parse_info<> result = parse(str, name_rule, space_p);      // binding 3rd argument produces a phrase scanner  
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
    return false;
    /* char const* s = result.stop;
       int limit = 10;
       while(s && limit--)
       cout << *s++;
       cout << endl;
    */
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
    return std::strtod(str.c_str(), NULL);
}



/*
  These functions write the output data in streaming style, with
       - name of the variable on current line
       - data for the variable as a following line (which can be long)
*/


void
write_missing_column (std::string const& varName, StringVector const& attributes, StringDataMatrix const& data,
		      int column, std::ostream& output)
{
  // write name with missing appended as leading line plus as an attribute
  std::string name (varName + "[missing]");
  output << name << endl << "name " << fill_blanks(name) << " ";
  // write attributes
  output << "parent " << varName << " category missing ";
  if (!attributes.empty())
    std::copy(attributes.begin(), attributes.end(), std::ostream_iterator<std::string>(output," "));
  output << endl;
  // write data
  int nObs (data.size());
  for(int i=0; i<nObs; ++i) {
    if (data[i][column].size()==0)     output << "1 ";
    else                               output << "0 ";
  }
  output << endl;
}  

void
write_numerical_column  (std::string const& varName, StringVector const& attributes, StringDataMatrix const& data,
			  int column, int numberMissing, std::ostream& output)
{
  // start by writing the undecorated name to output on top line and as attribute
  output << varName << endl << "name " << fill_blanks(varName) << " ";
  // write rest of attributes
  if (!attributes.empty())
    std::copy(attributes.begin(), attributes.end(), std::ostream_iterator<std::string>(output," "));
  output << endl;
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
write_categorical_column (std::string const& varName, StringVector const& attributes, StringDataMatrix const& data,
			  int column, bool dropLastLabel, std::ostream& output)
{
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
  { std::string name (varName + "[" + *it + "]");
    // write name, then as attribute
    output << name << endl << "name " << fill_blanks(name) << " ";
    // write attributes for each indicator
    output << "parent " << varName << " category " << *it << " ";
    if (!attributes.empty())
      std::copy(attributes.begin(), attributes.end(), std::ostream_iterator<std::string>(output," "));
    output << endl;
    for (int i=0; i<nObs; ++i)
      if (data[i][column] == *it)   output << "1 ";
      else                          output << "0 ";
  output << endl;
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
			   std::ostream& output)
// writes columns in streaming fashion
{
  int nVars (varNames.size());
  int nObs  (data.size());
  StringVector noAttr (0);

  int column = 0;
  if (hasSelector)
  { write_categorical_column("[in/out]", noAttr, data, column, true, output);  // true = drop last label
    ++column;
  }
  for (; column<nVars; ++column)
  {  
    if((varNumericCount[column]+varMissingCount[column])==nObs) // numerical column with possible missing
    { 
      write_numerical_column (varNames[column], attributes[column], data, column, varMissingCount[column], output);
    }
    else
    {
      write_categorical_column (varNames[column], attributes[column], data, column, false, output); // false = use all labels
    }
    if (varMissingCount[column] > 0)
    {
      write_missing_column (varNames[column], attributes[column], data, column, output);
    }
  }
}


// return number of obs, number of vars written
std::pair<int, int>
csv_parser(std::istream& input, std::ostream& output, std::string streamName)
{
  // read from input into this string
  std::string inputLine;
  
  // parse names of variables from first input line
  StringVector  inputColumnNames;
  AttributeMap  inputAttributes;
  
  if (getline(input, inputLine))
  {
    if (parse_variable_names(inputLine.c_str(),
			     StringCatcher( &inputColumnNames ),
			     MappedStringCatcher( &inputColumnNames, &inputAttributes ) ))
    { std::clog <<  "\nParser: Read " << inputColumnNames.size() << " variable names from the input data.  These are:\n" << endl;
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

  // insert default stream name if attribute not found
  for(unsigned int j=0; j<inputColumnNames.size(); ++j)
    inputAttributes[j] = set_default_stream_name (streamName, inputAttributes[j]);
  
  // set up vectors to count types of data in columns (# missing in each column, # numbers in each)
  int nVars (inputColumnNames.size());
  std::vector< int > numeric (nVars);
  std::vector< int > missing (nVars);
  
  // iterate through remaining lines of data in order to build the matrix of strings
  StringDataMatrix dataMatrix;

  int lineNumber (0);
  while (getline(input, inputLine))
  { ++lineNumber;
    std::vector<std::string> inputData;
    if( parse_data_line(inputLine.c_str(), StringCatcher( &inputData )) ) {
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
  std::clog << "Parser: Writing " << nToWrite << " column streams to output file.\n";
  
  // write leading header with number of obs, number of cols
  output << dataMatrix.size() << " " << nToWrite << endl;
  // write data
  write_numerical_data_file (inputColumnNames, inputAttributes, dataMatrix, hasSelector, missing, numeric, output);
  
  return std::make_pair(dataMatrix.size(), nVars);
}

////////////////////////////////////////////////////////////////////////////////


void
parse_arguments(int argc, char** argv, std::string& inputFile, std::string& outputFile, std::string& streamName);


int
main (int argc, char** argv)
{
  //  set default parameter values
  std::string inputFileName     ("");
  std::string outputFileName    ("");
  std::string streamName        ("main");

  // parse arguments from command line
  parse_arguments(argc, argv, inputFileName, outputFileName, streamName);
  std::clog << "CSVP: Arguments    --input-file=" << inputFileName
	    << " --output-file=" << outputFileName
	    << " --stream=" << streamName 
	    << std::endl;

  // 4 call variations
  if (inputFileName.size() == 0)
  { if (outputFileName.size() == 0)
      csv_parser(std::cin, std::cout, streamName);    // A
    else
    { std::ofstream output (outputFileName.c_str());
      if (!output)
      { std::cerr << "CSVP: Error. Cannot open output file " << outputFileName << std::endl;
	return 1;
      }
      csv_parser(std::cin, output, streamName);       // B
    }
  }
  else
  { std::ifstream input (inputFileName.c_str());
    if (!input)
    { std::cerr << "CSVP: Error. Cannot open input file " << inputFileName << std::endl;
      return 2;
    }
    if (outputFileName.size() == 0)
      csv_parser(input, std::cout, streamName);       // C
    else
    { std::ofstream output (outputFileName.c_str());
      if (!output)
      { std::cerr << "CSVP: Error. Cannot open output file " << outputFileName << std::endl;
	return 3;
      }
      csv_parser(input, output, streamName);          // D
    }
  }
  return 0;
}



void
parse_arguments(int argc, char** argv, std::string& inputFile, std::string& outputFile, std::string& stream)
{
  int key;
  while (1)                                  // read until empty key causes break
    {
      int option_index = 0;
      static struct option long_options[] = {
	  {"input-file",        1, 0, 'f'},  // has arg,
	  {"output-file",       1, 0, 'o'},  // has arg,
	  {"stream",            1, 0, 's'},  // has arg,
	  {"help",              0, 0, 'h'},  // no  arg, 
	  {0, 0, 0, 0}                       // terminator 
	};
	key = getopt_long (argc, argv, "f:o:s:h", long_options, &option_index);
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
	  case 's' :  
	    {
	      std::string name(optarg);
	      stream = optarg;
	      break;
	    }
	  case 'h' :
	    {
	      std::cout << "switches:" << std::endl << std::endl;
	      std::cout << "      --input-file=foo       input file" << std::endl;
	      std::cout << "      -ifoo" << std::endl << std::endl;
	      std::cout << "      --output-file=out      output file" << std::endl;
	      std::cout << "      -oout" << std::endl << std::endl;
	      std::cout << "      --stream=name          stream name" << std::endl;
	      std::cout << "      -sname" << std::endl << std::endl;
	      std::cout << "      --help      generates this message" << std::endl;
	      std::cout << "      -h" << std::endl << std::endl;
	      exit(0);
	      break;
	    }
	  }
    }
}
