#include <cassert>

#include <string>
#include <iostream>
#include <sstream>
#include <cstdio>

#include <vector>
#include <set>

#include <iterator>
#include <algorithm>
#include <numeric>

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


class NumberWriter
{
  template <class It>
  void operator()(It first, It last) const      { std::copy (first, last, std::ostream_iterator<char *>(std::cout)); }
};


template <class Op>
bool
parse_variable_names (char const* str, Op f)
{
  rule<phrase_scanner_t> name_rule, name_item, option_item;    // phrase_scanner type allows space_p in parser
  
  option_item = confix_p('[', *anychar_p , ']');               // bracketed options in var name
  name_item =
     (       
      alpha_p >> *(space_p | alnum_p) >> *option_item          // char first in name without quotes
      |  confix_p('\"', *c_escape_ch_p, '\"')                  // string with quotes around it
     );
  name_rule = list_p(name_item[f], ',');

  parse_info<> result = parse(str, name_rule, space_p);   // binding 3rd argument produces a phrase scanner  
  if (result.hit) {
    cout << "Parsing names from input line: " << endl<< "\t" << str << endl;
    if (!result.full) {
      cout << "Incomplete parse ...  Parsing stopped at  " ;
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
  if (result.hit) {
    cout << "Parsing data row: " << endl << "\t" << str << endl;
    if (!result.full) {
      cout << "Incomplete data parse ...  Parsing stopped at  " ;
      char const* s = result.stop;
      int limit = 10;
      while(s && limit--)
	  cout << *s++;
      cout << endl;
    }
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
  else {
    double x;
    std::istringstream ss (str);
    ss >> x;
    return x;
  }
}
double
parse_double (std::vector<std::string> const& sVec, int position)
{
  return parse_double(sVec[position]);
}


class ParseDoubleAtPosition
{
private:
  int mPosition;
public:
  ParseDoubleAtPosition(int position) : mPosition(position) {}

  double operator()(std::vector<std::string> const& svec) { return parse_double(svec,mPosition); }
};

int
string_length (std::vector<std::string> const& sVec, int position)
{
  return sVec[position].size();
}

void
write_numerical_data_file (std::vector<std::string> const& varNames, StringDataMatrix const& data,
			   std::vector<int> const& varMissingCount, std::vector<int> const& varNumericCount,
			   std::ostream& output)
// writes columns in streaming fashion
// output will contain at least as many cols as input due to categorical and missing expansion
{
  int nVars (varNames.size());
  int nObs  (data.size());
  
  for (int column=0; column<nVars; ++column) {
    output << varNames[column] << endl;
    if (varNumericCount[column]+varMissingCount[column]==nObs) {
      // if none are missing and data is numerical, then copy numbers to output
      if (varMissingCount[column]==0) {
	for(int i=0; i<nObs; ++i)
	  output << data[i][column].c_str() << " ";
      // figure out the mean, insert into the numerical data, then write it along with missing indicator
      } else {
	double sum = std::accumulate(data.begin(), data.end(),
				     0.0,
				     ParseDoubleAtPosition(column)
				     );
	std::vector< int  > missingPos;
	for(int i=0; i<nObs; ++i) {
	  if (data[i][column].size() > 0) {
	    sum += parse_double(data[i][column]);
	  } else {
	    missingPos.push_back(i);
	  }
	}
	double mean (sum/(nObs-varMissingCount[column]));
	for(int i=0, j=0; i<nObs; ++i) {
	  if(missingPos[j]>i)
	    output << data[i][column].c_str() << " ";
	  else {
	    output << mean << " ";
	    ++j;
	  }
	}
	// write missing indicator
	output << endl << varNames[column] << "[missing]" << endl;
	for(int i = 0, j=0; i<nObs; ++i) {
	  if (missingPos[j]>i)     output << "0 " ;
	  else {
	    output << "1 ";
	    ++j;
	  }
	}
      }
      output << endl;
    }
    else { // its categorical
      // find the collection of unique values
      std::set< std::string > uniqueValues;
      for (int i=0; i<nObs; ++i) {
	if (data[i][column].size() > 0)
	  uniqueValues.insert(data[i][column]);  // not missing
      }
    }
  }
}
  
////////////////////////////////////////////////////////////////////////////////

int main ()

////////////////////////////////////////////////////////////////////////////////
{
  std::string inputLine;

  // parse names of variables from first input line
  std::vector< std::string > inputColumnNames;
  if (getline(std::cin, inputLine)) {
    if (parse_variable_names(inputLine.c_str(), StringCatcher( &inputColumnNames ) )) {
      cout <<  "\nParser: Read " << inputColumnNames.size() << " variable names from the input data.  These are:\n" << endl;
      for (std::vector<std::string>::iterator it = inputColumnNames.begin(); it != inputColumnNames.end(); ++it)  {
	cout << " |" << *it << "| " << endl;
      }
    }
    else cout << "Parser: Failed to parse CSV variable names.\n" << endl;
  }    
  else cout << "Parser: Not able to read input data for names.\n " << endl;
  cout << "\n\n-----------------------------------------------------------------\n\n" << endl;

  // set up vectors to count types of data in columns
  int nVars (inputColumnNames.size());
  std::vector< int > numeric;
  std::vector< int > missing;
  for (int i=0; i<nVars; ++i) {
    numeric.push_back(0);
    missing.push_back(0);
  }
  
  // iterate through remaining lines of data in order to build the matrix of strings
  StringDataMatrix dataMatrix;

  int lineNumber (0);
  while (getline(std::cin, inputLine)) {
    ++lineNumber;
    std::vector<std::string> inputData;
    if( parse_data_line(inputLine.c_str(), StringCatcher( &inputData ))) {
      cout <<  "\nData from the item parser:" << endl;
      for (std::vector<std::string>::iterator it = inputData.begin(); it != inputData.end(); ++it)  {
	cout << " |" << *it << "| " << endl;
      }
      if ((int)inputData.size() == nVars) {
	dataMatrix.push_back(inputData);
	for(int i=0; i<nVars; ++i) {
	  if(inputData[i].size() == 0)              ++missing[i];
	  else if (can_parse_number(inputData[i]))  ++numeric[i];
	}
	cout
	  << "Parser: Line " << lineNumber
	  << ". Number of data elements (" << inputData.size()
	  << ") unequal to number of variables (" << nVars << ").\n\t" << inputLine << endl;
      }
      else cout << "Parser: Line " << lineNumber << ". Failed to parse input CSV data.\n\t" << inputLine << endl;
    }
  }
  
  cout << "-----------------------------------------------------------------" << endl;
  cout << "Parser: Summary \n\t nObs = " << dataMatrix.size() << " with nVars = " << nVars << endl;
  cout << "       #Numeric: " ;
  for (int i = 0; i<nVars; ++i)
    cout << numeric[i] << " ";
  cout << "\n       #Missing: " ;
  for (int i = 0; i<nVars; ++i)
    cout << missing[i] << " ";
  
  write_numerical_data_file (inputColumnNames, dataMatrix, missing, numeric, std::cout);
  
  return 0;
}


