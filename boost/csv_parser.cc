#include <string>
#include <iostream>
#include <cassert>
#include <vector>

#include <sstream>
#include <cstdio>


#include <boost/spirit/core.hpp>
#include <boost/spirit/utility/confix.hpp>
#include <boost/spirit/utility/lists.hpp>
#include <boost/spirit/utility/escape_char.hpp>

///////////////////////////////////////////////////////////////////////////////

using namespace std;
using namespace boost::spirit;

///////////////////////////////////////////////////////////////////////////////

class StringCatcher
{
  std::vector< std::string > mStrings;

  void operator()(std::string const& s) { mString.push_back(s); }
}

  
bool
parse_variable_names (char const* str, back_insert_iterator< std::vector<std::string> > const& inserter)
{
  class InsertOperator {
    typedef back_insert_iterator< std::vector<std::string> > Iter;
    InsertOperator (Iter 
    void operator()(std::string const& s) { *inserter = s; ++ inserter }
  }
  
  rule<phrase_scanner_t> name_rule, name_item, option_item;   // phrase_scanner type allows space_p in parser
  
  option_item = confix_p('[', *anychar_p , ']');               // bracketed options in var name

  name_item =
     (       
      (  alpha_p >> *(space_p | alnum_p) >> *option_item)      // char first in name without quotes
      |  confix_p('\"', *c_escape_ch_p, '\"')                  // string with quotes around it
     );
  
  name_rule =
    list_p(
	   name_item[inserter], 
	   ','
	   );

  

int main ()
{
  // Note: use of template parameter <phrase_scanner> allow for spaces in parsing


  // define csv grammar for parsing variable names
  // no missing names allowed; allow brackets as part of column name for options
  
  std::vector< std::string > inputColumnNames;


  // define csv grammar for parsing data
  
  rule<phrase_scanner_t> data_rule, data_item;

  data_item =
     (       
      (alpha_p >> *(space_p | alnum_p))                        // char first in name without quotes
      |   confix_p('\"', *c_escape_ch_p, '\"')                 // string with quotes around it
      |   longest_d[real_p | int_p]                            // numbers
      |   eps_p                                                // no missing values
      );
  
  std::vector< std::string > inputData;

  data_rule =
    list_p(
	   ! data_item[push_back_a(inputData)],     // ! matches 0 or 1 of the following  ... Why do we need it?
	   ','
	   );


  // open input file and read lines
  // next line is a test line
  //   char const *inputString = "\"string\",\"string with an embedded \\\"\"," "12345,spaced name, 0.12345e4,, 2 ,noSpace,";
  std::string inputLine;

  // parse names of variables first
  if (getline(std::cin, inputLine)) {
    parse_info<> result = parse(inputLine.c_str(), name_rule, space_p);   // binding 3rd argument produces a phrase scanner
    cout << "-----------------------------------------------------------------" << endl;
    if (result.hit) {
      cout
	<< "Parsing names: " << endl
	<< "\t" << inputLine << endl;
      if (!result.full) {
	cout << "Incomplete parse ...  Parsing stopped at  " ;
	char const* s = result.stop;
	int limit = 10;
	while(s && limit--)
	  cout << *s++;
	cout << endl;
      }
      cout <<  "\nVariable names from the item parser:" << endl;
      for (std::vector<std::string>::iterator it = inputColumnNames.begin(); it != inputColumnNames.end(); ++it)  {
	cout << " |" << *it << "| " << endl;
      }
    }
    else cout << "Failed to parse CSV list!" << endl;
  }

  // iterate through remaining lines of data
  while (getline(std::cin, inputLine)) {
    inputData.clear();
    parse_info<> result = parse(inputLine.c_str(), data_rule, space_p);
    cout << "-----------------------------------------------------------------" << endl;
    if (result.hit) {
      cout
	<< "Parsing data row: " << endl
	<< "\t" << inputLine << endl;
      if (!result.full) {
	cout << "Incomplete data parse ...  Parsing stopped at  " ;
	char const* s = result.stop;
	int limit = 10;
	while(s && limit--)
	  cout << *s++;
	cout << endl;
      }
      cout <<  "\nData from the item parser:" << endl;
      for (std::vector<std::string>::iterator it = inputData.begin(); it != inputData.end(); ++it)  {
	cout << " |" << *it << "| " << endl;
      }
    }
    else cout << "Failed to parse CSV list!" << endl;
  }
  
  cout << endl;


  return 0;
}


