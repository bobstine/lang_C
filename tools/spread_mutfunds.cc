/*
  A basic filter: reads from std input and writes to std output.

  Reads WRDS version of CRSP mutual funds assuming 3 tab-delimited columns

         DATE  ID  Return

  Writes date x ID table with NAs

*/

#include <iostream>
#include <string>
#include <set>
#include <map>


#include <algorithm>
#include <functional>
#include <cctype>
#include <locale>

// trim from start (in place)
static inline void ltrim(std::string &s) {
  s.erase(s.begin(), std::find_if(s.begin(), s.end(),
				  std::not1(std::ptr_fun<int, int>(std::isspace))));
}

// trim from end (in place)
static inline void rtrim(std::string &s) {
  s.erase(std::find_if(s.rbegin(), s.rend(),
		       std::not1(std::ptr_fun<int, int>(std::isspace))).base(), s.end());
}

// trim from both ends (in place)
static inline void trim(std::string &s) {
  ltrim(s);
  rtrim(s);
}
/*
// trim from start (copying)
static inline std::string ltrimmed(std::string s) {
  ltrim(s);
  return s;
}

// trim from end (copying)
static inline std::string rtrimmed(std::string s) {
  rtrim(s);
  return s;
}

// trim from both ends (copying)
static inline std::string trimmed(std::string s) {
  trim(s);
  return s;
}
*/


using std::string;

float
get_float_from_string(std::string s, size_t i0, size_t i1, std::string label);
  
int
main()
{
  string messageTag = "SPREAD: ";
  int    numFunds = 0;
  int    priorId = 0;
  string restOfLine;
  std::set<int> idSet;
  std::set<int> dateSet;

  std::map<std::pair<int,int>,float> table;

  std::clog << "Starting the spread..." << std::endl;
  
  getline(std::cin, restOfLine);   // toss first line with headers
  std::clog << messageTag << "Dumping first header line, with text '" << restOfLine << "' \n";
  while(! std::cin.eof() )
  { int date;
    int id;
    float retn;
    std::cin >> date >> id;
    std::getline(std::cin, restOfLine);
    trim(restOfLine);
    // skip line if ticker missing
    if (restOfLine.size() > 0) {       
      if (id != priorId) {
	++numFunds;
	priorId = id;
	idSet.insert(id);
      }
      // get return from rest of line
      try
      {
	retn = std::stof(restOfLine);
	dateSet.insert(date);
	table[std::make_pair(id,date)] = retn;
      }
      catch (std::exception &e)
      { std::clog << "PARSE:  Exception for fund " << id << " Date " << date << "  " << e.what()
		  << " parsing `" << restOfLine << "'" << std::endl;
      }
    }
  }
  std::clog << messageTag << "Read data for " << numFunds << " funds." << std::endl;
  // write the data, with names from pernos
  std::cout << "Date";
  for (auto const& p : idSet)
    std::cout << "\t" << "fund_" << p;
  std::cout << std::endl;
  for (auto const& d : dateSet) {
    std::cout << d ;
    for (auto const& p : idSet) {
      auto ptr = table.find(std::make_pair(p,d));
      if (ptr != table.end())
	std::cout << "\t" << ptr->second;
      else
	std::cout << "\t" << "NA";
    }
    std::cout << std::endl;
  }
  return 0;
}
