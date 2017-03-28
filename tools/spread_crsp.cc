/*
  A basic filter: reads from std input and writes to std output.

  Reads WRDS version of stock data assuming 4 tab-delimited columns

         PERMNO  DATE  Ticker Return

  Writes date x permno table with NAs

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
  int    numCompanies = 0;
  int    priorPermno = 0;
  string restOfLine;
  std::set<int> permnoSet;
  std::set<int> dateSet;

  std::map<int, string> tickerMap;
  std::map<std::pair<int,int>,float> table;

  std::clog << "Starting the spread..." << std::endl;
  
  getline(std::cin, restOfLine);   // toss first line with headers
  std::clog << messageTag << "Dumping first header line, with text '" << restOfLine << "' \n";
  while(! std::cin.eof() )
  { int permno;
    int date;
    string ticker;
    float retn;
    std::cin >> permno >> date;    // only permno and date seem to be reliably present
    std::getline(std::cin, restOfLine);
    size_t i0, i1;
    i0 = 1; i1 = restOfLine.find('\t',i0);
    ticker = restOfLine.substr(i0,i1);
    trim(ticker);
    // skip line if ticker missing
    if (ticker.size() > 0) {       
      if (permno != priorPermno) {
	++numCompanies;
	tickerMap[permno] = ticker;
	priorPermno = permno;
	permnoSet.insert(permno);
      }
      // get return from rest of line
      restOfLine = restOfLine.substr(i1+1, std::string::npos);
      try
      {
	retn = std::stof(restOfLine);
	dateSet.insert(date);
	table[std::make_pair(permno,date)] = retn;
      }
      catch (std::exception &e)
      { std::clog << "PARSE:  Exception for TICKER " << ticker << " Date " << date << "  " << e.what()
		  << " parsing `" << restOfLine << "'" << std::endl;
      }
    }
  }
  std::clog << messageTag << "Read data for " << numCompanies << " companies." << std::endl;
  // write the data, with names from pernos
  std::cout << "Date";
  for (auto const& p : permnoSet)
    std::cout << "\t" << tickerMap[p] << "_" << p;
  std::cout << std::endl;
  for (auto const& d : dateSet) {
    std::cout << d ;
    for (auto const& p : permnoSet) {
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
