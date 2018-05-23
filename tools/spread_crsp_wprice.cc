/*
  A basic filter: reads from std input and writes to std output.

  Reads WRDS version of stock data assuming 6 tab-delimited columns

         PERMNO  DATE  Ticker Price Return Shares

  Price is in $ and shares in thousands.  Set threshold accordingly.
  
  Writes date x permno table with NAs if price exceeds input threshold to total
  company value.

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
  
  const float threshold = 10000;      // x 000$ for total value
  const bool verbose = false;
  
  std::map<int, string> tickerMap;
  std::map<std::pair<int,int>,float> table;

  std::clog << messageTag
	    << "Starting spread with threshod at ..." << threshold << std::endl;
  
  getline(std::cin, restOfLine);   // toss first line with headers
  std::clog << messageTag
	    << "Dumping first header line, with text '" << restOfLine << "' \n";
  while(! std::cin.eof() )
  { int permno;
    int date;
    string ticker;
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
      // get price, return, shares from rest of line; keep if total value above threshold
      restOfLine = restOfLine.substr(i1+1, std::string::npos);
      try
      {
	i0 = 0; i1 = restOfLine.find('\t');
	float price = std::stof(restOfLine.substr(i0,i1));
	i0 = i1+1; i1 =restOfLine.find('\t', i0);
	float retn  = std::stof(restOfLine.substr(i0,i1-i0));
	i0 = i1+1;
	float shares= std::stof(restOfLine.substr(i0,std::string::npos));
	dateSet.insert(date);
	if(price < 0) price = -price;
	float value = price * shares;
	if(threshold < value)    // add to output table
	  table[std::make_pair(permno,date)] = retn;
	else
	  if (verbose)
	    std::clog << messageTag
		      << "Value for TICKER " << ticker << " Date " << date << "   = " << value
		      << " ( = " << price << " * " << shares << ") < " << threshold << std::endl;
      }
      catch (std::exception &e)
      { std::clog << messageTag
		  << "Exception for TICKER " << ticker << " Date " << date << "  " << e.what()
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
