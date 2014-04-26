/*
  A basic filter: reads from std input and writes to std output.

  Reads WRDS version of stock data with 5 columns

         PERMNO  DATE  Return  shares_outstanding  openPrice

  Writes one line per firm (only uses last 2 input columns for first company record)

         Number_of_returns  Opening_market_value PERMNO First_date Return_vector

*/

#include <iostream>
#include <vector>
#include <string>

using std::string;

float
get_float_from_string(std::string s, size_t i0, size_t i1, std::string label);
  
int
main()
{
  string messageTag = "STACK: ";
  string priorPermno, permno, firstDate ;
  int    numCompanies = 0;
  float  openPrice  = 0;
  float  ret        = 0;
  float  shareOut   = 0;
  string restOfLine;
  std::vector<float> returns;

  getline(std::cin, restOfLine);   // toss first line with headers
  std::clog << messageTag << "Dumping first line, with text '" << restOfLine << "' \n";
  while(! std::cin.eof() )
  { string date;
    std::cin >> permno >> date;
    std::getline(std::cin, restOfLine);
    // get items from rest of line
    size_t i0, i1;
    i0 = 1; i1 = restOfLine.find('\t',i0);
    ret = get_float_from_string(restOfLine, i0, i1, permno+"@"+date);
    if (permno != priorPermno)
    { ++numCompanies;
      std::clog << messageTag << "Starting to read data for company " << permno << std::endl;
      if(!returns.empty())
      {	std::cout << returns.size() << "\t" << openPrice * shareOut << "\t" << priorPermno << "\t" << firstDate;
	for (size_t i=0; i<returns.size(); ++i)
	  std::cout << "\t" << returns[i];
	std::cout << std::endl;
      }
      // read opening shares and price
      i0 = i1+1;
      i1 = restOfLine.find('\t',i0);
      shareOut = get_float_from_string (restOfLine, i0, i1, permno+"@"+date);
      openPrice = get_float_from_string (restOfLine, i1+1, std::string::npos, permno+"@"+date);
      priorPermno = permno;
      firstDate = date;
      returns.clear();
    }
    returns.push_back(ret);
  }
  // write last one
  std::cout << returns.size() << "\t" << openPrice * shareOut << "\t" <<  permno << "\t" << firstDate;
  for (size_t i=0; i<returns.size(); ++i)
    std::cout << "\t" << returns[i];
  std::cout << std::endl;
  // done
  std::clog << messageTag << "Read data for " << numCompanies << " companies." << std::endl;
  return 0;
}
    
float
get_float_from_string(std::string str, size_t i0, size_t i1, std::string label)
{
  float x;
  try
  {
    x = std::stof( str.substr(i0,i1) );
  }
  catch (std::exception &e)
  { std::clog << "PARSE: " << label << " Exception " << e.what() << " parsing `" << str << "'" << std::endl;
    x = 0;
  }
  return x;
}
  
