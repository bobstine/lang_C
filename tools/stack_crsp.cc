/*
  Converts std input to std output.

  Reads WRDS version of stock data with 4 columns, returning one line per firm

         PERMNO  DATE  ID  Return

  and writes as

         PERMNO FIRST_DATE LENGTH  RETURNS

*/

#include <iostream>
#include <vector>
#include <string>

using std::string;

int
main()
{
  string input;
  getline(std::cin, input);   // toss first line with headers
  std::clog << "Dumping first line, with text '" << input << "' \n";
  int    priorPermno = -1;
  int    permno      = -1;
  int    firstDate   = 0;
  string idCode;
  float  ret         = 0;
  string restOfLine;
  std::vector<float> returns;
  while(! std::cin.eof() )
  {    int date;
    std::cin >> permno >> date >> idCode;
    std::getline(std::cin, restOfLine);
    std::clog << permno << " " << date << " " << idCode << " " << restOfLine << std::endl;
    if (restOfLine.empty())
      ret = -666;
    else
      ret = atof(restOfLine.c_str());
    if (permno != priorPermno)
    { std::clog << "Starting to read data for new company " << permno << std::endl;
      if(!returns.empty())
      {	std::cout << priorPermno << "\t" << firstDate << "\t" << "\t" << returns.size();
	for (size_t i=0; i<returns.size(); ++i)
	  std::cout << "\t" << returns[i];
      std::cout << std::endl;
      }
      priorPermno = permno;
      firstDate = date;
      returns.clear();
    }
    returns.push_back(ret);
  }
  // write last one
  std::cout << permno << "\t" << firstDate << "\t" << "\t" << returns.size();
  for (size_t i=0; i<returns.size(); ++i)
    std::cout << "\t" << returns[i];
  std::cout << std::endl;
  
  return 0;
}
    
