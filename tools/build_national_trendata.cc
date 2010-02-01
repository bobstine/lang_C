/*
  Use this function to process the national TrenData files.  The input
  file named raw_data_1.csv is assumed to have a header line (which is
  skipped over) followed by lines with three values,

            quarter, var label, var value

  with each appearing in quotes.

  The output data table (national_trendata.csv) is tabular in a format suitable for
  reading into R for analysis. Data stored as a map of vectors. these vectors are columns
  written to csv file

  30 Jan 10 ... Created.

*/

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
#include <set>
#include <map>
#include <assert.h>

#include "read_utils.h"

int
row_number(std::string date)               // format is 1992.1, 1992.2, 1992.3, 1992.4, 1993.1, ...
{
  date[4] = ' ';
  int iyear   ;
  int quarter ;
  std::istringstream input(date);
  input >> iyear >> quarter;
  return 4*(iyear-1992)+quarter-1;
}


int
main()
{
  const int    num_quarters  (71);                              //   1992:1 .. 2009:3
  const double missing_value (-7.777777);
  
  std::string dataFileName ("raw_data_1.csv");
    
  typedef std::map<std::string, std::vector<double> > VariableMap;
  VariableMap data;
  std::set<std::string> varNames;
  int lineCount (0);
  
  std::cout << "Opening file " << dataFileName << " for reading..." << std::endl;
  std::ifstream dataFile (dataFileName.c_str());
  std::string header;
  getline(dataFile, header);                                   // dump header line
  std::cout << "Dumping header line: " << header << std::endl;
  while(!dataFile.eof())
  { // keep track of lines read
    ++lineCount;
    if (0 == lineCount % 1000) std::cout << "Line Count @ " << lineCount << std::endl;
    // read whole line from file
    std::string line;                
    getline(dataFile, line);
    // std::cout << "Read line " << line << std::endl;
    if(line.empty()) break;
    // process quoted tokens
    std::vector<std::string> strs (3);
    int s (0);
    std::string::const_iterator i (line.begin());
    if (*i != '"')
    { std::cerr << "Leading character is not a double quote; ending.\n";
      break;
    }
    ++i;                                                      // skip initial "
    while(s<3)
    { // std::cout << "top with *i = " << *i << std::endl;
      if (*i == '"')
      { ++s;
	if (s < 3)
	{ ++i;
	  assert (*i==',');
	  i = i+2;                                            // skip ,"
	}
      }
      else
      { //std::cout << "Pushing back " << *i << std::endl;
	strs[s].push_back(*i);
	++i;
      }
    }
    // convert to values
    // std::cout << "Converting: " << strs[0] << "  " << strs[1] << "  " << strs[2] << std::endl;
    int row = row_number(strs[0]);
    assert (row <= num_quarters);
    assert (0 <= row);
    double value   (read_utils::lexical_cast<double>(strs[2].c_str()));
    // store in map 
    if (varNames.find(strs[1]) == varNames.end())       // add vector initialized with missing value
    { std::cout << "Found new variable:  " << strs[1] << std::endl;
      varNames.insert(strs[1]);
      data[strs[1]] = std::vector<double>(num_quarters, missing_value);
    }
    // std::cout << "Insert " << value << " -> " << strs[1] << "[" << row << "]\n";
    data[strs[1]][row] = value;
  }
  std::cout << "Completed reading file: " << dataFileName
	    << ".  Read " << lineCount << " input lines with " << varNames.size() << " variables.\n";
  std::cout << "Preparing to write data to file national.td\n";
  // write data out, starting with var names
  std::ofstream output ("national.td");
  std::set<std::string>::const_iterator name (varNames.begin());
  output << *name;
  for(++name; name != varNames.end(); ++name)
    output << "\t" << *name;
  for(int q=0; q < num_quarters; ++q)
  { output << "\n";
    name = varNames.begin();
    output << data[*name][q];
    for(++name; name != varNames.end(); ++name)
    { if (data[*name][q] == missing_value)
	output << "\t NA";
      else
	output << "\t " << data[*name][q];
    }
  }
}

