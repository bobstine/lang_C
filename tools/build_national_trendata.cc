/*
  Use this function to process the national TrenData files.  Each of the two files
  named raw_data_1.csv and raw_data_2.csv is assumed to have a header line (which
  is skipped over) followed by lines with three values,
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
row_number(double dyear)               // format is 1992.1, 1992.2, 1992.3, 1992.4, 1993.1, ...
{
  int year (dyear);
  int quarter (10.0*(dyear-(double)year));
  return 4*(year-1992) + (quarter-1);
}


int
main()
{
  const int num_quarters (71);                              //   1992:1 .. 2009:3

  std::vector<std::string> dataFileNames;
  dataFileNames.push_back("raw_data_1.csv");
  //  dataFileNames.push_back("raw_data_2.csv");
    
  typedef std::map<std::string, std::vector<double> > VariableMap;
  VariableMap data;
  std::set<std::string> varNames;
  int lineCount (0);
  
  for(std::vector<std::string>::const_iterator f = dataFileNames.begin(); f != dataFileNames.end(); ++f)
  {
    std::cout << "Opening file " << *f << " for reading..." << std::endl;
    std::ifstream dataFile (f->c_str());
    std::string header;
    getline(dataFile, header);                                   // dump header line
    std::cout << "Dumping header line: " << header << std::endl;
    while(!dataFile.eof())
    { // keep track of lines read
      ++lineCount;
      std::cout << lineCount << std::endl;
      if (0 == lineCount % 100) std::cout << "Line Count @ " << lineCount << std::endl;
      // read whole line from file
      std::string line;                
      getline(dataFile, line);
      std::cout << "Read line " << line << std::endl;
      // process quoted tokens
      std::vector<std::string> strs (3);
      int s (0);
      std::string::const_iterator i (line.begin());
      assert(*i == '"');
      ++i;                                                      // skip initial "
      while(s<3)
      { std::cout << "top with *i = " << *i << std::endl;
	if (*i == '"')
	{ ++s;
	  std::cout << "        delimiter with s=" << s << std::endl;
	  if (s < 3)
	  { ++i;
	    std::cout << "Asserting comma\n";
	    assert (*i==',');
	    i = i+2;                                    // skip ,"
	  }
	}
	else
	{ std::cout << "Pushing back " << *i << std::endl;
	  strs[s].push_back(*i);
	  std::cout << "Incrementing\n";
	  ++i;
	}
      }
      // convert to values
      std::cout << "Converting: " << strs[0] << "  " << strs[1] << "  " << strs[2] << std::endl;
      double quarter (read_utils::lexical_cast<double>(strs[0].c_str()));
      int row = row_number(quarter);
      assert (row <= numQuarters);
      double value   (read_utils::lexical_cast<double>(strs[2].c_str()));
      std::cout << "Conversion done...inserting into row " << row << "\n";
      // store in map 
      if (varNames.find(strs[1]) == varNames.end())       // add new column;
      { std::cout << "New variable is " << strs[1] << std::endl;
	data[strs[1]] = std::vector<double>(num_quarters);
      }
      std::cout << "Doing insertion\n";
      data[strs[1]][row] = value;
    }
    std::cout << "Completed reading file: " << *f << std::endl;
  }
  std::cout << "Preparing to write data to file national.td\n";
  // write data out, starting with var names
  std::ofstream output ("national.td");
  for(std::set<std::string>::const_iterator i = varNames.begin(); i != varNames.end(); ++i)
    output << *i << "\t";
  for(std::set<std::string>::const_iterator i = varNames.begin(); i != varNames.end(); ++i)
  { output << std::endl;
    for(int q=0; q < num_quarters; ++q)
      output << data[*i][q] << "\t";
  }
}

