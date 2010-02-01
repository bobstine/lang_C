/*
  Use this function to process the state TrenData files. All of the relevant state data is
  in the file named raw_data_1.csv which is is assumed to have a header line (which
  is skipped over) followed by lines with five values,

         quarter, var label, var value, state name, state code

  with each value appearing in quotes in a csv layout.

  For each chosen variable the program writes a tab-delimined output table.

  Each output data table (state_trendata_xxxx.csv) is tabular in a format suitable for
  reading into R for analysis. Data stored as a map of vectors. these vectors are columns
  written to csv file.  

   1 Feb 10 ... Created from national source.

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


typedef std::map<std::string, std::vector<double> > TimeSeriesMap;   // state x time series

const int    number_of_quarters  (71);                               //   1992:1 .. 2009:3
const double missing_value (-7.777777);



int
quarter_number(std::string date)               // format is 1992.1, 1992.2, 1992.3, 1992.4, 1993.1, ...
{
  date[4] = ' ';
  int iyear   ;
  int quarter ;
  std::istringstream input(date);
  input >> iyear >> quarter;
  return 4*(iyear-1992)+quarter-1;
}

void
parse_line(std::string line, std::vector<std::string> & strs)
{
  int nFields (strs.size());
  int s (0);
  std::string::const_iterator i (line.begin());
  assert (*i == '"');
  ++i;                                                      // skip initial "
  while(s<nFields)
  { // std::cout << "top with *i = " << *i << std::endl;
    if (*i == '"')
    { ++s;
      if (s < nFields)
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
}

void
insert_value(TimeSeriesMap &m, std::string state, int quarter, std::string value)
{
  // std::cout << "Inserting " << value << " into time series for " << state << " @ q= " << quarter << std::endl;
  if (m.find(state) == m.end()) // insert empty vector
  { // std::cout << "   initialize vector for state " << state << std::endl;
    m[state] = std::vector<double>(number_of_quarters, missing_value);
  }
  m[state][quarter] = read_utils::lexical_cast<double>(value.c_str());
}


int
main()
{
  const int    iVarName   (1);
  const int    iValue     (2);
  const int    iState     (3);
  
  std::string dataFileName("raw_data_1.csv");
  std::set<std::string> states;
  
  std::set<std::string> variableNames;                         // build state tables for these variables
  variableNames.insert("BCNARB");
  variableNames.insert("BCPB60M");
  variableNames.insert("BIPB60M");
  variableNames.insert("MTPB60M");
  variableNames.insert("REAU");
  variableNames.insert("RTTDB");
  variableNames.insert("TMMEAN");

  std::map<std::string, int> varValueCount;
  
  std::map<std::string, TimeSeriesMap>  data;
  for (std::set<std::string>::const_iterator v=variableNames.begin(); v != variableNames.end(); ++v)
  { TimeSeriesMap aMap;
    data[*v] = aMap;
  }

  int lineCount (0);
  std::cout << "Opening file " << dataFileName << " for reading..." << std::endl;
  std::ifstream dataFile (dataFileName.c_str());
  std::string header;
  getline(dataFile, header);                                   // dump header line

  const int number_of_fields (5);
  while(!dataFile.eof())
  { ++lineCount;
    if (0 == lineCount % 10000) std::cout << "Line Count @ " << lineCount << std::endl;
    std::string line;                
    getline(dataFile, line);
    // std::cout << "Read line " << line << std::endl;
    if (line.empty()) break;
    if (line[0] != '"')
    { std::cerr << "Leading character is not a double quote; ending.\n";
      break;
    }
    // process quoted tokens
    std::vector<std::string> strs (number_of_fields);
    parse_line(line, strs);
    // check date
    int q = quarter_number(strs[0]);
    assert (q <= number_of_quarters);
    assert (0 <= q);
    // keep track of found states
    states.insert(strs[iState]);
    // store value if find sought variable
    if (variableNames.find(strs[iVarName]) != variableNames.end())
    { ++varValueCount[strs[iVarName]];
      insert_value(data[strs[iVarName]], strs[iState], q, strs[iValue]);
    }
  }
  // summarize read and field counts
  std::cout << "Completed reading file: " << dataFileName
	    << ".  Read " << lineCount << " input lines with " << states.size() << " states.\n";
  for(std::map<std::string, int>::const_iterator i=varValueCount.begin(); i != varValueCount.end(); ++i)
    std::cout << "        " << i->first << "  " << i->second << std::endl;
  // write separate table for each variable
  for(std::set<std::string>::const_iterator v = variableNames.begin(); v != variableNames.end(); ++v)
  { std::string fileName = *v + ".state.td";
    std::cout << "Preparing to write data to " << fileName << std::endl;
    // write data out, starting with quarter labels
    std::ofstream output (fileName.c_str());
    output << "State";
    for (int q=0; q<number_of_quarters; ++q)
      output << "\t" << 1992 + ((int)(q/4)) << "." << (1+q%4);
    output << std::endl;
    for(std::set<std::string>::const_iterator state=states.begin(); state != states.end(); ++state)
    { output << *state;
      for (int q=0; q<number_of_quarters; ++q)
      { double value (data[*v][*state][q]);
	if (value == missing_value)
	  output << "\t NA";
	else
	  output << "\t " << value;
      }
      output << std::endl;
    }
  }
}

