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

  4 Feb 10 ... Include FIPS code in output file.
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

#include "trendata.h"


int
main()
{
  const int number_of_fields (5);
  const int    iVarName   (1);
  const int    iValue     (2);
  const int    iState     (3);
  const int    iFIPS      (4);

  const std::string path ("/Users/bob/data/TrenData/state/");
  std::string dataFileName(path + "raw_data_1.csv");
  
  // state level variables that go back to 1992 and match those used nationally
  std::string nameString = std::string("ATABB ") +
                 "ATACLB "  + "ATAU "    + "ATNB12 "  + "ATNC "    + "ATNNB12 " + "ATNNC "   +
    "ATPB120M "+ "ATPB60M " + "ATPB90M " + "ATPNN "   + "ATTDB "   + "ATTDC "   + "ATABB "   +
    "BCNARB "  + "BCPB120M "+ "BCPB60M " + "BCPBA "   + "BINAC "   + "BINAIB "  + "BIPB120M "+
    "BIPB60M " + "BIPB90M " + "BRNAC "   + "BRNARB "  + "BRPB120M "+ "BRPB60M " + "BRPB90M " +
    "BRPBA "   + "FINAC "   + "FINAIB "  + "FIPB120M "+ "FIPB60M " + "FIPB90M " + "FRNAC "   +
    "FRNARB "  + "FRPB120M "+ "FRPB60M " + "FRPB90M " + "FRPBA "   + "INAB "    + "INDTD "   +
    "INNAC "   + "INNAIB "  + "INNNC "   + "INNNIB "  + "INPB120M "+ "INPB60M " + "INPB90M " +
    "INPBA "   + "INPN "    + "INPNI "   + "INTDB "   + "INTDC "   + "MTDTD "   + "MTNAB "   + 
    "MTNAC "   + "MTPB120M "+ "MTPB60M " + "MTPB90M " + "MTPBA "   + "MTTDB "   + "REAB "    +
    "REACLRB " + "REAU "    + "REDTD "   + "RENAC "   + "RENARB "  + "REPB60M " + "REPB120M "+
    "REPB90M " + "REPBA "   + "RETDB "   + "RETDC "   + "RTAB "    + "RTAU "    + "RTDTD "   +
    "RTNAC "   + "RTNARB "  + "RTPB120M "+ "RTPB60M " + "RTPB90M " + "RTPBA "   + "RTTDB "   +  "RTTDC";

  // read into separate strings
  std::set<std::string> variableNames;    // build state tables for these variables
  std::istringstream iss (nameString);
  while (iss)
  { std::string name;
    iss >> name;
    name = read_utils::trim(name);
    if (!name.empty())
      variableNames.insert(name);
  }
  std::cout << "Preparing to build state data for the following variables...\n";
  for (std::set<std::string>::const_iterator it = variableNames.begin(); it != variableNames.end(); ++it)
    std::cout << *it << " ";
  std::cout << std::endl;
  
  std::map<std::string, int> varValueCount;
  
  std::map<std::string, TimeSeriesMap>  data;
  for (std::set<std::string>::const_iterator v=variableNames.begin(); v != variableNames.end(); ++v)
  { TimeSeriesMap aMap;
    data[*v] = aMap;
  }

  std::map<std::string, int> fipsMap;
  std::set<std::string> states;

  int lineCount (0);
  std::cout << "Opening file " << dataFileName << " for reading..." << std::endl;
  std::ifstream dataFile (dataFileName.c_str());
  std::string header;
  getline(dataFile, header);                                   // dump header line

  while(!dataFile.eof())
  { ++lineCount;
    if (0 == lineCount % 25000) std::cout << "Line Count @ " << lineCount << std::endl;
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
    fipsMap[strs[iState]] = read_utils::lexical_cast<int>(strs[iFIPS]);
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
  { std::string fileName = path + "data/" + *v + ".state.td";
    std::cout << "Preparing to write data to " << fileName << std::endl;
    // write data out, starting with quarter labels
    std::ofstream output (fileName.c_str());
    output << "State\tFIPS";
    for (int q=0; q<number_of_quarters; ++q)
      output << "\t \"" << 1992 + ((int)(q/4)) << "." << (1+q%4) << "\"";
    output << std::endl;
    for(std::set<std::string>::const_iterator state=states.begin(); state != states.end(); ++state)
    { output << *state << "\t" << fipsMap[*state];
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

