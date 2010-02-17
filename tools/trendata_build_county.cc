/*
  Use this function to process county TrenData files. The relevant county data is
  in the files

         Year           Files
         1992-1996      raw_data.csv
	 1997-1998      raw_data_1.csv, raw_data_2.csv
	 1999-2009      raw_data_x.csv, x=1,2,3,4
	 
  Each is assumed to have a header line (which is skipped over) followed by
  lines with five values, such as the following.  Each value appears in quotes
  in a csv layout with no extraneous blanks.


         "2009.1","TMSTD","165.1729","Alabama","Autauga","01001"

         quarter, var label, var value, state name, county name, FIPS code


  For each chosen variable the program writes a tab-delimined output table with
  about 3140 lines, one for each county.  Each output data table
  (county_trendata_xxxx.csv) is tabular in a format suitable for reading into R
  for analysis. Within this C++ code, each variable is held as a named map of
  vectors. These vectors are columns written to csv file.

  16 Feb 10 ... Update to remove chars that were a problem in R.
   1 Feb 10 ... Created from national and state source code.

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
  const int number_of_fields (6);
  const int    iQuarter   (0);
  const int    iVarName   (1);
  const int    iValue     (2);
  const int    iState     (3);
  const int    iCounty    (4);
  const int    iFIPS      (5);
  
  std::vector<std::string> dataFileNames;
  dataFileNames.push_back("1992/raw_data.csv");
  dataFileNames.push_back("1993/raw_data.csv");
  dataFileNames.push_back("1994/raw_data.csv");
  dataFileNames.push_back("1995/raw_data.csv");
  dataFileNames.push_back("1996/raw_data.csv");
  dataFileNames.push_back("1997/raw_data_1.csv");
  dataFileNames.push_back("1997/raw_data_2.csv");
  dataFileNames.push_back("1998/raw_data_1.csv");
  dataFileNames.push_back("1998/raw_data_2.csv");
  dataFileNames.push_back("1999/raw_data_1.csv");
  dataFileNames.push_back("1999/raw_data_2.csv");
  dataFileNames.push_back("1999/raw_data_3.csv");
  dataFileNames.push_back("1999/raw_data_4.csv");
  dataFileNames.push_back("2000/raw_data_1.csv");
  dataFileNames.push_back("2000/raw_data_2.csv");
  dataFileNames.push_back("2000/raw_data_3.csv");
  dataFileNames.push_back("2000/raw_data_4.csv");
  dataFileNames.push_back("2001/raw_data_1.csv");
  dataFileNames.push_back("2001/raw_data_2.csv");
  dataFileNames.push_back("2001/raw_data_3.csv");
  dataFileNames.push_back("2001/raw_data_4.csv");
  dataFileNames.push_back("2002/raw_data_1.csv");
  dataFileNames.push_back("2002/raw_data_2.csv");
  dataFileNames.push_back("2002/raw_data_3.csv");
  dataFileNames.push_back("2002/raw_data_4.csv");
  dataFileNames.push_back("2003/raw_data_1.csv");
  dataFileNames.push_back("2003/raw_data_2.csv");
  dataFileNames.push_back("2003/raw_data_3.csv");
  dataFileNames.push_back("2003/raw_data_4.csv");
  dataFileNames.push_back("2004/raw_data_1.csv");
  dataFileNames.push_back("2004/raw_data_2.csv");
  dataFileNames.push_back("2004/raw_data_3.csv");
  dataFileNames.push_back("2004/raw_data_4.csv");
  dataFileNames.push_back("2005/raw_data_1.csv");
  dataFileNames.push_back("2005/raw_data_2.csv");
  dataFileNames.push_back("2005/raw_data_3.csv");
  dataFileNames.push_back("2005/raw_data_4.csv");
  dataFileNames.push_back("2006/raw_data_1.csv");
  dataFileNames.push_back("2006/raw_data_2.csv");
  dataFileNames.push_back("2006/raw_data_3.csv");
  dataFileNames.push_back("2006/raw_data_4.csv");
  dataFileNames.push_back("2007/raw_data_1.csv");
  dataFileNames.push_back("2007/raw_data_2.csv");
  dataFileNames.push_back("2007/raw_data_3.csv");
  dataFileNames.push_back("2007/raw_data_4.csv");
  dataFileNames.push_back("2008/raw_data_1.csv");
  dataFileNames.push_back("2008/raw_data_2.csv");
  dataFileNames.push_back("2008/raw_data_3.csv");
  dataFileNames.push_back("2008/raw_data_4.csv");
  dataFileNames.push_back("2009/raw_data_1.csv");
  dataFileNames.push_back("2009/raw_data_2.csv");
  dataFileNames.push_back("2009/raw_data_3.csv");
  dataFileNames.push_back("2009/raw_data_4.csv");

  std::set<std::string> variableNames;                         // build state tables for these variables
  variableNames.insert("REPB60M");        // 60+ days past due, revolving
  variableNames.insert("INPB60M");        // installment
  variableNames.insert("MTPB60M");        // mortgage
  variableNames.insert("ATTDC");          // total debt per consumer
  variableNames.insert("RENARB");         // revolving per borrower
  variableNames.insert("REAU");           // utilization of revolving
  variableNames.insert("MTDTD");          // mortgage as percentage of total debt
  variableNames.insert("MTNAB");          // mortgages per borrower
  
  std::map<std::string, int> varValueCount;  // records total values read per variable name
  
  std::map<std::string, TimeSeriesMap>  data;
  for (std::set<std::string>::const_iterator v=variableNames.begin(); v != variableNames.end(); ++v)
  { TimeSeriesMap aMap;
    data[*v] = aMap;
  }

  std::map<std::string, int> fipsMap;
  std::set<std::string> counties;
  
  for (std::vector<std::string>::const_iterator fileName = dataFileNames.begin(); fileName != dataFileNames.end(); ++fileName)
  { counties.clear();
    int lineCount (0);
    std::cout << "Opening file " << *fileName << " for reading..." << std::endl << "         ";
    std::ifstream dataFile (fileName->c_str());
    std::string header;
    getline(dataFile, header);                                   // dump header line
    
    while(!dataFile.eof())
    { ++lineCount;
      if (0 == lineCount % 100000) std::cout << " " << lineCount/1000;
      std::string line;                
      getline(dataFile, line);
      // std::cout << "Read line " << line << std::endl;
      if (line.empty()) break;
      if (line[0] != '"')
      { std::cerr << "   ***  Leading character is not a double quote; ending.  ***\n";
	break;
      }
      // process quoted tokens
      std::vector<std::string> strs (number_of_fields);
      parse_line(line, strs);
      // check date
      int q = quarter_number(strs[iQuarter]);
      assert (q <= number_of_quarters);
      assert (0 <= q);
      // store value if find sought variable
      if (variableNames.find(strs[iVarName]) != variableNames.end())
      { // keep track of found county names
	std::string county (strs[iCounty]);
	// remove embedded ' symbol, embedded blank in 'Mc Cullugh' type names
	unsigned int pos;
	pos = county.find("'");
	if(pos < county.size()) county.erase(pos,1);
	pos = county.find("Mc ");
	if(pos < county.size()-2) county.erase(pos+2,1);
	// combine state with county
	county = strs[iState] + "," + county;
	fipsMap[county] = read_utils::lexical_cast<int>(strs[iFIPS]);
	counties.insert(county);
	++varValueCount[strs[iVarName]];
	insert_value(data[strs[iVarName]], county, q, strs[iValue]);
      }
    }
    std::cout << " Read complete: " << lineCount << " lines, " << counties.size() << " counties.\n";
  }
  std::cout << "\n\nCompleted reading all " << dataFileNames.size() << " data files:\n";
  for(std::map<std::string, int>::const_iterator i=varValueCount.begin(); i != varValueCount.end(); ++i)
    std::cout << "        " << i->first << "  " << i->second << std::endl;
  // write separate table for each variable
  for(std::set<std::string>::const_iterator v = variableNames.begin(); v != variableNames.end(); ++v)
  { std::string fileName = *v + ".county.td";
    std::cout << "Writing data to " << fileName << std::endl;
    // write data out, starting with quarter labels
    std::ofstream output (fileName.c_str());
    output << "County\tFIPS";
    for (int q=0; q<number_of_quarters; ++q)
      output << "\t" << 1992 + ((int)(q/4)) << "." << (1+q%4);
    output << std::endl;
    for(std::set<std::string>::const_iterator county=counties.begin(); county != counties.end(); ++county)
    { output << *county << "\t" << fipsMap[*county];
      for (int q=0; q<number_of_quarters; ++q)
      { double value (data[*v][*county][q]);
	if (value == missing_value)
	  output << "\tNA";
	else
	  output << "\t" << value;
      }
      output << std::endl;
    }
  }
}

