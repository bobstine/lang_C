/* -*-c++-*-

   MAKE:     make insert_column

   EXECUTE:  insert_column -r correlation -j index -o output.dat -f input.dat


   Input
   
   The format of the input should be that produced by the csv_parser:
   a sequence of variables written as name followed on the next line
   by data. The first line of the file must give the number of
   observations (all numerical) and the number of variables in the
   file (again, as produced by csv_parser).

   
   Output

   Echos the input file except that it builds a column that has
   correlation (on average) r with the first column of the input file
   (which is Y). It writes this column as the ith explanatory variable
   in the output file.
   

   26 Feb 09 ... Created for testing auction code in simulations.

*/

#include <cassert>

#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <cstdio>

#include <vector>
#include <set>

#include <iterator>
#include <algorithm>
#include <numeric>
#include <getopt.h>

#include "range_ops.h"
#include "range_stats.h"
#include "random.h"

///////////////////////////////////////////////////////////////////////////////



// return number of obs, number of vars written
std::pair<int, int>
insert_column(double corr, int index, std::istream& input, std::ostream& output)
{
  // read nRows and nCols from input source
  int nRows, nCols;

  input >> nRows >> nCols;
  std::clog << "TEST: read nRows = " << nRows << " and nCols " << nCols << std::endl;
  
  // read from input into this string and numbers into this vector
  std::string inputLine;
  std::vector< double > y (nRows);
  std::vector< double > noise (nRows);
  
  // read the response
  std::string responseName;
  double yBar = 0.0;
  getline(input, responseName);
  std::cout << "INSC: Response variable is " << responseName << std::endl;
  getline(input, inputLine);
  std::stringstream ss(inputLine);
  for(int i=0; i<nRows; ++i)
  { ss >> y[i];
    yBar += y[i];
  }
  yBar = yBar/nRows;
  double sd = range_stats::standard_deviation(make_range(y),yBar,nRows-1);

  // generate new column with corr with y
  RandomGenerator rand(253);
  double r2 = corr * corr;
  double b = sd * sqrt((1-r2)/r2);
  for(int i=0; i<nRows; ++i)
    noise[i] = y[i] + b * rand.normal();

  // copy over columns until hit position of new one
  output << nRows << " " << nCols+1 << std::endl;  // add another column
  // write y back out
  output << responseName << std::endl;
  std::copy(y.begin(), y.end(), std::ostream_iterator<double>(output));
  output << std::endl;
  // insert new column into data
  unsigned int atVariable = 1;
  while (getline(input, inputLine))
    { if ((int)atVariable == index) // insert added column into output stream
    { output << "CorrelatedVariable\n";
      std::copy(noise.begin(), noise.end(), std::ostream_iterator<double> (output));
      output << std::endl;
    }
    output << inputLine;
    getline(input, inputLine);
    output << inputLine;
    ++atVariable;
  }
  std::cout << "INSC: Wrote expanded file with total of " << atVariable+2 << " " << nCols+1 << " columns to output.\n";
  return std::make_pair(nRows, nCols+1);
}

////////////////////////////////////////////////////////////////////////////////


void
parse_arguments(int argc, char** argv, double &corr, int &index, std::string& inputFile, std::string& outputFile );


int
main (int argc, char** argv)
{
  //  set default parameter values
  std::string inputFileName     ("");
  std::string outputFileName    ("");
  int index = 1;
  double corr = 0.0;
  
  // parse arguments from command line
  parse_arguments(argc, argv, corr, index, inputFileName, outputFileName);
  std::clog << "INSC: Arguments   --corr=" << corr << " --index=" << index 
	    << " --input-file=" << inputFileName << " --output-file=" << outputFileName
	    << std::endl;

  // 4 call variations
  if (inputFileName.size() == 0)
  { if (outputFileName.size() == 0)
      insert_column(corr, index, std::cin, std::cout);                // A
    else
    { std::ofstream output (outputFileName.c_str());
      if (!output)
      { std::cerr << "CSVP: Error. Cannot open output file " << outputFileName << std::endl;
	return 1;
      }
      insert_column(corr, index, std::cin, output);                  // B
    }
  }
  else
  { std::ifstream input (inputFileName.c_str());
    if (!input)
    { std::cerr << "CSVP: Error. Cannot open input file " << inputFileName << std::endl;
      return 2;
    }
    if (outputFileName.size() == 0)
      insert_column(corr, index, input, std::cout);                   // C
    else
    { std::ofstream output (outputFileName.c_str());
      if (!output)
      { std::cerr << "CSVP: Error. Cannot open output file " << outputFileName << std::endl;
	return 3;
      }
      insert_column(corr, index, input, output);                      // D
    }
  }
  return 0;
}



void
parse_arguments(int argc, char** argv, double &corr, int &index, std::string& inputFile, std::string& outputFile)
{
  int key;
  while (1)                                  // read until empty key causes break
    {
      int option_index = 0;
      static struct option long_options[] = {
	  {"input-file",        1, 0, 'f'},  // has arg,
	  {"output-file",       1, 0, 'o'},  // has arg,
	  {"corr",              1, 0, 'r'},  // has arg,
	  {"index",             1, 0, 'j'},  // has arg,
	  {"help",              0, 0, 'h'},  // no  arg, 
	  {0, 0, 0, 0}                       // terminator 
	};
	key = getopt_long (argc, argv, "f:o:r:j:h", long_options, &option_index);
	if (key == -1)
	  break;
	//	std::cout << "Option key " << char(key) << " with option_index " << option_index << std::endl;
	switch (key)
	  {
	  case 'f' :                                    
	    {
	      std::string name(optarg);
	      inputFile = name;
	      break;
	    }
	  case 'o' :  
	    {
	      std::string name(optarg);
	      outputFile = optarg;
	      break;
	    }
          case 'r' :
	    {
	      std::istringstream is(optarg);
	      is >> corr;
	      break;
	    }
          case 'j' :
	    {
	      std::istringstream is(optarg);
	      is >> index;
	      break;
	    }
	  case 'h' :
	    {
	      std::cout << "switches:" << std::endl << std::endl;
	      std::cout << "      --input-file=foo       input file" << std::endl;
	      std::cout << "      -ifoo" << std::endl << std::endl;
	      std::cout << "      --output-file=out      output file" << std::endl;
	      std::cout << "      -oout" << std::endl << std::endl;
	      std::cout << "      --corr=#      correlation with response" << std::endl;
	      std::cout << "      -r#" << std::endl << std::endl;
	      std::cout << "      --index=#     position in output" << std::endl;
	      std::cout << "      -j#" << std::endl << std::endl;
	      std::cout << "      --help      generates this message" << std::endl;
	      std::cout << "      -h" << std::endl << std::endl;
	      exit(0);
	      break;
	    }
	  }
    }
}
