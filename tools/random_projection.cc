/* -*-c++-*-

   RUN:  random_projection -f input.dat -o file -k (# columns) -q


   Input...

   Standard n x c array of columns

   
   Output...

   n x k matrix of random projections of the c columns (p=1) or of the
   (c + c choose 2) if set the quadratic option q (no arg option).

   If the input array has many columns, then reads the entire input
   array so that it can more efficiently produce the output without
   having to recycle through the random generation process over and
   over.

*/

#include <cassert>

#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <cstdio>

#include <Eigen/Core>

#include <vector>
#include <iterator>
#include <algorithm>
#include <numeric>
#include <getopt.h>

#include "random.h"

///////////////////////////////////////////////////////////////////////////////

typedef Eigen::VectorXf Vector;
typedef Eigen::MatrixXf Matrix;

std::string messageTag ("RPRJ: ");

// return number of obs, number of vars written
int
project(int nProjections, bool quadratic, std::istream& input, std::ostream& output)
{
  // read nRows and nCols from input source
  int nRows, nCols;
  input >> nRows >> nCols;
  std::string inputLine;
  getline(input, inputLine);     // empty the input line after nRows nCols
  std::clog << messageTag << "Data table claims nRows (# obs) = " << nRows << " and nCols (# vars) = " << nCols << std::endl;
  
  // read input data table X
  Matrix X(nRows,nCols);
  for(int row=0; row<nRows; ++row)
  { for (int col=0; col<nCols; ++col)
      input >> X(row,col);
  }

  // center X
  for (int j=0; j<nCols; ++j)
  { float mean (X.col(j).sum()/nRows);
    X.col(j) = X.col(j).array() - mean;
    // std::cout << "Input mean " << j << " =" << mean << std::endl;
  }
  
  // make room for projections and form base linear component
  Matrix Y (nRows,nProjections);
  Matrix C = Matrix::Random(nCols,nProjections);
  Y = X * C;
  if (quadratic)
  { std::clog << messageTag << "Computing random projection with " << nCols + (nCols*(nCols-1))/2 << " quadratics." << std::endl;
    for (int j=0; j<nCols; ++j)
      for (int k=j; k<nCols; ++k)
      { Vector cp   = X.col(j).array() * X.col(k).array();
	Vector rand = Vector::Random(nProjections);
	for (int i=0; i<nProjections; ++i)
	  Y.col(i) += cp * rand(i);
      }
  }
    
  // write y back out
  output << Y << std::endl;

  return 0;
}

////////////////////////////////////////////////////////////////////////////////


void
parse_arguments(int argc, char** argv, int &projColumns, bool &quadratic, std::string& inputFile, std::string& outputFile );


int
main (int argc, char** argv)
{
  //  set default parameter values
  int projColumns                (1);
  bool quadratic             (false);
  std::string inputFileName     ("");
  std::string outputFileName    ("");
  
  // parse arguments from command line
  parse_arguments(argc, argv, projColumns, quadratic, inputFileName, outputFileName);
  std::clog << "MAIN: Arguments   --columns=" << projColumns << " --quadratic=" << quadratic
	    << " --input-file=" << inputFileName << " --output-file=" << outputFileName
	    << std::endl;

  // 4 call variations
  if (inputFileName.size() == 0)
  { if (outputFileName.size() == 0)
      project(projColumns, quadratic, std::cin, std::cout);                // A
    else
    { std::ofstream output (outputFileName.c_str());
      if (!output)
      { std::cerr << "CSVP: Error. Cannot open output file " << outputFileName << std::endl;
	return 1;
      }
      project(projColumns, quadratic, std::cin, output);                  // B
    }
  }
  else
  { std::ifstream input (inputFileName.c_str());
    if (!input)
    { std::cerr << "CSVP: Error. Cannot open input file " << inputFileName << std::endl;
      return 2;
    }
    if (outputFileName.size() == 0)
      project(projColumns, quadratic, input, std::cout);                   // C
    else
    { std::ofstream output (outputFileName.c_str());
      if (!output)
      { std::cerr << "CSVP: Error. Cannot open output file " << outputFileName << std::endl;
	return 3;
      }
      project(projColumns, quadratic, input, output);                      // D
    }
  }
  return 0;
}



void
parse_arguments(int argc, char** argv, int &nProjections, bool &quadratic, std::string& inputFile, std::string& outputFile)
{
  int key;
  while (1)                                  // read until empty key causes break
    {
      int option_index = 0;
      static struct option long_options[] = {
	  {"input",             1, 0, 'f'},  // has arg,
	  {"output",            1, 0, 'o'},  // has arg,
	  {"projections",       1, 0, 'p'},  // has arg,
	  {"quadratic",         0, 0, 'q'},  // no  arg, 
	  {0, 0, 0, 0}                       // terminator 
	};
	key = getopt_long (argc, argv, "f:o:p:q", long_options, &option_index);
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
          case 'p' :
	    {
	      std::istringstream is(optarg);
	      is >> nProjections;
	      break;
	    }
          case 'q' :
	    {
	      quadratic = true;
	      break;
	    }
	  }
    }
}
