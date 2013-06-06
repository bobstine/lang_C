/* -*-c++-*-

   RUN:  distance_matrix << stdin >> stdout

 Input...
   Standard n x c array of columns, with prefix first line giving n and p

   
 Output...
   Quadratic distance matrix, as for R's read.table function

*/

#include <cassert>

#include <string>
#include <iostream>
#include <fstream>
#include <sstream>

#include <Eigen/Core>

#include <vector>
#include <iterator>
#include <getopt.h>

#include "random.h"

///////////////////////////////////////////////////////////////////////////////

typedef Eigen::VectorXf Vector;
typedef Eigen::MatrixXf Matrix;

std::string messageTag ("DMTX: ");


int
distance_matrix(bool standardize, std::istream& input, std::ostream& output)
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

  float mean=0.0, sd=0.0;
  if(standardize)
  { std::clog << messageTag << "Standardizing columns." << std::endl;
    for (int j=0; j<nCols; ++j)
    { mean = X.col(j).sum()/nRows;
      X.col(j) = X.col(j).array() - mean;
      sd = sqrt(X.col(j).squaredNorm()/nRows);
      if(sd>0) X.col(j) = X.col(j).array() / sd;
    }
  }

  // allocate result, choose method
  Matrix D (nRows,nRows);
  for(int i=0; i<nRows; ++i)
    for (int j=i; j<nRows; ++j)
    { float dp = X.row(i).dot(X.row(j)) + 2.0;
      D(i,j) = dp * dp;
      D(j,i) = D(i,j);
    }

  // write y
  std::clog << messageTag << "Writing " << D.rows() << " rows and " << D.cols() << " columns to output." << std::endl;
  output << D << std::endl;

  return nRows;
}

////////////////////////////////////////////////////////////////////////////////

void
parse_arguments(int argc, char** argv, bool &standardize);

////////////////////////////////////////////////////////////////////////////////


int
main (int argc, char** argv)
{
  //  set default parameter values
  bool standardize           (true);
  //  bool quadratic             (true);

  parse_arguments(argc, argv, standardize);
  
  distance_matrix(standardize, std::cin, std::cout);
  
  return 0;
}




void
parse_arguments(int argc, char** argv, bool &standardize)
{
  int key;
  while (1)                                  // read until empty key causes break
    {
      int option_index = 0;
      static struct option long_options[] = {
	  {"standardize",       0, 0, 's'},  // no  arg, 
	  {0, 0, 0, 0}                       // terminator 
	};
	key = getopt_long (argc, argv, "s", long_options, &option_index);
	if (key == -1)
	  break;
	//	std::cout << "Option key " << char(key) << " with option_index " << option_index << std::endl;
	switch (key)
	  {
          case 's' :
	    {
	      standardize = true;
	      break;
	    }
	  default:
	    {
	      std::cout << "PARSE: Option not recognized; returning.\n";
	    }
	  } // switch
    }
}
