/* -*-c++-*-

 RUN:  random_projection -f input.dat -o file -k num_pc -p power_iter -s standardize ( -q  if quadratic )


 Input...

   Standard n x c array of columns, with prefix first line giving n and p

   
 Output...

   n x k matrix of random projections of the c columns (p=1) or of the
   (c + c choose 2) quadratics alone if set quadratic option q (no arg).

   Reads the entire input array so that it can more efficiently produce
   the output without having to recycle through the random generation
   process over and over.  If -s option used, normalizes the columns to
   have length 1.

*/

#include <cassert>

#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <cstdio>
#include <time.h>

#include <Eigen/Core>
#include <Eigen/Dense>

#include <vector>
#include <iterator>
#include <algorithm>
#include <numeric>
#include <cmath>
#include <getopt.h>

#include "random.h"
#include "timing.h"

///////////////////////////////////////////////////////////////////////////////

typedef Eigen::VectorXf Vector;
typedef Eigen::MatrixXf Matrix;

std::string messageTag ("RPRJ: ");


// return number of obs, number of vars written
int
project(int nProjections, int powerIterations, bool standardize, bool quadratic, std::istream& input, std::ostream& output)
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
  std::clog << messageTag << "Centering input columns.\n";
  for (int j=0; j<nCols; ++j)
  { float mean (X.col(j).sum()/nRows);
    X.col(j) = X.col(j).array() - mean;
  }
  // optionally rescale X to norm 1
  if (standardize)
  { std::clog << messageTag << "Standarizing input columns to unit length.\n";
    for (int j=0; j<nCols; ++j)
    { if (X.col(j).squaredNorm()>0)
	X.col(j).normalize();
      else
	std::clog << "WARNING: Column " << j << " is constant (and so zeroed out)." <<std::endl;
    }
  }
  // allocate result, choose method
  Matrix Y (nRows,nProjections);     // U of SVD of reduced matrix
  Vector D (nProjections);           // Diagonal of singular values of reduced matrix
  if (! quadratic) // linear
  { std::clog << messageTag << "Computing left singular vectors of matrix by random projection";
    if (powerIterations) std::clog << " with power iterations.\n" ; else std::clog << ".\n";
    print_with_time_stamp("Starting base linear random projection", std::clog);
    Matrix localP = X * Matrix::Random(nCols, nProjections);         // local version to avoid later potential aliasing problem
    while (powerIterations--)
    { print_with_time_stamp("Performing X X' multiplication for power iteration", std::clog);
      Matrix R = X * (X.transpose() * localP);
      print_with_time_stamp("Performing Householder step of iterated random projection", std::clog);
      localP = Eigen::HouseholderQR<Matrix>(R).householderQ() * Matrix::Identity(localP.rows(),localP.cols());  // block fails; gets left P.cols()
    }
    std::clog << messageTag << "Checking norms after Householder; 0'0="
	      << localP.col(0).dot(localP.col(0)) << "   0'1=" << localP.col(0).dot(localP.col(1))
	      << "   1'1=" << localP.col(1).dot(localP.col(1)) << std::endl;
    Matrix rX = localP.transpose() * X;
    print_with_time_stamp("Computing SVD of reduced matrix", std::clog);
    Eigen::JacobiSVD<Matrix> svd(rX, Eigen::ComputeThinU|Eigen::ComputeThinV);
    Y = localP * svd.matrixU();                         // n x nProjections
    D = svd.singularValues();
    print_with_time_stamp("Completed random projection", std::clog);
  }
  else             // quadratic has not been revised for proper power iterations or subsequent SVD
  { Y.setZero();
    std::clog << messageTag << "Computing random projection of " << (nCols*(nCols+1))/2 << " quadratics (excludes linear)." << std::endl;
    print_with_time_stamp("Top, with sum over cols", std::clog);
    for (int j=0; j<nCols; ++j)
      for (int k=j; k<nCols; ++k)
      { Vector cp   = X.col(j).array() * X.col(k).array();
	Vector rand = Vector::Random(nProjections);
	for (int i=0; i<nProjections; ++i)   // does Y += cp * rand.transpose(), but faster this way
	{ Y.col(i) += cp * rand(i);
	  if (!std::isfinite(Y(0,i)))
	  { std::clog << "Not finite at j=" << j << " k=" << k << " i=" << i << " cp(0)=" << cp(0) << std::endl;
	    assert(false);
	  }
	}
      }
    print_with_time_stamp("Complete base projection", std::clog);
    if (powerIterations)
    { std::clog << messageTag << "Preparing for " << powerIterations << " quadratic power iterations." << std::endl;
      Matrix XXt = Matrix::Zero(X.rows(),X.rows());
      for (int j=0; j<nCols; ++j)
      {	for (int k=j; k<nCols; ++k)
	{ Vector cp = X.col(j).array() * X.col(k).array();
	  for (int i=0; i<cp.size(); ++i)   // string out XXt += cp * cp.transpose() for speed
	    XXt.col(i) += cp * cp(i);
	}
      }
      while (powerIterations--)
      	Y = XXt * Eigen::HouseholderQR<Matrix>(Y).householderQ();
      print_with_time_stamp("Complete power iterations", std::clog);
    }
  }
    
  // write diagonal singular values, then Y
  std::clog << messageTag << "Writing " << D.size() << " singular values to output.\n";
  output << D << std::endl;
  std::clog << messageTag << "Writing " << Y.rows() << " rows and " << Y.cols() << " columns to output." << std::endl;
  output << Y << std::endl;
  return Y.rows();
}

////////////////////////////////////////////////////////////////////////////////


void
parse_arguments(int argc, char** argv, int &projColumns, int &powerIterations, bool &standardize, bool &quadratic, std::string& inputFile, std::string& outputFile );


int
main (int argc, char** argv)
{
  //  set default parameter values
  int projColumns                (1);
  int powerIterations            (0);
  bool standardize           (false);
  bool quadratic             (false);
  std::string inputFileName     ("");
  std::string outputFileName    ("");
  
  // parse arguments from command line
  parse_arguments(argc, argv, projColumns, powerIterations, standardize, quadratic, inputFileName, outputFileName);
  std::clog << messageTag
	    << "random_project --dimension=" << projColumns << " --power_iterations=" << powerIterations
	    << " --standardize=" << standardize<< " --quadratic=" << quadratic 
	    << " --input-file=" << inputFileName << " --output-file=" << outputFileName << std::endl;

  // 4 call variations
  if (inputFileName.size() == 0)
  { if (outputFileName.size() == 0)
      project(projColumns, powerIterations, standardize, quadratic, std::cin, std::cout);                // A
    else
    { std::ofstream output (outputFileName.c_str());
      if (!output)
      { std::cerr << "CSVP: Error. Cannot open output file " << outputFileName << std::endl;
	return 1;
      }
      project(projColumns, powerIterations, standardize, quadratic, std::cin, output);                  // B
    }
  }
  else
  { std::ifstream input (inputFileName.c_str());
    if (!input)
    { std::cerr << "CSVP: Error. Cannot open input file " << inputFileName << std::endl;
      return 2;
    }
    if (outputFileName.size() == 0)
      project(projColumns, powerIterations, standardize, quadratic, input, std::cout);                   // C
    else
    { std::ofstream output (outputFileName.c_str());
      if (!output)
      { std::cerr << "CSVP: Error. Cannot open output file " << outputFileName << std::endl;
	return 3;
      }
      project(projColumns, powerIterations, standardize, quadratic, input, output);                      // D
    }
  }
  return 0;
}



void
parse_arguments(int argc, char** argv,
		int &nProjections, int &powerIterations, bool &standardize, bool &quadratic, std::string& inputFile, std::string& outputFile)
{
  int key;
  while (1)                                  // read until empty key causes break
    {
      int option_index = 0;
      static struct option long_options[] = {
	  {"input",             1, 0, 'f'},  // has arg,
	  {"output",            1, 0, 'o'},  // has arg,
	  {"dimension",         1, 0, 'd'},  // has arg,
	  {"power_iterations",  1, 0, 'p'},  // has arg,
	  {"standardize",       0, 0, 's'},  // no  arg, 
	  {"quadratic",         0, 0, 'q'},  // no  arg, 
	  {0, 0, 0, 0}                       // terminator 
	};
	key = getopt_long (argc, argv, "f:o:d:p:sq", long_options, &option_index);
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
	      outputFile = name;
	      break;
	    }
          case 'd' :
	    {
	      std::istringstream is(optarg);
	      is >> nProjections;
	      break;
	    }
          case 'p' :
	    {
	      std::istringstream is(optarg);
	      is >> powerIterations;
	      break;
	    }
          case 's' :
	    {
	      standardize = true;
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

