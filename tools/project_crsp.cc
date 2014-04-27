/* -*-c++-*-

 RUN:  project_crsp -f input.file -o output.prefix -k num_pc -p power_iter -w 


 Input...

   Expects output as produced by running the filter stack_crsp, with

   #rows (constant), value (used as weight), idnum, date, returns... (of length #rows)
     
 Output...

   Pads the output file name with _sv.txt and _u.txt for singular values and u matrix;
   adds a _w_ if the analysis is weighted.
   
   n x k matrix of random projections of the optionally weighted

   If the w option is used, then the weights are used to weight the returns prior to
   forming the random projection (as in a value-weighted portfolio).

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

#include "file_utils.h"
#include "random.h"
#include "timing.h"

using std::string;

///////////////////////////////////////////////////////////////////////////////

typedef Eigen::VectorXd Vector;    // float will be faster/less memory, but need double for dynamic
typedef Eigen::MatrixXd Matrix;    // range when doing the analysis with weights; SV is all nan if float.


string messageTag ("CRSP: ");



void
parse_arguments(int argc, char** argv, int &projColumns, int &powerIterations, bool &weight,
		string& inputFile, string& outputFile );

std::pair<int,int>
get_dimensions(string fileName);


  
////////////////////////////////////////////////////////////////////////////////

int
main (int argc, char** argv)
{
  //  set default parameter values
  int nProjections          (1);
  int powerIterations       (0);
  bool weightCols           (false);
  string inputFileName      ("");
  string outputFilePrefix   ("");
  
  // parse arguments from command line
  parse_arguments(argc, argv, nProjections, powerIterations, weightCols, inputFileName, outputFilePrefix);
  std::clog << messageTag
	    << "random_project --dimension=" << nProjections << " --power_iterations=" << powerIterations << " --weight=" << weightCols
	    << " --input-file=" << inputFileName << " --output-file=" << outputFilePrefix << std::endl;

  // get sizes
  std::pair<int,int> dim = get_dimensions(inputFileName);
  std::clog << messageTag << "Reading input data for complete companies with " << dim.first << " rows and " << dim.second << " cols." << std::endl;
  
  // read data into weight vector and data matrix
  float weight;
  Matrix X (dim.first, dim.second);
  std::ifstream input (inputFileName.c_str());
  if (weightCols) std::clog << messageTag << "Using weights... " ;
  for (int col=0; col<dim.second; ++col)                   // file is column major layout
  { int nRow;
    string id, date;
    input >> nRow >> weight >> id >> date;                 // special parse for first four colums
    if (weightCols)
    { std::clog << " " << weight;
      for(int row=0; row<dim.first; ++row)
      { float x;
	input >> x;
	X(row,col) = weight * x;
      }
    }
    else
    { for(int row=0; row<dim.first; ++row)
	input >> X(row,col);
    }
  }
  if (weightCols) std::clog << std::endl;
  std::clog << messageTag << "Top left corner of X " << std::endl
	    << X.topLeftCorner(10,10)<< std::endl;
  Vector sums = X.colwise().sum();
  std::clog << messageTag << "Average returns for leading columns" << std::endl
	    << (sums.head(10)/dim.first) << std::endl;
  
  // compute projection
  Matrix U (dim.first,nProjections);     // U of SVD of reduced matrix
  Vector D (nProjections);           // Diagonal of singular values of reduced matrix
  {
    std::clog << messageTag << "Computing left singular vectors of matrix by random projection";
    if (powerIterations) std::clog << " with power iterations.\n" ; else std::clog << ".\n";
    print_with_time_stamp("Starting base linear random projection", std::clog);
    Matrix localP = X * Matrix::Random(X.cols(), nProjections);         // local version to avoid later potential aliasing problem
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
    U = localP * svd.matrixU();                         // n x nProjections
    D = svd.singularValues();
    print_with_time_stamp("Completed random projection", std::clog);
  }
    
  // write diagonal singular values, then U
  if (weightCols) outputFilePrefix += "_w";
  {
    string fileName = outputFilePrefix + "_sv.txt";
    std::ofstream output (fileName.c_str());
    std::clog << messageTag << "Writing " << D.size() << " singular values to output.\n";
    output << D.transpose() << std::endl;
  }
  {
    string fileName = outputFilePrefix + "_u.txt";
    std::ofstream output (fileName.c_str());
    std::clog << messageTag << "Writing " << U.rows() << " rows and " << U.cols() << " columns to output." << std::endl;
    output << U << std::endl;
  }
  return 0;
}

//
//     parse     parse     parse     parse     parse     parse     parse     parse     parse     parse     parse
//

void
parse_arguments(int argc, char** argv,
		int &nProjections, int &powerIterations, bool &weight, string& inputFile, string& outputFile)
{
  int key;
  while (1)                                  // read until empty key causes break
    {
      int option_index = 0;
      static struct option long_options[] = {
	  {"input",             1, 0, 'f'},  // has arg,
	  {"output",            1, 0, 'o'},  // has arg,
	  {"projections",       1, 0, 'k'},  // has arg,
	  {"power_iterations",  1, 0, 'p'},  // has arg,
	  {"weight",            0, 0, 'w'},  // no  arg, 
	  {0, 0, 0, 0}                       // terminator 
	};
	key = getopt_long (argc, argv, "f:o:k:p:w", long_options, &option_index);
	if (key == -1)
	  break;
	//	std::cout << "Option key " << char(key) << " with option_index " << option_index << std::endl;
	switch (key)
	  {
	  case 'f' :                                    
	    {
	      string name(optarg);
	      inputFile = name;
	      break;
	    }
	  case 'o' :  
	    {
	      string name(optarg);
	      outputFile = name;
	      break;
	    }
          case 'k' :
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
          case 'w' :
	    {
	      weight = true;
	      break;
	    }
	  default:
	    {
	      std::cerr << "*****  Invalid option choice " << key << " given." << std::endl;
	      break;
	    }
	  }
    }
}

std::pair<int,int>
get_dimensions(string fileName)
{
  int nCols = FileUtils::count_lines(fileName);
  std::ifstream input (fileName.c_str());
  if (!input)
  { std::cerr << " *** Error. Cannot open input file " << fileName << std::endl;
    return std::make_pair(0,0);
  }
  int nRows;
  input >> nRows;
  return std::make_pair(nRows,nCols);  
}

