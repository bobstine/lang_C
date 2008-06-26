/*  $Id: seq_regr_cmd.cc,v 1.1 2003/12/05 21:40:56 bob Exp $

  Driver for the sequential regression program.  From the command line
  the optional options are -i, -o (defaults input.dat, output.dat)

     seq_regr.exec -i input.file -o output.file
    
  Input file format: Last line is a 'q' to stop the reader loop.  A p
  gets the prediction SS for a new copy of the y variable (one that
  shares the same X structure as the Y that is being used for
  estimation).

     n
     y_1 y_2 y_3 ... y_n
     x
     x_1 x_2 x_3 ... x_n
     x
     z_1 z_2 z_3 ... z_n
     ...
     p
     y*_1 y*_2 .... y*_n
     q

  Output file format: binary indicators for whether predictor was added.

     0 1 0 1 0 ... prss
  
*/

#include "model.h"
#include "print_utils.h"

#include <vector>
#include <iostream>
#include <fstream>
#include <sstream>


void
parse_arguments(int argc, char** argv,
		std::string& inputFileName,
		std::string& outputFileName)
{
  // Set default values
  inputFileName  = "input.dat";
  outputFileName = "output.dat";

  // Reading in the command line parameters
  for (int i=1; i<argc; i=i+2)
  {
    char key(argv[i][0]);
    if ('-' == key)
    { 
      switch (argv[i][1])
      {
      case 'i' : { inputFileName  = argv[i+1]; break; }
      case 'o' : { outputFileName = argv[i+1]; break; }
      default  : { std::clog << "Option " << argv[i][0] << " not recognized." << std::endl; break;}
      }
    }
  }
}

  

int main(int argc, char** argv)
{
  std::string inputFileName;
  std::string outputFileName;
  
  // parse options
  parse_arguments(argc, argv, inputFileName, outputFileName); // writes these two
  std::clog << "Reading from " << inputFileName << " and writing to " << outputFileName
	    << std::endl;
  
  // open files for reading, writing
  std::ifstream input(inputFileName.c_str());
  std::ofstream output(outputFileName.c_str());
  
  // first read sample size from the input file
  int n(0);
  input >> n;
  std::clog << "Anticipating " << n << " cases for each variable." << std::endl;

  // read the response vector
  std::vector<double> Y(n, 0.0);
  for (int i=0; i<n; ++i)
    input >> Y[i];
  Model regr(Y);
  
  // read loop for series of X's
  while (true)
  {
    char inputChar;
    input >> inputChar;
    switch (inputChar)
    {
    case 'q':
      {
	output.flush();
	return(0);
      }
    case 'p':
      {
	std::vector<double> yy(n);
	for (int i=0; i<n; ++i)
	  input >> yy[i];
	output << regr.PSS(yy) << std::endl;
	break;
      }
    case 'x':
      {
	std::vector<double> X(n, 0.0);
	double sum(0.0);
	for (int i=0; i<n; ++i)
	{ input >> X[i];
	  sum += X[i];
	}
	bool add(regr.evaluate_gaussian_predictor(X,sum/n));
	if (add)
	  output << 1 << " ";
	else
	  output << 0 << " ";
      }
    }
    output.flush();
  }
  return 0;
}
