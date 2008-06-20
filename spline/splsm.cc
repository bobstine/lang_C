/*   $Id: splsm.cc,v 1.6 2005/06/14 22:11:34 bob Exp $

  Filter version of the smoothing spline code, with input as n rows of  3  cols,
  laid out as x y w triples.  Reads/writes to std input, with -v argument giving the
  df for the smoothing.  Writes back x y^.

       splsm -f input.dat -out smooth -v sdf

   2 Sep 04 ... Weights as third input.
   3 Aug 04 ... Created as utility and for debugging use of spline in auction.
  
*/

#include "smoothing_spline.h"

#include <math.h>
#include <vector>
#include <iostream>
#include <fstream>
#include <sstream>

void
parse_arguments(int argc, char** argv, std::string& inputFile, std::string& outputFile, int& df);

void
read_data (std::istream& input, std::vector<double> &x, std::vector<double> &y, std::vector<double> &w);

void
write_data (std::ostream& output, std::vector<double> const& x, std::vector<double> const& y);

int
main(int argc, char** argv)
{
  // arguments
  std::string inputFile ("");
  std::string outputFile("");
  int df (4);
  parse_arguments(argc, argv, inputFile, outputFile, df);

  // read data from stdin or file
  std::vector<double> x, y, w;
  if (inputFile == "")
    read_data(std::cin, x, y, w);
  else
  { std::ifstream input(inputFile.c_str());
    if(input)
    { read_data(input,x,y,w);
      input.close();
    }
    else
    { std::cerr << "SPSM: Could not open input file " << inputFile << std::endl;
      return 1;
    }
  }

  // halt unless got enough data
  std::cerr << "SPLS: Read " << x.size() << " cases; smoothing with " << df << " df.\n";
  if (x.size() < 5)
  { std::cerr << "SPLS: Termininating; fewer than five cases.\n";
    return 3;
  }
  
  // do some rounding first to the x's if lots of data
  int n (x.size());
  if (n > 1000)
  { std::cerr << "SPLS: Rounding X's\n";
    for (int i=0; i<n; ++i)
      x[i] = ((double) round(1000.0*x[i]))/1000.0;
  }

  // do the smoothing
  SmoothingSpline ss(df, x,y,w);
  std::vector<double> smth (n);
  ss.fill_with_smooth(df, smth.begin());

  // write the output data
  if (outputFile == "")
    write_data(std::cout,x,smth);
  else
  { std::ofstream output (outputFile.c_str());
    if (output)
    { write_data(output,x,smth);
      output.close();
    }
    else
    { std::cerr << "SPSM: Could not open output file " << inputFile << std::endl;
      return 2;
    }
  }
  return 0;
}

void
read_data (std::istream& input, std::vector<double> &x, std::vector<double> &y, std::vector<double> &w)
{
  double xIn,yIn,wIn;
  while (input)
  { input >> xIn;
    if (input)
    { input >> yIn >> wIn;
      x.push_back(xIn);
      y.push_back(yIn);
      w.push_back(wIn);
    }
  }
}

void
write_data (std::ostream& output, std::vector<double> const& x, std::vector<double> const& y)
{
  for (unsigned int i=0; i<x.size(); ++i)
    output << x[i] << " " << y[i] << std::endl;
}


void
parse_arguments(int argc, char** argv,
		std::string& inputFile,
		std::string& outputFile,
		int& df)
{
  for (int i=1; i<argc; i=i+2)
  {
    char key(argv[i][0]);
    if ('-' == key)
    { 
      switch (argv[i][1])
      {  
      case 'f' :                                    
	{ std::string name(argv[i+1]);
	  inputFile = name;
	  break;
	}
      case 'o' :  
	{ std::string name(argv[i+1]);
	  outputFile = name;
	  break;
	}
      case 'v' :
	{ std::istringstream is(argv[i+1]);
	  is >> df;
	  break;
	}
      default  :
	{
	  std::clog << "Option " << argv[i][0] << " not recognized." << std::endl;
	  break;
	}
      }
    }
  }
}  
  
