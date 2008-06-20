/*  $Id: generate-x.cc,v 1.1 2005/06/13 20:47:50 bob Exp $

  16 Jun 03 ... Generates a predictor matrix
  
*/

#include <vector>
#include <iostream>
#include <fstream>
#include <sstream>

#include "random.h"

void
parse_arguments(int argc, char** argv,
		int& seed,
		int& n,
		int& p,
		std::string& outputFileName)
{
  for (int i=1; i<argc; i=i+2)
  {
    char key(argv[i][0]);
    if ('-' == key)
    { 
      switch (argv[i][1])
      {
      case 'n' :
	{ std::stringstream input(argv[i+1]);
	  input >> n;
	  break;
	}
      case 'o' :
	{ std::string out(argv[i+1]);
	  outputFileName = out;
	  break;
	}
      case 'p' :
	{ std::stringstream input(argv[i+1]);
	  input >> p;
	  break;
	}
      case 's' :
	{ std::stringstream input(argv[i+1]);
	  input >> seed;
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
  
int
generate_x(int seed, int n, int p, std::ostream& output)
{
  // add extra space for y
  ++p;

  // write dimensions
  output << n << " " << p << std::endl;

  // init the random generator
  RandomGenerator rand(seed);
  
  for (int j=0; j<p; ++j)
  {
    for (int i=0; i<n; ++i)
      output << rand.normal() << " ";
    output << std::endl;
  }
  output.flush();
  return n;
}

int main (int argc, char** argv)
 {
   // set defaults
   int seed (4536);

   int n    (100);
   int p    (10);       // number of predictors

   std::string outputFileName ("cout");

   parse_arguments (argc, argv, seed, n, p, outputFileName);
   
   // call depends on file io
   if (outputFileName != "cout")
   {
     std::ofstream output(outputFileName.c_str());
     generate_x(seed, n, p, output);
   }
   else
     generate_x(seed, n, p, std::cout);
   return 0;
 }

