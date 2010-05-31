#include <iostream>
#include <fstream>
#include <getopt.h>

// dynamic cast
#include "read_utils.h"

// cyclic iterator
#include "iterators.h"


void
parse_arguments(int argc, char** argv, int& multiplier, std::string& expansionFile)
{
  {
  int key;
  while (1)                                  // read until empty key causes break
    {
      int option_index = 0;
      static struct option long_options[] = {
	  {"constant-factor",  1, 0, 'c'},  // has arg,
	  {"factor-file",      1, 0, 'f'},  // has arg,
	  {"help",             0, 0, 'h'},  // no  arg, 
	  {0, 0, 0, 0}                       // terminator 
	};
	key = getopt_long (argc, argv, "c:f:h", long_options, &option_index);
	if (key == -1)
	  break;
	switch (key)
	{
	case 'c' : 
	  {
	    multiplier = read_utils::lexical_cast<int>(optarg);
	    break;
	  }
	case 'f' :
	  {
	    expansionFile = optarg;
	    break;
	  }
	case 'h' :
	  {
	    std::cout << "switches:" << std::endl << std::endl;
	    std::cout << "      --constant-factor=#        integer number of times to duplicate each input value" << std::endl;
	    std::cout << "      -c4" << std::endl <<  std::endl;
	    std::cout << "      --factor-file=#            file that gives times to duplicate each input value" << std::endl;
	    std::cout << "      -f factors.txt"   <<  std::endl;
	    std::cout << "      --help                   generates this message" << std::endl;
	    std::cout << "      -h" << std::endl << std::endl;
	    exit(0);
	    break;
	  }
	}
    }
  }
}


int main(int argc, char** argv)
{
  int multiplier (0);
  std::string file ("");

  parse_arguments(argc, argv, multiplier, file);

  if (multiplier > 0)            // constant expansion factor found, ignore file
  { std::clog << "Expanding by factor " << multiplier << std::endl;
    double x;
    while (std::cin >> x)
    { int counter = multiplier;
      while(counter--)
	std::cout << x << " ";
    }
    std::cout << std::endl;
  }
  else                           // read vector of multipliers from input file
  { if (file.empty())
      return -1;
    else
    { std::ifstream inputFile (file.c_str());
      if (!inputFile)
	return -2;
      else
      { std::vector<int> repeats;
	int i;
	while(inputFile >> i)
	  repeats.push_back(i);
	std::clog << "Read " << repeats.size() << " expansion factors from file " << file << std::endl;
	cyclic_iterator<int> multipliers (repeats.begin(), repeats.end());
	double x;
	while (std::cin >> x)
	{ int counter = *multipliers;
	  ++multipliers;
	  while(counter--)
	    std::cout << x << " ";
	}
	std::cout << std::endl;
      }
    }
  }
  return 0;
}
