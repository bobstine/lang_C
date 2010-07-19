#include "random.h"
#include "read_utils.h"

#include <getopt.h>
#include <time.h>

#include <cstdlib>
#include <iostream>


void
parse_arguments(int argc, char** argv,
		int &n, double &p, int &seed)
{
  int key;
  while (1)                                  // read until empty key causes break
    {
      int option_index = 0;
      static struct option long_options[] = {
	  {"number",            1, 0, 'n'},  // has arg,
	  {"probability",       1, 0, 'p'},  // has arg,
	  {"seed",              1, 0, 's'},  // has arg,
	  {"help",              0, 0, 'h'},  // no  arg, 
	  {0, 0, 0, 0}                       // terminator 
	};
	key = getopt_long (argc, argv, "n:p:h", long_options, &option_index);
	if (key == -1)
	  break;
	// std::cout << "Option key " << char(key) << " with option_index " << option_index << std::endl;
	switch (key)
	  {
	  case 'n' : 
	    {
	      n = read_utils::lexical_cast<int>(optarg);
	      break;
	    }
	  case 'p' : 
	    {
	      p = read_utils::lexical_cast<double>(optarg);
	      break;
	    }
	  case 's' : 
	    {
	      seed = read_utils::lexical_cast<int>(optarg);
	      break;
	    }
	  case 'h' :
	    {
	      std::cout << "switches:" << std::endl << std::endl;
	      std::cout << "      --number=#               number " << std::endl;
	      std::cout << "      -n100" << std::endl << std::endl;
	      std::cout << "      --probability=#          probability of 1" << std::endl;
	      std::cout << "      -p#" << std::endl << std::endl;
	      std::cout << "      --seed=#               random integer seed " << std::endl;
	      std::cout << "      -s100" << std::endl << std::endl;
	      std::cout << "      --help                   generates this message" << std::endl;
	      std::cout << "      -h" << std::endl << std::endl;
	      break;
	    }
	  }
    }
}



int
main(int argc, char** argv)
{
  int n (0);
  double p (1.0);
  int seed (0);

  // init with random seed
  srand ( time(NULL) );
  seed = rand();
  
  parse_arguments(argc, argv, n, p, seed);
  
  RandomGenerator rand(seed);
  while(n--)
  { if(rand.uniform() < p)
      std::cout << "1 ";
    else std::cout << "0 ";
  }
  std::cout << std::endl;
  
  return 0;
}
