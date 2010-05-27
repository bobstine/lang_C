#include <iostream>
#include <getopt.h>

#include "read_utils.h"

void
parse_arguments(int argc, char** argv, int& multiplier)
{
  {
  int key;
  while (1)                                  // read until empty key causes break
    {
      int option_index = 0;
      static struct option long_options[] = {
	  {"expansion",     1, 0, 'e'},  // has arg,
	  {"help",          0, 0, 'h'},  // no  arg, 
	  {0, 0, 0, 0}                       // terminator 
	};
	key = getopt_long (argc, argv, "e:h", long_options, &option_index);
	if (key == -1)
	  break;
	switch (key)
	{
	case 'e' : 
	  {
	    multiplier = read_utils::lexical_cast<int>(optarg);
	    break;
	  }
	case 'h' :
	  {
	    std::cout << "switches:" << std::endl << std::endl;
	    std::cout << "      --expansion=#        integer number of times to duplicate each input value" << std::endl;
	    std::cout << "      -e4" << std::endl <<  std::endl;
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
  int multiplier (1);

  parse_arguments(argc, argv, multiplier);
  std::clog << "Expanding by factor " << multiplier << std::endl;

  if (multiplier <= 0)
    return -1;

  double x;
  while (std::cin >> x)
  { int counter = multiplier;
    while(counter--)
      std::cout << x << " ";
  }
  std::cout << std::endl;
  return 0;
}
