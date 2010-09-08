/*
  Build a collecion of 0/1 indicators as used in cross-validation, with replication as
  needed in block-structured applications.

  7 Sep 2010
  
*/

#include "read_utils.h"

#include <iostream>
#include <fstream>
#include <vector>
#include <stdlib.h>
#include <getopt.h>
#include <assert.h>



void
parse_arguments(int argc, char** argv,
		int    &prefix,              // number of prefix 1 blocks
		int    &n,                   // number random items
		int    &c,                   // number of 1s
		int    &blockSize,           // blocking factor, such as number of counties
		int    &seed)                // for rand

{
  int key;
  while (1)                                  // read until empty key causes break
    {
      int option_index = 0;
      static struct option long_options[] = {
	{"prefix",        1, 0, 'p'},  // has arg,
	{"number",        1, 0, 'n'},  // has arg,
	{"choose",        1, 0, 'c'},  // has arg,
	{"block-size",    1, 0, 'b'},  // has arg,
	{"seed",          1, 0, 's'},
	{"help",          0, 0, 'h'},  // no  arg, 
	{0, 0, 0, 0}                       // terminator 
      };
      key = getopt_long (argc, argv, "p:n:c:b:s:h", long_options, &option_index);
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
	      prefix = read_utils::lexical_cast<int>(optarg);
	      break;
	    }
	  case 'c' : 
	    {
	      c = read_utils::lexical_cast<int>(optarg);
	      break;
	    }
	  case 'b' : 
	    {
	      blockSize = read_utils::lexical_cast<int>(optarg);
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
	      std::cout << "      --number=#           number of random draws" << std::endl;
	      std::cout << "      -n100" << std::endl <<  std::endl;
	      std::cout << "      --choose=#           number of draws to be selected at random" << std::endl;
	      std::cout << "      -c20" << std::endl << std::endl;
	      std::cout << "      --block-size=#       number times each value is repeated" << std::endl;
	      std::cout << "      -b50" << std::endl << std::endl;
	      std::cout << "      --prefix=#           prefix with blocks of 1, choose from rest" << std::endl;
	      std::cout << "      -b50" << std::endl << std::endl;
	      std::cout << "      --seed=#             random seed " << std::endl;
	      std::cout << "      -s50363" << std::endl << std::endl;
	      std::cout << "      --help                   generates this message" << std::endl;
	      std::cout << "      -h" << std::endl << std::endl;
	      exit(0);
	      break;
	    }
	  }
    }
}

int main(int argc, char ** argv)
{
  int prefix (0);
  int n, c;
  int blockSize (1);
  int seed (2217);

  std::string one  ("1 ");
  std::string zero ("0 ");
  
  parse_arguments(argc, argv,   prefix, n,c, blockSize,seed);
  assert(c < n);
  //  std::clog << "Arguments are prefix=" << prefix << "  n=" << n << "  c=" << c << "   b=" << blockSize << "  s=" << seed << std::endl;
  srand48(seed);
  double dc (c);
  for (int i=0; i<prefix; ++i)
    for (int j=0; j<blockSize; ++j)
      std::cout << one;
  ++n;
  while (--n)
  {
    std::string valueStr (zero);
    double p (dc/n);
    // std::cout << "\n c=" << dc << "/n=" << n << "  p=" << p << " ";
    if(drand48() < p)
    { valueStr = one;
      dc -= 1.0;
    }
    int write (blockSize);
    while(write--)
      std::cout << valueStr;
  }
  std::cout << std::endl;
  return 0;
}
