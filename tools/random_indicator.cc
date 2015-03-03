/*
  Build a collecion of 0/1 indicators as used in cross-validation, with replication as
  needed in block-structured applications.

  7 Sep 2010
  
*/

#include "read_utils.h"
#include "string_trim.h"

#include <iostream>
#include <fstream>
#include <vector>
#include <map>
#include <cmath>
#include <stdlib.h>
#include <getopt.h>
#include <assert.h>


void
parse_arguments(int argc, char** argv,
		bool        &header,           // add lines with length, name, attributes
		int         &prefix,           // number of prefix 1 blocks
		int         &n,                // number random items (reads from std in if not set)
		double      &pc,               // proportion to use (if <= 1, or count to use if > 1)
		std::string &balanceFile,      // response values to balanced over (eg, multinomial comparison)
		int         &blockSize,        // blocking factor, such as number of counties
		int         &seed);              

  
int main(int argc, char ** argv)
{
  using std::string;

  bool headerLines   = false;
  int prefixCases    = 0;
  int n              = 0;
  double choose      = 0;
  string balanceFile = "";
  int blockSize      = 1;
  int seed           = 2741;

  parse_arguments(argc, argv, headerLines,  prefixCases, n, choose, balanceFile, blockSize,seed);
  if (n == 0) std::cin >> n;
  std::clog << "random_indicator --prefix=" << prefixCases << " -n" << n << " -c" << choose
	    << "--balance=" << balanceFile << " --blocksize=" << blockSize << " --seed=" << seed;
  if (headerLines) std::clog << " --header";
  std::clog << std::endl;
  if ((!balanceFile.empty()) && (1<blockSize))
  { std::clog << "random_indicator does not allow balancing and blocks.\n";
    return 1;
  }
  if (headerLines) std::cout << n << "\nCV\nrole cv\n";
  srand48(seed);
  for (int i=0; i<prefixCases; ++i)
    for (int j=0; j<blockSize; ++j)
      std::cout << "1\t";
  if (choose <= 1.0)
    choose = (double) floor(choose * (double)n);
  assert(choose < n);
  if(balanceFile.empty()) 
  { ++n;
    int value;
    while (--n)
    { double p = choose/(double)n;       // std::cout << "\n c=" << dc << "/n=" << n << "  p=" << p << " ";
      if(drand48() < p)
      { value = 1;
	choose -= 1.0;
      }
      else value = 0;
      for(int i=0; i<blockSize; ++i)
	std::cout << value << "\t";
    }
    std::cout << std::endl;
  }
  else // write count 1s for each of the tags identified in the balance file
  { std::map<string,double> counts;
    std::vector<string> tags;
    std::ifstream input{balanceFile};
    while (input.good())
    { string word;
      input >> word;
      word = trim(word);
      if (word.empty()) break;
      tags.push_back(word);
      ++counts[word];
    }
    std::clog << "random_indicator read " << tags.size() << " tokens of " << counts.size() << " types to balance:\n      ";
    std::map<string,size_t> tagIndex;
    std::vector<double> numLeftToChoose(counts.size()),numRemaining(counts.size());
    {
      size_t i = 0;
      for(auto p : counts)
      { std::clog << "{" << p.first << " " << p.second << "}  ";
	tagIndex[p.first]=i;
	numLeftToChoose[i] = choose;
	numRemaining[i]    = p.second;
	++i;
      }
      std::clog << std::endl;
    }
    for (size_t i=0; i<(size_t)n; ++i)
    { size_t index = tagIndex[tags[i]];
      double p = numLeftToChoose[index]/numRemaining[index];
      if (drand48() < p)
      { std::cout << 1;
	numLeftToChoose[index] -= 1;
      }
      else std::cout << 0;
      std::cout << "\t";
      numRemaining[index] -= 1;
    }
    std::cout << std::endl;
  }
  return 0;
}



void
parse_arguments(int argc, char** argv,
		bool   &header,
		int    &prefix,              // number of prefix 1 blocks
		int    &n,                   // number random items
		double &c,                   // number of 1s or proportion 1
		std::string &balanceFile,    // file of response to balance
		int    &blockSize,           // blocking factor, such as number of counties
		int    &seed)                // for rand
{
  int key;
  while (1)                                  // read until empty key causes break
    {
      int option_index = 0;
      static struct option long_options[] = {
	{"header",        0, 0, 'v'},  
	{"prefix",        1, 0, 'p'},  // has arg,
	{"number",        1, 0, 'n'},  // has arg,
	{"choose",        1, 0, 'c'},  // has arg,
	{"balance",       1, 0, 'y'},
	{"block-size",    1, 0, 'b'},  // has arg,
	{"seed",          1, 0, 's'},
	{"help",          0, 0, 'h'},  // no  arg, 
	{0, 0, 0, 0}                       // terminator 
      };
      key = getopt_long (argc, argv, "vp:n:c:y:b:s:h", long_options, &option_index);
      if (key == -1)
	break;
      // std::cout << "Option key " << char(key) << " with option_index " << option_index << std::endl;
	switch (key)
	  {
	  case 'v' : { header      = true;                                     break; }      
	  case 'p' : { prefix      = read_utils::lexical_cast<int>(optarg);    break; }
	  case 'n' : { n           = read_utils::lexical_cast<int>(optarg);    break; }
	  case 'c' : { c           = read_utils::lexical_cast<double>(optarg); break; }
	  case 'y' : { balanceFile = optarg;                                   break; }
	  case 'b' : { blockSize   = read_utils::lexical_cast<int>(optarg);    break; }
	  case 's' : { seed        = read_utils::lexical_cast<int>(optarg);    break; }
	  case 'h' :
	    {
	      std::cout << "switches:" << std::endl << std::endl;
	      std::cout << "      --number=#           number of random draws" << std::endl;
	      std::cout << "      -n100" << std::endl <<  std::endl;
	      std::cout << "      --choose=#           number of draws to be selected at random" << std::endl;
	      std::cout << "      -c20" << std::endl << std::endl;
	      std::cout << "      --balance= <file>    name of file with y values to balance" << std::endl;
	      std::cout << "      -yfileName" << std::endl << std::endl;
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
