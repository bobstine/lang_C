/*
 *  random_head
 *  tools
 *
 *  Created by Robert Stine on 3/1/2009.
 *  Copyright 2009. All rights reserved.

 Always copies the initial line, writes sample of others to std output
 
 */

#include <iostream>
#include <string>
#include <sstream>
#include <random>
#include <cstdio>
#include <getopt.h>

void
parse_arguments(int argc, char** argv, int &seed, int &numberToSample, int &linesAvailable);

int 
main(int argc, char** argv)
{
  // Parse   seed, number of lines to sample, and total number of lines on first line
  int seed, numberToSample, linesAvailable;
  parse_arguments(argc, argv, seed, numberToSample, linesAvailable);
  std::clog << "RH: Sample " << numberToSample << " lines out of " << linesAvailable << " [seed=" << seed << "]\n";

  // Echo the name line
  std::string inputLine;
  getline(std::cin, inputLine);
  std::cout << inputLine << std::endl;

  // Initialize random number generator
  std::default_random_engine generator(seed);
  std::uniform_real_distribution<double> urand(0.0, 1.0);
  
  // Sample subsequent lines
  while((0 < linesAvailable) & (0 < numberToSample))
  { getline(std::cin, inputLine);
    if (urand(generator) < (double)numberToSample/(double)linesAvailable) { // output current line
      std::cout << inputLine << std::endl; 
      --numberToSample;
    }
    --linesAvailable;
  }

  return 0;
}

void
parse_arguments(int argc, char** argv, int &seed, int &numberToSample, int &linesAvailable)
{
  int key;
  while (1)                                  // read until empty key causes break
    {
      int option_index = 0;
      static struct option long_options[] = {
	  {"seed",         1, 0, 's'},  // has arg,
	  {"sample",       1, 0, 'n'},  // has arg,
	  {"total",        1, 0, 'N'},  // has arg,
	  {0, 0, 0, 0}                       // terminator 
	};
	key = getopt_long (argc, argv, "s:n:N:", long_options, &option_index);
	if (key == -1)
	  break;
	//	std::cout << "Option key " << char(key) << " with optarg " << optarg << std::endl;
	switch (key)
	  {
          case 's' :
	    {
	      std::istringstream is(optarg);
	      is >> seed;
	      break;
	    }
          case 'n' :
	    {
	      std::istringstream is(optarg);
	      is >> numberToSample;
	      break;
	    }
          case 'N' :
	    {
	      std::istringstream is(optarg);
	      is >> linesAvailable;
	      break;
	    }
	  }
    }
}

