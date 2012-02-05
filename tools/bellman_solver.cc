#include "bellman.h"

#include <math.h>

#include <iostream>
#include <getopt.h>
#include "read_utils.h"     



void
parse_arguments(int argc, char** argv,     double &gamma, int &nRounds, char &probChar, double &spendPct, bool &writeTable);

int  main(int argc, char** argv)
{
  const double omega  = 0.05;

  // default arguments
  double     gamma  = 2.5;
  int      nRounds  = 100;
  double  spendPct  = 0.5;
  char    probChar  = 'u';
  bool   writeTable = false;    // if false, only return final value
  
  parse_arguments(argc, argv, gamma, nRounds, probChar, spendPct, writeTable);
  
  // select function for spending down probability
  ProbDist p;
  switch (probChar)
  {
  case 'u': { p = universal; break; }
  case 'g': { p = geometric; break; }
  case 'e': { p = equal;     break; }
  default: { std::cerr << "ERROR: Unrecognized probablity distribution " << probChar << " chosen.\n"; return -1; }
  }
  
  // output parameters and let solver tack on results: e-gamma b, e, b
  std::cout << gamma << " " << nRounds << " " << writeTable << " " << probChar << " " << spendPct << " ";

  // one-dimensional state, unconstrained expert
  solve_bellman_equation (gamma, omega, nRounds, spendPct, p, true);

  // two-dimensional state, constrained
  solve_constrained_bellman_equation (gamma, omega, nRounds, spendPct, geometric, p);
  
  return 0;
}



void
parse_arguments(int argc, char** argv,		double &gamma, int &nRounds, char &probChar, double &spendPct, bool &writeTable)
{
  static struct option long_options[] = {
    {"gamma",   required_argument, 0, 'g'},
    {"rounds",  required_argument, 0, 'n'},
    {"prob",    required_argument, 0, 'p'},
    {"spend",   required_argument, 0, 's'},
    {"write",         no_argument, 0, 'w'},
    {0, 0, 0, 0}                             // terminator 
  };
  int key;
  int option_index = 0;
  while (-1 !=(key = getopt_long (argc, argv, "g:n:p:s:w", long_options, &option_index))) // colon means has argument
  {
    // std::cout << "Option key " << char(key) << " for option " << long_options[option_index].name << ", option_index=" << option_index << std::endl;
    switch (key)
    {
    case 'g' : 
      {
	gamma = read_utils::lexical_cast<double>(optarg);
	break;
      }
    case 'n' :
      {
	nRounds = read_utils::lexical_cast<int>(optarg);
	break;
      }
    case 'p' :
      {
	probChar = read_utils::lexical_cast<char>(optarg);
	break;
      }
    case 's' :
      {
	spendPct = read_utils::lexical_cast<double>(optarg);
	break;
      }
    case 'w' : 
      {
	writeTable=true ;
	break;
      }
    default:
      {
	std::cout << "PARSE: Option not recognized; returning.\n";
      }
    } // switch
  } // while
}

