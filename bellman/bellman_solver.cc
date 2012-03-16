#include "bellman.h"

#include <math.h>

#include <iostream>
#include <getopt.h>
#include "read_utils.h"     



void
parse_arguments(int argc, char** argv,     double &gamma, int &nRounds, double &constrainGeoProb, char &probChar, double &spendPct, bool &writeTable);

int  main(int argc, char** argv)
{
  const double omega  = 0.05;

  // default arguments
  double      gamma  = 2.5;
  int       nRounds  = 100;
  double consGeoProb = 0.0;      // use constrained geometric oracle if non-zero prob
  double   spendPct  = 0.5;
  char     probDist  = 'u';
  bool    writeTable = false;    // if false, only return final value
  
  parse_arguments(argc, argv, gamma, nRounds, consGeoProb, probDist, spendPct, writeTable);
  
  // select function for spending down probability
  ProbDist pdf;
  switch (probDist)
  {
  case 'u': { pdf = universal; break; }
  case 'g': { pdf = geometric; break; }
  default: { std::cerr << "ERROR: Unrecognized probablity distribution " << probDist << " chosen.\n"; return -1; }
  }

  
  RiskMatrixUtility utility(gamma, omega);             // oracle    bidder
  solve_bellman_utility (gamma, omega, nRounds, utility, universal ,   pdf   , writeTable);


  /*
    if (consGeoProb <= 0)     // one-dimensional state, unconstrained expert
    solve_bellman_alpha_equation (gamma, omega, nRounds, spendPct, p, true);
    else                      // two-dimensional state, constrained
    solve_constrained_bellman_alpha_equation (gamma, omega, nRounds, spendPct, consGeoProb, p, writeTable);
  */
  return 0;
}



void
parse_arguments(int argc, char** argv,		double &gamma, int &nRounds, double &consGeoProb, char &probDist, double &spendPct, bool &writeTable)
{
  static struct option long_options[] = {
    {"gamma",    required_argument, 0, 'g'},
    {"constrain",required_argument, 0, 'c'},
    {"rounds",   required_argument, 0, 'n'},
    {"probdist", required_argument, 0, 'p'},
    {"spend",    required_argument, 0, 's'},
    {"write",          no_argument, 0, 'w'},
    {0, 0, 0, 0}                             // terminator 
  };
  int key;
  int option_index = 0;
  while (-1 !=(key = getopt_long (argc, argv, "g:c:n:p:s:w", long_options, &option_index))) // colon means has argument
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
    case 'c' : 
      {
	consGeoProb= read_utils::lexical_cast<double>(optarg);
	break;
      }
    case 'p' :
      {
	probDist = read_utils::lexical_cast<char>(optarg);
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

