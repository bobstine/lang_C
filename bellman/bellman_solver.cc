#include "bellman.h"

#include <math.h>

#include <iostream>
#include <getopt.h>
#include "read_utils.h"     



void
parse_arguments(int argc, char** argv,     double &gamma, int &nRounds, double &constrainGeoProb, char &bidderProbChar, double &spendPct, bool &writeTable);

int  main(int argc, char** argv)
{
  const double omega  = 0.05;

  // default arguments
  double       gamma  = 2.5;
  int        nRounds  = 100;
  double    spendPct  = 0.5;
  double      geoProb = 0.0;      // use constrained geometric oracle if non-zero prob
  char bidderProbDist = 'u';
  bool     writeTable = false;    // if false, only return final value
  
  parse_arguments(argc, argv, gamma, nRounds, geoProb, bidderProbDist, spendPct, writeTable);
  
  // select function bidder uses to spend down probability
  ProbDist pdf;
  switch (bidderProbDist)
  {
  case 'u': { pdf = universal; break; }
  case 'g': { pdf = geometric; break; }
  default: { std::cerr << "ERROR: Unrecognized probablity distribution " << bidderProbDist << " chosen.\n"; return -1; }
  }
  
  RejectMatrixUtility utility(gamma, omega); 
  if (0 == geoProb)
  { std::cout << "u " << bidderProbDist << " ";                                     // prefix to id bidder type
    solve_bellman_utility (gamma, omega, nRounds, utility, universal, pdf , writeTable);
  } else
  { std::cout << "g" << geoProb << " " << bidderProbDist << " ";                   // prefix to id bidder type
    set_geometric_rate(geoProb);
    solve_bellman_utility (gamma, omega, nRounds, utility, geometric, pdf , writeTable);
  }

  /*
    if (geoProb <= 0)     // one-dimensional state, unconstrained expert
    solve_bellman_alpha_equation (gamma, omega, nRounds, spendPct, p, true);
    else                      // two-dimensional state, constrained
    solve_constrained_bellman_alpha_equation (gamma, omega, nRounds, spendPct, geoProb, p, writeTable);
  */
  return 0;
}



void
parse_arguments(int argc, char** argv,		double &gamma, int &nRounds, double &geoProb, char &probDist, double &spendPct, bool &writeTable)
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
	geoProb= read_utils::lexical_cast<double>(optarg);
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

