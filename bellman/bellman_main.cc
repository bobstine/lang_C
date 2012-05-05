#include "bellman.h"

#include <math.h>

#include <iostream>
#include <getopt.h>
#include "read_utils.h"     



/*
  prob character indicates the distribution, u for universal and g for geometric
  
  following probability is only relevant for a geometric distribution.

*/

ProbDist*
make_prob_dist_ptr (char code, double p = 0.0);

void
parse_arguments(int argc, char** argv,
		double &gamma, int &nRounds,
		char &oracleProbChar, double &oracleProb, char &bidderProbChar, double &bidderProb,
		double &spendPct, bool &writeTable);

int  main(int argc, char** argv)
{
  const double omega  = 0.05;

  // default arguments
  double       gamma  = 2.5;
  int        nRounds  = 100;
  double    spendPct  = 0.5;
  char     oracleChar = 'u';
  double   oracleProb = 0.0;      // use constrained geometric oracle if non-zero prob
  char     bidderChar = 'u';
  double   bidderProb = 0.0;
  bool     writeTable = false;    // if false, only return final value
  
  parse_arguments(argc, argv, gamma, nRounds, oracleChar, oracleProb, bidderChar, bidderProb, spendPct, writeTable);

  // build probability distribution (null if none)
  ProbDist *pOracleProb = make_prob_dist_ptr(oracleChar, oracleProb);
  ProbDist *pBidderProb = make_prob_dist_ptr(bidderChar, bidderProb);

  if(0 == oracleProb)  // unconstrained oracle 
  { std::cout <<                         "uncon " << pBidderProb->identifier() << " ";
    RejectVectorUtility utility(gamma,omega);
    solve_bellman_utility (gamma, omega, nRounds, utility,               *pBidderProb, writeTable);
  }
  else                     // constrained expert
  { std::cout << pOracleProb->identifier() << " " << pBidderProb->identifier() << " ";
    RejectMatrixUtility utility(gamma, omega);
    solve_bellman_utility (gamma, omega, nRounds, utility, *pOracleProb, *pBidderProb, writeTable);
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
parse_arguments(int argc, char** argv,
		double &gamma, int &nRounds,
		char &oracleProbChar, double &oracleProb, char &bidderProbChar, double &bidderProb,
		double &spendPct, bool &writeTable)
{
  static struct option long_options[] = {
    {"gamma",      required_argument, 0, 'g'},
    {"oracle",     required_argument, 0, 'o'},
    {"oracleprob", required_argument, 0, 'm'},
    {"bidder",     required_argument, 0, 'b'},
    {"bidderprob", required_argument, 0, 'p'},
    {"rounds",     required_argument, 0, 'n'},
    {"spend",      required_argument, 0, 's'},
    {"write",            no_argument, 0, 'w'},
    {0, 0, 0, 0}                             // terminator 
  };
  int key;
  int option_index = 0;
  while (-1 !=(key = getopt_long (argc, argv, "g:o:m:b:p:n:s:w", long_options, &option_index))) // colon means has argument
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
    case 'o' : 
      {
	oracleProbChar = read_utils::lexical_cast<char>(optarg);
	break;
      }
    case 'm' :
      {
	oracleProb = read_utils::lexical_cast<double>(optarg);
	break;
      }
    case 'b' : 
      {
	bidderProbChar = read_utils::lexical_cast<char>(optarg);
	break;
      }
    case 'p' :
      {
	bidderProb = read_utils::lexical_cast<double>(optarg);
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

ProbDist*
make_prob_dist_ptr (char code, double prob)
{
  ProbDist *p = NULL;
  
  switch (code)
  {
  case 'O': { break; }
  case 'u': { p = new UniversalDist(); break; }
  case 'g': { p = new GeometricDist(prob); break; }
  default : { std::cout << "PARSE: Option " << code << " for distribution not recognized." << std::endl; }
  }
  return p;
}
