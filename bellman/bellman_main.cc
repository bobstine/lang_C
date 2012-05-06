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
make_prob_dist_ptr (double p);

void
parse_arguments(int argc, char** argv,
		double &gamma, int &nRounds, bool &constrained,
		double &oracleProb, double &bidderProb,  
		double &spendPct, bool &writeTable);

int  main(int argc, char** argv)
{
  const double omega  = 0.05;

  // default arguments
  double       gamma  = 2.5;
  int        nRounds  = 100;
  bool     constrain  = false;    // ignores oracle prob if not constrained
  double   oracleProb = 0.0;      // use univ oracle if zero prob and constrained
  double   bidderProb = 0.0;
  bool     writeTable = false;    // if false, only return final value
  double    spendPct  = 0.5;
  
  parse_arguments(argc, argv, gamma, nRounds, constrain, oracleProb, bidderProb, spendPct, writeTable);
  if ((!constrain) && (oracleProb != 0))
    std::cout << "Warning: Unconstrained but nonzero oracle probability " << oracleProb << " assigned." << std::endl;

  
  // build probability distribution (null if none)
  ProbDist *pOracleDist = make_prob_dist_ptr(oracleProb);
  ProbDist *pBidderDist = make_prob_dist_ptr(bidderProb);

  if(!constrain)           // unconstrained oracle 
  { std::cout <<                         "uncon " << pBidderDist->identifier() << " ";
    RejectVectorUtility utility(gamma,omega);
    solve_bellman_utility (gamma, omega, nRounds, utility,               *pBidderDist, writeTable);
  }
  else                     // constrained expert
  { std::cout << pOracleDist->identifier() << " " << pBidderDist->identifier() << " ";
    RejectMatrixUtility utility(gamma, omega);
    solve_bellman_utility (gamma, omega, nRounds, utility, *pOracleDist, *pBidderDist, writeTable);
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
		double &gamma, int &nRounds, bool &constrain,
		double &oracleProb, double &bidderProb,   // zero denotes universal
		double &spendPct, bool &writeTable)
{
  static struct option long_options[] = {
    {"gamma",      required_argument, 0, 'g'},
    {"constrain",        no_argument, 0, 'c'},
    {"oracleprob", required_argument, 0, 'o'},
    {"bidderprob", required_argument, 0, 'b'},
    {"rounds",     required_argument, 0, 'n'},
    {"spend",      required_argument, 0, 's'},
    {"write",            no_argument, 0, 'w'},
    {0, 0, 0, 0}                             // terminator 
  };
  int key;
  int option_index = 0;
  while (-1 !=(key = getopt_long (argc, argv, "g:co:b:n:s:w", long_options, &option_index))) // colon means has argument
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
	constrain = true;
	break;
      }
    case 'o' : 
      {
	oracleProb = read_utils::lexical_cast<double>(optarg);
	break;
      }
    case 'b' : 
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
make_prob_dist_ptr (double prob)
{
  ProbDist *p = NULL;
  
  if (0 == prob)
    p = new UniversalDist(); 
  else
    p = new GeometricDist(prob);
  return p;
}
