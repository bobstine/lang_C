#include "bellman.h"

#include <math.h>

#include <iostream>
#include <sstream>
#include <getopt.h>
#include "read_utils.h"     


// Where to start the universal coder

const int universalStart (1);

// Need to use special order of calls to fill geometric
// prob=0 signals universal, prob > 0 is geometric

WealthArray*
make_wealth_array(double omega, int iOmega, double prob);


//  prob character indicates the distribution, u for universal and g for geometric

void
parse_arguments(int argc, char** argv,
		double &gamma, int &nRounds, bool &constrained,
		double &oracleProb, double &bidderProb,  
		double &spendPct, bool &writeTable);


// Main     Main     Main     Main     Main     Main     Main     Main     Main     Main     Main     Main     
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
  
  const int iOmega    (nRounds+1);   
  WealthArray* pBidderWealth = make_wealth_array(omega, iOmega, bidderProb);
  WealthArray* pOracleWealth = make_wealth_array(omega, iOmega, oracleProb);
  
  if(!constrain)           // unconstrained oracle 
  { std::cout <<                         "uncon " << pBidderWealth->name() << " ";
    RejectVectorUtility utility(gamma, omega); 
    //    RiskVectorUtility utility(gamma, omega);
    solve_bellman_utility (nRounds, utility, *pBidderWealth, writeTable);
  }
  else                     // constrained expert
  { std::cout << pOracleWealth->name() << " " << pBidderWealth->name() << " ";
    // RejectMatrixUtility utility(gamma, omega); 
    RiskMatrixUtility utility(gamma, omega);
    solve_bellman_utility (nRounds, utility, *pOracleWealth, *pBidderWealth, writeTable);
  }
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


WealthArray*
make_wealth_array(double omega, int iOmega, double prob)
{
  if(0 == prob)         // universal
    return new WealthArray(omega, iOmega, UniversalDist(universalStart));
  else if (prob > 1)    // uniform
    return new WealthArray(omega, iOmega, UniformDist( trunc(prob) ));
  else                  // geometric
    return new WealthArray(omega, iOmega, prob);
}
