// $Id: double-boot.cc,v 1.1 2003/07/21 21:58:25 bob Exp $

/*
  Code to run a double bootstrap fast.

   1 Jul 03 ... Created in AA for class.
   
*/

#include <vector>
#include <functional>
#include <algorithm>
#include <iostream>
#include <sstream>
#include <math.h>

#include "random.h"

using std::vector;

class Resampler : public std::unary_function< vector<double>, vector<double> >
{
  RandomGenerator mRand;
public:
  
  Resampler() :
    mRand(RandomGenerator()) { };

  vector<double>
  operator()(vector<double> x)
    {
      int n (x.size());
      vector<double> result (n);
      for (int i=0; i<n; ++i)
	result[i] = x[(int)floor(mRand.uniform()*n)];
      return result;
    }
};

class Average : public std::unary_function<vector<double>,double>
{
public:
  double operator()(vector<double> const& x) const
  {
    double sum (0.0);
    for (unsigned int i=0; i<x.size(); ++i)
      sum += x[i];
    return sum/x.size();
  }
};
 
class StandardDeviation : public std::unary_function<vector<double>,double>
{
public:
  double operator()(vector<double> const& x) const
  {
    double sum (0.0);
    for (unsigned int i=0; i<x.size(); ++i)
      sum += x[i];
    double avg (sum/x.size());
    double ss (0.0);
    for (unsigned int i=0; i<x.size(); ++i)
    { double dev (x[i] - avg);
      ss += dev * dev;
    }
    return sqrt(ss/(x.size()-1));
  }
};


class Variance : public std::unary_function<vector<double>,double>
{
public:
  double operator()(vector<double> const& x) const
  {
    double sum (0.0);
    for (unsigned int i=0; i<x.size(); ++i)
      sum += x[i];
    double avg (sum/x.size());
    double ss (0.0);
    for (unsigned int i=0; i<x.size(); ++i)
    { double dev (x[i] - avg);
      ss += dev * dev;
    }
    return (ss/(x.size()-1));
  }
};

void
parse_arguments(int argc, char** argv,
		int& B,
		double& lowerQuantile,
		double& upperQuantile)
{
  for (int i=1; i<argc; i=i+2)
  {
    char key(argv[i][0]);
    if ('-' == key)
    { 
      switch (argv[i][1])
      {
      case 'B' : // number of replications
	{ std::stringstream input(argv[i+1]);
	  input >> B;
	  break;
	}
      case 'l':  // lower quantile, as in 0.05
	{ std::stringstream input(argv[i+1]);
	  input >> lowerQuantile;
	  break;
	}
      case 'u':  // upper quantile, as in 0.95
	{ std::stringstream input(argv[i+1]);
	  input >> upperQuantile;
	  break;
	}
      default  :
	{
	  std::clog << "Option " << argv[i][0] << " not recognized." << std::endl;
	  break;
	}
      }
    }
  }
}


int main (int argc, char **argv)
{
  vector<double> data;

  // parse options
  double lowerQuantile (0.01);
  double upperQuantile (0.99);
  int    B             (1000);
  parse_arguments(argc, argv, B,lowerQuantile,upperQuantile);
  
  // read data
  double x;
  while (std::cin>>x)
    data.push_back(x);
  
  //  Average theStat;
  //  StandardDeviation theStat;
  Variance theStat;

  double theta (theStat(data));
  std::cout << "Theta = " << theta << " from " << data.size() << " values;"
	    << " with quantiles at " << lowerQuantile << " and " << upperQuantile << std::endl;
  
  // do the double BS
  int coverCount (0);
  int inLow  ((int) floor(B*lowerQuantile));
  int inUp   ((int) ceil(B*upperQuantile));
  Resampler sampler;
  for (int i=0; i<B; ++i)
  { vector<double> xStar (sampler(data));
    vector<double> stat (B);
    for (int b=0; b<B; ++b)
      stat[b] = theStat(sampler(xStar));
    sort(stat.begin(), stat.end());
    if ((stat[inLow] < theta) && (theta <stat[inUp]))
      ++coverCount;
    if (i%50 == 0)
      std::cout << i << ": (" << stat[inLow] << ", " << stat[inUp] << ")" << std::endl;
  }
  std::cout << std::endl << coverCount << "/" << B << std::endl;
  return 0;
}
  
