/*  $Id: simulate.cc,v 1.1 2005/06/13 20:47:50 bob Exp $

  12 Jun 03 ... Driver for comparing batch to sequential procedures.


  Simulation of sequential regression writes sequence of p prediction
  SS to the output, followed by the order of the model that would have
  been chosen.

  Simulation of batch model for the same data returns a sequence of up
  to MAX_FIT prediction SS.  If the batch stops before adding MAX_FIT
  items, the last value is continued to fill the columns. Finally, it
  adds the chosen model order.

  Combined the output line has 2 + p + MAX_FIT columns arranged as
    (sequential pss), sequential q, (batch pss), batch q

  For the available command line options, see the makefile.
  
*/

#include <fstream>
#include <sstream>
#include <ctime>
#include <algorithm>  // random shuffle

#include "random.h"
#include "stat_utils.h"
#include "range.h"
#include "covariance_matrix.h"

#include "seq_regr.h"    // sequential modeling
#include "raters.h"   // batch

const int MAX_BATCH_FIT (25);

void
parse_arguments(int argc, char** argv,
		int& seed,
		int& reps,
		double& r2,
		int& k,
		double& permutePct,
		char& criterion,
		std::string& xFileName,
		std::string& outputFileName)
{
  for (int i=1; i<argc; i=i+2)
  {
    char key(argv[i][0]);
    if ('-' == key)
    { 
      switch (argv[i][1])
      {
      case 'c':  // criterion
	{ std::stringstream input(argv[i+1]);
	  input >> criterion;
	  break;
	}
      case 'k':  // number of coefs
	{ std::stringstream input(argv[i+1]);
	  input >> k;
	  break;
	}
      case 'o' :
	{ std::string out(argv[i+1]);
	  outputFileName = out;
	  break;
	}
      case 'p' :
	{ std::stringstream input(argv[i+1]);
	  input >> permutePct;
	  break;
	}
      case 'r' :
	{ std::stringstream input(argv[i+1]);
	  input >> reps;
	  break;
	}
      case 's' :
	{ std::stringstream input(argv[i+1]);
	  input >> seed;
	  break;
	}
      case 'v' : 
	{ std::stringstream input(argv[i+1]);
	  input >> r2;
	  break;
	}	
      case 'x' :
	{ std::string out(argv[i+1]);
	  xFileName = out;
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
  

std::vector<double>
generate_beta(int p, double r2, int k)  // linear comb of colms in data file
{
  std::vector<double> beta (p,0.0);
  const double b (sqrt(r2/(k*(1-r2))));
  std::clog << "SIMU: Nonzero beta set to " << b << std::endl;
  beta[0] = 0.0;   // skip this column in the data file
  for (int j=1; j<=k; ++j)
    beta[j] = b;
  return beta;
}

std::vector<int>
shuffle_indices(double permutePct, int p)  // from 1 to p
{
  std::vector<int> result (p);
  for (int i=0; i<p; ++i)
    result[i] = i+1;
  int k ((int)ceil(permutePct*(p-1)));
  if (k > 1)
    random_shuffle(result.begin(), result.begin()+k);
  return result;
}

double
sweep_prediction_ss(SweepMatrix const& sm, NumericDataset & data, std::vector<double> const& theta)
{
  std::vector<double> slopes (sm.slopes());
  std::vector<int> slopeIndex (sm.predictors());
  double* fit (begin(data.linear_combination(sm.slopes(), sm.predictors(), sm.intercept())));
  double ss (0.0);
  for (int i=0; i<data.n_rows(); ++i)
  { double dev(fit[i] - theta[i]);
    ss += dev * dev;
  }
  return ss;
}

		    
int
run_simulation(std::string const& xFileName, char criterion,
	       int seed, int nReps, double r2, int k, double permutePct,
	       std::ostream& output)
{
  // number of sequential passes
  const int N_PASSES (2);
  
  // later code overwrites first column to insert random y vectors
  NumericDataset data (xFileName);
  int p (data.n_cols()-1);
  int n (data.n_rows());

  // header line in output describes context; stars flag comment lines
  time_t theTime; time(&theTime);
  output << "* " <<  ctime(&theTime) // includes eol
	 << "* criterion=\"" << criterion << "\"; dim={" << n << "," << p << "}; x=\"" << xFileName
	 << "\"; r2=" << r2 << "; k=" << k << "; permute=" << permutePct 
	 << "; seed=" << seed << "; reps=" << nReps << ";" << std::endl;

  // pick beta vector and generate theta
  std::vector<double> beta (generate_beta(p+1, r2, k));
  std::vector<double> theta (n);
  double* lc (begin(data.linear_combination(beta)));
  for (int i=0; i<n; ++i)
  { theta[i] = *lc;
    ++lc;
  }

  // init the random generator
  RandomGenerator rand(seed);

  const bool outputClock (false);
  
  for (int rep=1; rep<=nReps; ++rep)
  {
    std::clog << "    -----  Top of iteration, rep = " << rep
	      << "/" << nReps << "  -----" << std::endl; 
    output << rep << " ";

    // generate y=theta+N(0,1) and insert it into the data set
    std::vector<double> y (n);
    for (int i=0; i<n; ++i)
      y[i] = theta[i] + rand.normal();
    data.insert_column(y,0);
    
    // set search order for sequential search; marks zero when used
    std::vector<int> indices = shuffle_indices(permutePct, p);

    // multi-pass sequential model starts with base threshold
    {
      int     numberTried (1);
      int     qFirstPass  (0);                           // count number skipped in first pass
      int     dpCount     (0);
      clock_t startTime (clock());
      SequentialRegression regr  (y);
      double yBar (average(y));
      double ss   (sum_of_squares(theta,yBar));         // initial value if none picked
      for (int pass = 1; pass <= N_PASSES ; ++pass)
      {	std::clog << "SIMU: Starting sequential pass #" << pass << " @ "<< startTime << std::endl;
	for (int j=0; j<p; ++j)
	{ if (indices[j] > 0)
	  { double pValueThreshold;
	    if (criterion == 'a')
	      pValueThreshold = 0.05/numberTried;
	    else
	      pValueThreshold = 0.05/p;
	    int xCol (indices[j]);
	    std::vector<double> x(data.extract_column(xCol));
	    double pValue (regr.gaussian_predictor_stats(x, data.average(xCol)).second);
	    if (pValue < pValueThreshold)
	    { regr.add_predictor(x, data.average(xCol));
	      ss = regr.PSS(theta);
	      indices[j] = 0;
	      if (pass == 1) ++qFirstPass;
	      std::clog << "SEQR: Added predictor in column " << xCol << " since "
			<< pValue << " < "<< pValueThreshold << std::endl;
	      numberTried = 1;
	    }
	    else
	    { // std::clog << "SEQR: Col " << xCol << " not added; " << pValue << ">"<< pValueThreshold << std::endl;
	      ++numberTried;
	    }
	    dpCount += 1 + regr.number_of_predictors();
	    output << ss/n << " ";
	    if (outputClock)
	    { clock_t timeNow (clock());
	      output << timeNow-startTime << " ";
	    }
	    else
	      output << dpCount << " ";
	  }
	}
      }
      if (N_PASSES > 1)  // pad output for those skipped over
	for (int i=0; i<qFirstPass; ++i)
	{ output << ss/n << " ";
	  if (outputClock)
	    output << clock()-startTime << " ";
	  else
	    output << dpCount << " ";
	}
      output << regr.number_of_predictors() << " ";
      std::clog << "SIMU: Sequential R2 = " << regr.R2() << std::endl;
    }

    // build sweep and use stepwise rater for batch process
    {
      double pValueThreshold (0.05/p);  // initially Bonferroni
      int    step        (1);
      double ss          (0.0);
      int    dpCount     (2*p+1);
      clock_t startTime  (clock());
      std::clog << "SIMU: Starting batch @ "<< startTime << std::endl;
      CovarianceMatrix<NumericDataset> covMat(data);
      SweepMatrix sm(covMat.cross_products(0), covMat.sums_of_squares(), data.average_vector());
      StepwiseRater rater(sm, data.n_rows());
      std::vector<int> picks ( rater.recommend_predictors(1,pValueThreshold) );
      while (picks.size() > 0 && sm.number_of_predictors() < MAX_BATCH_FIT)
      {
	sm.add_predictor(picks[0], covMat.cross_products(picks[0]));
	ss = sweep_prediction_ss(sm, data, theta);
	output << ss/n << " ";
	if (outputClock)
	{ clock_t timeNow (clock());
	  output << timeNow-startTime << " ";
	}
	else
	  output << dpCount << " ";
	++ step;
	dpCount += p;
	if (criterion == 'a') pValueThreshold = step * 0.05 / p;
	picks = rater.recommend_predictors(1, pValueThreshold);
      }
      for (int i=sm.number_of_predictors(); i<MAX_BATCH_FIT; ++i)
      { output << ss/n << " ";
	if (outputClock)
	  output << clock()-startTime << " ";
	else
	  output << dpCount << " ";
      }
      output << sm.number_of_predictors() << " " <<  std::endl;
      output.flush();
    }
  }
  return nReps;
}

int main (int argc, char** argv)
{
  // set defaults
  int  seed (354);
  int nReps (5);
  double r2 (.25);
  int     k (10);
  char criterion ('b');
  double permutePct (0.0);
  
  std::string outputFileName ("cout");
  
  // predictor design matrix (which implicitly defines n and p)
  // x file should have extra column for holding y at front
  std::string xFileName ("test/randomx.dat");
  
  parse_arguments (argc, argv, seed, nReps, r2, k, permutePct, criterion, xFileName, outputFileName);
  
  // call depends on file io
  if (outputFileName != "cout")
  {
    std::ofstream output(outputFileName.c_str());
    run_simulation(xFileName, criterion, seed, nReps, r2, k, permutePct, output);
  }
  else
    run_simulation(xFileName, criterion, seed, nReps, r2, k, permutePct, std::cout);
  return 0;
}
