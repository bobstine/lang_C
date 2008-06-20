//  $Id: seq_regr.test.cc,v 1.1 2007/11/21 23:18:27 bob Exp $
 
#include "seq_regr.h"
#include "print_utils.h"
#include "range.h"

#include <fstream>
#include <iostream>
#include <vector>

std::vector<int>
make_vector (int i)
{
  std::vector<int> vi;
  vi.push_back(i);
  return vi;
}

int main()
{
  // very simple test input file, with n and dim on first line
  //   0/1 indicators on second, y on third line
  std::ifstream input ("/Users/bob/C/seq_regr/test/covar.dat");

  // read sample size and number of variables to read
  int n (0); int d(0);
  input >> n >> d;
  std::cout << "TEST: n = " << n << "  d = " << d << std::endl;

  // read weights first; currently assumed to be 0/1 *selection* weights only
  std::vector<double> w (n);
  for (int i=0; i<n; ++i)
    input >> w[i];
  std::cout << "TEST: Last weight w[n-1] = " << w[n-1] 
            << " with sum " << range_ops::accumulate(make_range(w), 0.0) << std::endl;
  
  // read response
  std::vector<double> y (n);
  for (int i=0; i<n; ++i)
    input >> y[i];
  std::cout << "TEST: Last response y[n-1] = " << y[n-1] << std::endl;
      
  // build and print base regression model
  SequentialRegression regr(make_anonymous_range(make_range(y)));
  std::cout << "TEST:  *************  Initialized Model  *************** \n"
	    << regr << std::endl;

  // read next x variable
  std::vector<double> x(n);
  for (int i=0; i<n; ++i)
    input >> x[i];
  std::cout << "TEST: x[n-1] = " << x[n-1] << std::endl;
  
  std::cout << "TEST: Evaluation of " << x << std::endl 
		<< regr.gaussian_predictor_stats(make_anonymous_range(x))
		<< regr.bennett_predictor_stats(make_anonymous_range(x))
		<< std::endl;

  std::string xName ("xxx");
  std::cout << "TEST:  *************  One-predictor Model  *************** \n"
	    << "     Adding first, change in SS = "
	    << regr.add_predictor(make_tag(xName, make_vector(1)), make_anonymous_range(x)) << std::endl
	    << regr << std::endl;

  std::cout << "    Pred SS = " << regr.PSS(make_anonymous_range(y)) << std::endl;
  std::cout << "    Slopes (including intercept): " << regr.beta() << std::endl;

  std::cout.flush();
  return 0;
}
