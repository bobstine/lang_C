//  $Id: seq_regr.test.cc,v 1.4 2003/12/06 03:13:04 bob Exp $
 
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
  std::ifstream input ("test/covar.dat");

  // read sample size
  int n (0); int p(0);
  input >> n >> p;
  std::cout << "n = " << n << "  p = " << p << std::endl;

  // read response
  std::vector<double> y (n);
  for (int i=0; i<n; ++i)
    input >> y[i];
  std::cout << "y[n-1] = " << y[n-1] << std::endl;
      
  // build and print base regression model
  SequentialRegression regr(
			    make_anonymous_range(make_range(y)));
  std::cout << " *************  Initialized Model  *************** \n"
	    << regr << std::endl;

  // read next x variable
  std::vector<double> x(n);
  for (int i=0; i<n; ++i)
    input >> x[i];
  std::cout << "x[n-1] = " << x[n-1] << std::endl;
  
  std::cout << "Evaluation of " << x << std::endl 
    	    << regr.gaussian_predictor_stats(make_anonymous_range(x))
	    << regr.bennett_predictor_stats(make_anonymous_range(x))
	    << std::endl;

  std::string xName ("xxx");
  std::cout << " *************  One-predictor Model  *************** \n"
	    << "Adding first, change in SS = "
	    << regr.add_predictor(make_tag(xName, make_vector(1)), make_anonymous_range(x)) << std::endl
	    << regr << std::endl;

  std::cout << "Pred SS = " << regr.PSS(make_anonymous_range(y)) << std::endl;
  std::cout << "Slopes (including intercept): " << regr.beta() << std::endl;

  std::cout.flush();
  return 0;
}
