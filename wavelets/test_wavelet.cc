// $Id: test_wavelet.cc,v 1.2 2000/02/14 22:33:38 bob Exp $-*- c++ -*-

#include <iostream>
#include <vector.h>
#include <deque.h>
#include <algo.h>

#include "wavelet.h"
#include "wavecoef.h"
#include "utils.h"


int main()
{
  // make up input data series and report the input ss
  vector<double> test;
  for(int i=0; i<8; ++i)
    test.push_back( (double)i );
  print_vector()("test data", test);
  cout << "Total input SS = " << sum_of_squares()(test) << endl << endl;
    
  // select the coefs, print them, check ss and orthogonal
  Wave_coef wavelet = daub2;
  cout << "Wavelet coefs " << wavelet.lo_pass();
  cout << "Sum of squared wavelet coefs = "
       << sum_of_squares()(wavelet.lo_pass())
       << " with dot prod = "
       << inner_product(wavelet.lo_pass().begin(), wavelet.lo_pass().end(),
			wavelet.hi_pass().begin(), 0.0)
       <<endl;
    
  // make the filter with check of coef ss
  Filter filter(wavelet);
  cout << "Filter initializing done..." << endl << endl;
  
  // build wavelet decomposition of the test data
  filter.decompose(test);
  cout << "Decomposition done with total SS = "
       << filter.sum_of_squares() << " ..." << endl;
  
  // recover the wavelet coef estimates
  deque< vector<double> > estimates = filter.estimated_coefficients();
  cout << "Copied " << estimates.size() << " vectors of estimates" << endl;
  for_each (estimates.begin(), estimates.end(), print_vector());
  cout << endl;
  
  // reconstruct the data from the decomposition
  vector<double> reconstructedTest = filter.reconstruction();
  print_vector()("Reconstruction", reconstructedTest);
  cout << "with total SS in reconstruction = "
       << sum_of_squares()(reconstructedTest);

}

////////////////////////////   EOF   //////////////////////////////


