// $Id: sweep_matrix.test.cc,v 1.3 2003/05/30 21:09:53 bob Exp $

#include <iostream>
#include <iterator>
#include <string>
#include <algorithm>

#include "datasets.h"
#include "covariance_matrix.h"
#include "sweep_matrix.h"


std::ostream&
operator<< (std::ostream& os, std::vector<double> x)
{
  std::copy(x.begin(), x.end(), std::ostream_iterator<double>(os," "));
  os << std::endl;
  return os;
}


std::string fileName("test/covar.dat");

int main (void)
 {
   NumericDataset data (fileName);
   CovarianceMatrix<NumericDataset> covMat (data);

   SweepMatrix sm (covMat.cross_products(0), covMat.sums_of_squares());
   std::cout << sm;

   sm.add_predictor(4, covMat.cross_products(4));
   std::cout << sm;

   sm.add_predictor(3, covMat.cross_products(3));
   std::cout << sm;

   sm.add_predictor(1, covMat.cross_products(1));
   std::cout << sm;

   return 0;
}
    
