// $Id: projector.test.cc,v 1.1 2003/06/02 16:21:04 bob Exp $

#include <iostream>
#include <iterator>
#include <string>
#include <algorithm>

#include "datasets.h"
#include "covariance_matrix.h"
#include "sweep_matrix.h"
#include "projector.h"

#include "range.h"

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

   Projector<NumericDataset> projector (data,sm);
   std::vector<double> wts (data.n_rows(),1.0);

   sm.add_predictor(4, covMat.cross_products(4));
   std::cout << "beta " << sm.slopes();
   std::cout << "Residuals, with x4" << std::endl
	     << projector.ols_residual_vector() << std::endl
	     << projector.weighted_partial_residual_ss_vector(wts) << std::endl;

   sm.add_predictor(3, covMat.cross_products(3));
   std::cout << "beta " << sm.slopes();
   std::vector<double> e  (projector.ols_residual_vector());
   std::vector<double> e2 (e);
   for (unsigned int i=0; i<e.size(); ++i)
     e2[i] = e[i]*e[i];
   std::cout << "Residuals, with x4, x3" << std::endl
	     << e << std::endl 
     	     << projector.weighted_partial_residual_ss_vector(e2) << std::endl;

   sm.add_predictor(1, covMat.cross_products(1));
   std::cout << "Residuals, with x4, x3, x1" << std::endl
	     << projector.ols_residual_vector()
     	     << projector.weighted_partial_residual_ss_vector(wts) << std::endl;

   return 0;
}
    
