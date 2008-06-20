// $Id: covariance_matrix.test.cc,v 1.2 2003/05/30 16:10:48 bob Exp $

#include <iostream>
#include <string>

#include "datasets.h"

#include "covariance_matrix.h"


std::string fileName("test/covar.dat");


int main (void)
{
  NumericDataset data(fileName);
  CovarianceMatrix<NumericDataset> cov(data);

  for (int j=0; j<cov.dim(); ++j)
  { 
    CovarianceMatrix<NumericDataset>::Vector c (cov.row(j));
    for (int k=0; k<cov.dim(); ++k)
      std::cout <<  c[k] << " " ;
    std::cout << std::endl;
  }
  CovarianceMatrix<NumericDataset>::Vector v (cov.diagonal());
  std::cout << " -------------- " << std::endl
	    << "Diagonal: "  ;
  for (int k=0; k<cov.dim(); ++k)
    std::cout << v[k] << " ";
  std::cout << std::endl;
  
  return 0;
}
    
