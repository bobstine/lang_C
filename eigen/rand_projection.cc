#include "eigen_svd.h"

#include "random.h"
#include "functor.h"

#include <iostream>
#include <algorithm>
#include <functional>

#include <Eigen/Array>
#include <Eigen/SVD>


std::ostream&
operator<< (std::ostream& os, Eigen::VectorXd const& x)
{
  os << "{";
  for (int i=0; i<x.size(); ++i)
    os << " " << x[i];
  os << "}";
  return os;
}


double
corr(Eigen::VectorXd const& x, Eigen::VectorXd const& y)
{
  double mx (x.sum()/x.size());
  double my (y.sum()/y.size());
  Eigen::VectorXd xx (x.cwise() - mx);
  Eigen::VectorXd yy (y.cwise() - my);

  return xx.dot(yy)/sqrt(xx.dot(xx)*yy.dot(yy));
}



int main(int, char **)
{
  std::cout.precision(2);

  const int nRows (300);
  const int nCols (100);

  const int nKeep ( 40);  // for subspace basis
  

  // initialize random generator
  std::cout << "Making a random generator\n";
  RandomGenerator rand(33627);
  std::cout << "Initial state of generator: " << rand << std::endl;

  // fill underlying data with N(0,1)
  double hiddenData[nRows*nCols];
  std::generate(hiddenData, hiddenData+(nRows*nCols), member_as_operator(rand, &RandomGenerator::normal));

  // map data into eigen matrix
  Eigen::Map<Eigen::MatrixXd> data (hiddenData,nRows,nCols);

  // complete SVD
  Eigen::SVD<Eigen::MatrixXd> svdFull(data);
  Eigen::MatrixXd uFull (svdFull.matrixU());
  Eigen::VectorXd sFull (svdFull.singularValues().segment(0,nKeep));
  std::cout << "Spectrum of complete SVD" << sFull << std::endl;
  

  // sensible to let system choose number of components if normalized
  bool standardize (false);
  Eigen::MatrixXd basis (eigenSVD(nKeep,standardize)(data));
  std::cout << "Correlation of U1 with itself              = " << corr(uFull.col(0), uFull.col(0)) << std::endl;
  std::cout << "Correlation of U1 with random basis member = " << corr(uFull.col(0), basis.col(0)) << std::endl;

  return 0;
}

