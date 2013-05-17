#include "eigen_svd.h"

#include "random.h"
#include "functor.h"

#include <iostream>
#include <algorithm>
#include <functional>

#include <Eigen/Dense>


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
  Eigen::VectorXd xx (x.array() - mx);
  Eigen::VectorXd yy (y.array() - my);

  return xx.dot(yy)/sqrt(xx.dot(xx)*yy.dot(yy));
}



int main(int, char **)
{
  std::cout.precision(2);

  const int nRows (500);
  const int nCols (200);
  const int dim   (nRows * nCols);
  const int nKeep ( 20);          // for projected subspace, printing spectra

  // initialize random generator
  std::cout << "\nMaking a random generator\n";
  RandomGenerator rGen(33627 + rand());
  std::cout << "Initial state of generator: " << rGen << std::endl;

  // fill underlying noise data with N(0,1) then divide by root n for scaling
  std::cout << "\nFilling noise array...\n";
  double noiseArray[nRows*nCols];
  double rootn (sqrt(nRows));
  std::generate(noiseArray, noiseArray+dim, member_as_operator(rGen, &RandomGenerator::normal));
  std::transform(noiseArray, noiseArray+dim, noiseArray, [rootn](double x)->double { return x/rootn; });

  // map data into eigen matrix and find spectrum
  std::cout << "\nFinding noise spectrum...\n";
  Eigen::Map<Eigen::MatrixXd> noise (noiseArray,nRows,nCols);
  Eigen::JacobiSVD<Eigen::MatrixXd> svdNoise(noise);
  Eigen::VectorXd sNoise (svdNoise.singularValues().segment(0,nKeep));
  std::cout << "    Spectrum of noise SVD" << sNoise << std::endl;

  // build a random rotation matrix
  std::cout << "\nFilling subspace array...\n";
  double subspaceArray[nRows*nCols];
  std::generate(subspaceArray, subspaceArray+(nRows*nCols), member_as_operator(rGen, &RandomGenerator::normal));
  Eigen::Map<Eigen::MatrixXd> subspace (subspaceArray,nRows,nCols);

  std::cout << "\nSVD of subspace array...\n";
  Eigen::JacobiSVD<Eigen::MatrixXd> svdSubspace (subspace);

  // insert a low dimension subspace
  std::cout << "\nAdding noise to low-dim subspace...\n";
  Eigen::MatrixXd data = noise + 6. * svdSubspace.matrixU().col(0) * svdSubspace.matrixV().col(0).transpose();
  data +=                        2. * svdSubspace.matrixU().col(1) * svdSubspace.matrixV().col(1).transpose();
  data +=                        1.6* svdSubspace.matrixU().col(2) * svdSubspace.matrixV().col(2).transpose();
  
  // complete SVD
  std::cout << "\nSVD of data array...\n";
  Eigen::JacobiSVD<Eigen::MatrixXd> svdFull(data);
  Eigen::MatrixXd uFull (svdFull.matrixU());
  Eigen::VectorXd sFull (svdFull.singularValues().segment(0,nKeep));
  std::cout << "    Spectrum of complete SVD" << sFull << std::endl;

  // random projected SVD
  std::cout << "\nSVD of projected data array...\n";
  const bool standardize (false);
  Eigen::MatrixXd basis (eigenSVD(nKeep,standardize)(data));
  std::cout << "Correlation of U[0] with projection basis member = " << corr(uFull.col(0), basis.col(0)) << std::endl;
  std::cout << "Correlation of U[1] with projection basis member = " << corr(uFull.col(1), basis.col(1)) << std::endl;
  std::cout << "Correlation of U[2] with projection basis member = " << corr(uFull.col(2), basis.col(2)) << std::endl;

  return 0;
}

