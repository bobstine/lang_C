#include "eigen_svd.h"

#include <iostream>


int main(int, char **)
{
  std::cout.precision(2);

  int nRows (200);
  int nCols (30);

  // peek at an Eigen random vector... random on [-1,1]
  {
    Eigen::VectorXd x (Eigen::VectorXd::Random(30));
    std::cout << "TEST: sample a random vector " << x.transpose() << std::endl;  // vecs are columns
    std::cout << "TEST: <x,x> = " << x.dot(x) << " with norm squared = " << x.squaredNorm() << std::endl;
  }
    
  // form random matrix
  Eigen::MatrixXd data (Eigen::MatrixXd::Random(nRows,nCols));

  // sensible to let system choose number of components if normalized
  int nBasisElements (0);
  bool standardize   (true);
  Eigen::MatrixXd basis (pca(nBasisElements,standardize)(data));
  std::cout << "Basis of data-determined count and standardized:\n" << basis.block(0,0,5,basis.cols()) << std::endl;

  // othrwise better choose externally
  nBasisElements = 3;
  standardize = false;
  basis = pca(nBasisElements,standardize)(data);
  std::cout << "Basis with 3 elements and not standardized:\n" << basis.block(0,0,5,basis.cols()) << std::endl;

  // try the RKHS code
  Eigen::MatrixXd rkhsBasis (rkhs<Kernel::Radial>(nBasisElements, standardize)(data));
  std::cout << "Basis of RKHS with data-determined count and standardized:\n" << rkhsBasis.block(0,0,5,basis.cols()) << std::endl;

  return 0;
}
