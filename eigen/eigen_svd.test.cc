#include "eigen_svd.h"

#include <iostream>

#include <Eigen/Array>
#include <Eigen/SVD>


int main(int, char *)
{
  std::cout.precision(2);

  int nRows (200);
  int nCols (30);

  // peek at an Eigen random vector... random on [-1,1]
  std::cout << "TEST: sample a random vector " << Eigen::VectorXd::Random(30).transpose() << std::endl;  // vecs are col
  
  // form random matrix
  Eigen::MatrixXd data (Eigen::MatrixXd::Random(nRows,nCols));

  // sensible to let system choose number of components if normalized
  int nBasisElements (0);
  bool standardize   (true);
  Eigen::MatrixXd basis (eigenSVD(nBasisElements,standardize)(data));
  std::cout << "Basis of data-determined count and standardized:\n" << basis.block(0,0,5,basis.cols()) << std::endl;

  // othrwise better choose externally
  nBasisElements = 3;
  standardize = false;
  basis = eigenSVD(nBasisElements,standardize)(data);
  std::cout << "Basis with 3 elements and not standardized:\n" << basis.block(0,0,5,basis.cols()) << std::endl;

  return 0;
}
