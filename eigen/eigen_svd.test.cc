#include "eigen_svd.h"
#include "debug.h"
 
#include <iostream>

int main(int, char **)
{
  debugging::debug_init(std::cout, 5);

  std::cout.precision(2);

  int nRows (10000);   // needs to be more than 400 to test distance matrix calculations
  int nCols (   32);
 
  // peek at an Eigen random vector... random on [-1,1]
  {
    Eigen::VectorXd x (Eigen::VectorXd::Random(30));
    std::cout << "TEST: sample a random vector " << x.transpose() << std::endl;  // vecs are columns
    std::cout << "TEST: <x,x> = " << x.dot(x) << " with norm squared = " << x.squaredNorm() << std::endl;
  }
    
  // form random matrix
  Eigen::MatrixXd data (Eigen::MatrixXd::Random(nRows,nCols));

  // let system choose number of components if normalized
  int nBasisElements (0);
  bool standardize   (true);
  Eigen::MatrixXd basis (pca(nBasisElements,standardize)(data));
  std::cout << "Basis of data-determined count and standardized:\n" << basis.block(0,0,5,basis.cols()) << std::endl;

  // othrwise choose externally
  nBasisElements = 3;
  standardize = false;
  basis = pca(nBasisElements,standardize)(data);
  std::cout << "Basis with 3 elements and not standardized:\n" << basis.block(0,0,5,basis.cols()) << std::endl;

  // try the RKHS code
  nBasisElements = 3;
  standardize = true;
  Eigen::MatrixXd rkhsBasis (rkhs<Kernel::Radial>(nBasisElements, standardize)(data));
  std::cout << "RKHS basis is sized " << rkhsBasis.rows() << " " << rkhsBasis.cols() << std::endl;
  std::cout << " begin           :\n" << rkhsBasis.block(0       ,0,10,basis.cols()) << std::endl;
  std::cout << " end             :\n" << rkhsBasis.block(nRows-10,0,10,basis.cols()) << std::endl;

  return 0;
}
