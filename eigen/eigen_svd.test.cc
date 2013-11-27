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

  // check what eigen does for an identity that is not square
  {
    Eigen::MatrixXd x = Eigen::MatrixXd::Identity(6,3);
    std::cout << "\nTEST: Partial identity \n" << x << std::endl;
  }
  
  // check that householder produces orthogonal matrix
  {
    Eigen::MatrixXd x = Eigen::MatrixXd::Random(1000,10);
    // block does not work for Q matrix; use partial identity to get leading 5 cols
    Eigen::MatrixXd q = Eigen::HouseholderQR<Eigen::MatrixXd>(x).householderQ() * Eigen::MatrixXd::Identity(x.rows(),5);
    std::cout << "\nTEST: Leading q vector begins "        << q.col(0).head(6).transpose() << std::endl;
    std::cout << "TEST: q'q for leading 5 cols is \n"      << q.transpose() * q << std::endl << std::endl;
  }
  
  // form random matrix
  Eigen::MatrixXd data (Eigen::MatrixXd::Random(nRows,nCols));

  // let system choose number of components if normalized
  int nBasisElements (0);
  bool standardize   (true);
  Eigen::MatrixXd basis (PCA(nBasisElements,standardize)(data));
  std::cout << "\nTEST: Basis of data-determined count and standardized:\n" << basis.block(0,0,5,basis.cols()) << std::endl;

  // othrwise choose externally
  nBasisElements = 3;
  standardize = false;
  basis = PCA(nBasisElements,standardize)(data);
  std::cout << "Basis with 3 elements and not standardized:\n" << basis.block(0,0,5,basis.cols()) << std::endl;

  // try the RKHS code
  nBasisElements = 3;
  standardize = true;
  Eigen::MatrixXd rkhsBasis (RKHS<Kernel::Radial>(nBasisElements, standardize)(data));
  std::cout << "RKHS basis is sized " << rkhsBasis.rows() << " " << rkhsBasis.cols() << std::endl;
  std::cout << " begin           :\n" << rkhsBasis.block(0       ,0,10,basis.cols()) << std::endl;
  std::cout << " end             :\n" << rkhsBasis.block(nRows-10,0,10,basis.cols()) << std::endl;

  return 0;
}
