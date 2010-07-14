#include "eigen_svd.h"

#include <iostream>



//   SVD namespace     SVD namespace     SVD namespace     SVD namespace     SVD namespace     SVD namespace     SVD namespace     

double
SVD::mean (Eigen::VectorXd const& x)
{
  return x.sum()/x.size();
}

double
SVD::standard_deviation (Eigen::VectorXd const& x)
{
  Eigen::VectorXd centered (x.cwise() - mean(x));
  return centered.squaredNorm()/(x.size()-1);
}


Eigen::MatrixXd
SVD::sample_rows(Eigen::MatrixXd const& m, int nRows)
{
  Eigen::MatrixXd result (nRows,m.cols());
  int slotsLeft (nRows);
  int rowsLeft  (m.rows());
  while (rowsLeft && slotsLeft)
  {
    if ( ((double)(rand())/RAND_MAX) <  (((double)slotsLeft) / rowsLeft)  )
    { result.row(nRows-slotsLeft) = m.row(rowsLeft-1);
      --slotsLeft;
    }
    --rowsLeft;
  }
  return result;
}


Eigen::MatrixXd
SVD::standardize_columns (Eigen::MatrixXd const& data)
{
  Eigen::MatrixXd result (data.rows(), data.cols());
  Eigen::VectorXd mean (data.colwise().sum());
  // center cols, moving into result
  mean = mean / data.rows();
  for (int j=0; j<data.cols(); ++j)
    result.col(j) = data.col(j).cwise() - mean(j);
  // scale so that x'x = 1
  Eigen::VectorXd ss (data.colwise().squaredNorm());
  for (int j=0; j<data.cols(); ++j)
  { double sd (sqrt(ss(j)));
    result.col(j) = result.col(j) / sd;
  }
  return result;
}


Eigen::MatrixXd
SVD::random_linear_combinations (int k, Eigen::MatrixXd const& data)
{
  Eigen::MatrixXd linComb (data.rows(),k);
  Eigen::MatrixXd rand (Eigen::MatrixXd::Random(data.cols(), k));
  linComb = data * rand;
  return linComb;
}


//    PCA    PCA    PCA    PCA    PCA    PCA    PCA    PCA    PCA    PCA    PCA    PCA    PCA    PCA    PCA    PCA    PCA    PCA    

Eigen::MatrixXd
pca::operator()(Eigen::MatrixXd const& data) const
{
  // form m >= max(5, num components)
  int m (floor(sqrt(data.cols())));
  m = (m > mNumComponents) ? m : mNumComponents;  
  m = (m > 5) ? m : 5;
  // std::cout << "TESTING: Given " << mNumComponents << " requested, returning " << m << std::endl;
  Eigen::MatrixXd linComb;
  if (mStandardize)
  { Eigen::MatrixXd sData (SVD::standardize_columns(data));
    linComb = SVD::random_linear_combinations(m,sData);
  }
  else
    linComb = SVD::random_linear_combinations(m,data);
  // compute svd
  Eigen::SVD<Eigen::MatrixXd> svd(linComb);
  Eigen::MatrixXd u (svd.matrixU());
  Eigen::VectorXd s (svd.singularValues());
  std::cout << "ESVD : All singular values are {" << s.transpose() << "}\n";
  // return those with sing value > 0 (at least 1) or desired number
  int k (mNumComponents);
  if (0 == k)
  { k = 1;
    while ((k < s.size()) && (s(k) > 1.0/sqrt(3.0))) ++k;     // account for uniform variance
  }
  return u.block(0,0,u.rows(),k);
}



//   Kernel    Kernel    Kernel    Kernel    Kernel    Kernel    Kernel    Kernel    Kernel    Kernel    Kernel    Kernel    Kernel    
 
std::string Kernel::Radial::classname          ("Radial Kernel");
std::string Kernel::WeightedRadial::classname  ("Wtd Radial Kernel");
std::string Kernel::Quadratic::classname       ("Quadratic Kernel");
std::string Kernel::L2::classname              ("L2 Kernel");


double
Kernel::L2::operator()(Eigen::VectorXd const& x, Eigen::VectorXd const& y)  const
{
  return x.dot(y);
}


double
Kernel::Quadratic::operator()(Eigen::VectorXd const& x, Eigen::VectorXd const& y) const
{
  double l2 (1.0 + Kernel::L2()(x,y));
  return l2*l2;
}


double
Kernel::Radial::operator()(Eigen::VectorXd const& x, Eigen::VectorXd const& y) const
{
  Eigen::VectorXd diff (x - y);
  return exp(-diff.squaredNorm());
}


double
Kernel::WeightedRadial::operator()(Eigen::VectorXd const& x, Eigen::VectorXd const& y) const
{
  Eigen::VectorXd wdiff ((x - y).cwise()/mWts);
  return exp(-wdiff.squaredNorm());
}
