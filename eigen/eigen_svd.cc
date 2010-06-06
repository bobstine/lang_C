#include "svd.h"

#include <iostream>

Eigen::MatrixXd
eigenSVD::standardize (Eigen::MatrixXd const& data) const
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
eigenSVD::make_random_linear_combinations (int k, Eigen::MatrixXd const& data) const
{
  Eigen::MatrixXd linComb (data.rows(),k);
  for (int j=0; j<k; ++j)
  { Eigen::VectorXd rand(Eigen::VectorXd::Random(data.cols()));
    linComb.col(j) = data * rand;
  }
  return linComb;
}


Eigen::MatrixXd
eigenSVD::operator()(Eigen::MatrixXd const& data) const
{
  // form m >= max(5, num components)
  int m (floor(sqrt(data.cols())));
  m = (m > mNumComponents) ? m : mNumComponents;  
  m = (m > 5) ? m : 5;
  std::cout << "TESTING: Given " << mNumComponents << " requested, returning " << m << std::endl;
  Eigen::MatrixXd linComb;
  if (mStandardize)
  { Eigen::MatrixXd sData (standardize(data));
    linComb = make_random_linear_combinations(m,sData);
  }
  else
    linComb = make_random_linear_combinations(m,data);
  // compute svd
  Eigen::SVD<Eigen::MatrixXd> svd(linComb);
  Eigen::MatrixXd u (svd.matrixU());
  Eigen::VectorXd s (svd.singularValues());
  std::cout << "ESVD : Singular values are {" << s.transpose() << "}\n";
  // return those with sing value > 0 (at least 1) or desired number
  int k (mNumComponents);
  if (0 == k)
  { k = 1;
    while (k < s.size() && s(k) > 1.0/sqrt(3.0) ++k;     // account for uniform variance
  }
  return u.block(0,0,u.rows(),k);
}
