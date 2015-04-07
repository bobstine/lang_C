#include "eigen_svd.Template.h"

using debugging::debug;

//    PCA    PCA    PCA    PCA    PCA    PCA    PCA    PCA    PCA    PCA    PCA    PCA    PCA    PCA    PCA    PCA    PCA    PCA    

Eigen::MatrixXd
PCA::operator()(Eigen::MatrixXd const& data) const
{
  // m controls the number of random projections: at least 5 or number requested
  int m ((int)floor(sqrt((double)data.cols())));
  m = (m > mNumComponents) ? m : mNumComponents;  
  m = (m > 5) ? m : 5;
  // std::cout << "TESTING: Given " << mNumComponents << " requested, returning " << m << std::endl;
  Eigen::MatrixXd linComb;
  if (mStandardize)
  { Eigen::MatrixXd sData (SVD::standardize_columns(data, false));  // norm so x'x=1
    linComb = SVD::random_linear_combinations(sData,m);
  }
  else
    linComb = SVD::random_linear_combinations(data,m);
  // compute svd
  Eigen::JacobiSVD<Eigen::MatrixXd> svd(linComb, Eigen::ComputeThinU);
  Eigen::MatrixXd u (svd.matrixU());
  Eigen::VectorXd s (svd.singularValues());
  debug("EIGN",3) << "Singular values are {" << s.transpose() << "}" << std::endl;
  // return those with decent singular value (at least 1) if numComponents = 0 or desired number if positive
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


float
Kernel::Radial::operator()(Eigen::VectorXf const& x, Eigen::VectorXf const& y) const
{
  Eigen::VectorXf diff (x - y);
  return (float)exp(-0.5 * diff.squaredNorm()/mScale2);
}


double
Kernel::WeightedRadial::operator()(Eigen::VectorXd const& x, Eigen::VectorXd const& y) const
{
  Eigen::VectorXd wdiff ((x - y).cwiseQuotient(mWts));
  return exp(-0.5 * wdiff.squaredNorm());
}
