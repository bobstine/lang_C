#ifndef _EIGEN_SVD_H_
#define _EIGEN_SVD_H_
/* 
 *  svd.h
 *
 *  Created by Robert Stine on 6/5/10.
 *  Copyright 2008. All rights reserved.

 Uses Eigen matrices, methods to build a random projection SVD
 of columns.

 Protocol:
           Class creator has 2 arguments (int, bool)
           operator()
	      applies to a eigen::matrix
              returns selected number of singular vectors
 
 */

#include <Eigen/Array>
 

namespace SVD
{
  double
    mean (Eigen::VectorXd const& x);
  double
    standard_deviation (Eigen::VectorXd const& x);
  
  Eigen::MatrixXd
    standardize_columns (Eigen::MatrixXd const& data, bool useSD=true);                  // if orthogonal, norms so X'X = 1

  Eigen::MatrixXd
    sample_rows (Eigen::MatrixXd const& data, int nRows);

  Eigen::MatrixXd
    random_linear_combinations (int k, Eigen::MatrixXd const& data);
}


//  PCA    PCA    PCA    PCA    PCA    PCA    PCA    PCA    PCA    PCA    PCA    PCA    PCA    

class pca: public std::unary_function<Eigen::MatrixXd const&, Eigen::MatrixXd>
{
  int         mNumComponents;
  bool        mStandardize;
  
public:
  pca(int k, bool standardize) : mNumComponents(k), mStandardize(standardize) { }
  
  int number_of_components() const { return mNumComponents; }
   
  result_type operator()(Eigen::MatrixXd const& data) const;
};



//  RKHS    RKHS    RKHS    RKHS    RKHS    RKHS    RKHS    RKHS    RKHS    RKHS    RKHS    RKHS    

namespace Kernel
{
  class Radial
  {
    static std::string classname;
    double mScale2;  // sigma^2
  public:
    
  Radial(double const& scale) : mScale2(scale*scale) {};   

    std::string const& name() const { return classname; }
    double operator()(Eigen::VectorXd const& a, Eigen::VectorXd const& b) const;
  };
  
  class WeightedRadial
  {
    static std::string classname;
    const Eigen::VectorXd mWts;
  public:
    WeightedRadial (Eigen::VectorXd wts) : mWts(wts) { };
    
    std::string const& name() const { return classname; }
    double operator()(Eigen::VectorXd const& a, Eigen::VectorXd const& b) const;
  };
  
  
  class Quadratic
  {
    static std::string classname;
  public:
    std::string const& name() const { return classname; }
    double operator()(Eigen::VectorXd const& a, Eigen::VectorXd const& b) const;
  };
  
  
  class L2
  {
    static std::string classname;
  public:
    std::string const& name() const { return classname; }
    double operator()(Eigen::VectorXd const& a, Eigen::VectorXd const& b) const;
  };
}
 

template <class K>
class rkhs: public std::unary_function<Eigen::MatrixXd const&, Eigen::MatrixXd>
{
  int     mNumComponents;
  bool    mStandardize;
  
public:
  rkhs(int k, bool standardize) : mNumComponents(k), mStandardize(standardize) { }
  
  int number_of_components() const { return mNumComponents; }
   
  result_type operator()(Eigen::MatrixXd const& data) const;
};

#include "eigen_svd.Template.h"

#endif
