#ifndef _EIGEN_SVD_H_
#define _EIGEN_SVD_H_
/* 
 *  svd.h
 *
 *  Created by Robert Stine on 6/5/10.
 *  Copyright 2008. All rights reserved.

 Uses Eigen matrices, methods to build a random projection SVD
 of columns.  To select based on the relative size of the
 found eigenvalues, indicate that by selecting k=0 components.

 Protocol:
           Class creator has 2 arguments (int, bool)
           operator()
	      applies to a eigen::matrix
              returns selected number of singular vectors
 
 */

#include <Eigen/Array>
 

namespace SVD
{
  template<class EigenVec>
    typename EigenVec::Scalar
    mean (EigenVec const& x);

  template<class EigenVec>
    typename EigenVec::Scalar
    standard_deviation (EigenVec const& x);
  

  template<class EigenMatrix>
    EigenMatrix
    standardize_columns (EigenMatrix const& data, bool useSD=true);                  // if orthogonal, norms so X'X = 1

  template<class EigenMatrix>
    void
    standardize_columns_in_place (EigenMatrix& data, bool useSD=true);               

  
  template<class EigenMatrix>
    EigenMatrix
    sample_rows (EigenMatrix const& data, int nRows);

  template<class EigenMatrix>
    EigenMatrix
    random_linear_combinations (EigenMatrix const& data, int k);
}


//  PCA    PCA    PCA    PCA    PCA    PCA    PCA    PCA    PCA    PCA    PCA    PCA    PCA    

class PCA: public std::unary_function<Eigen::MatrixXd const&, Eigen::MatrixXd>
{
  int         mNumComponents;
  bool        mStandardize;
  
public:
  PCA(int k, bool standardize) : mNumComponents(k), mStandardize(standardize) { }
  
  int number_of_components() const { return mNumComponents; }
   
  result_type operator()(Eigen::MatrixXd const& data) const;
};



//  RKHS    RKHS    RKHS    RKHS    RKHS    RKHS    RKHS    RKHS    RKHS    RKHS    RKHS    RKHS    

namespace Kernel
{
  class Radial                                                                           // implement in single precision for faster distance calc
  {
    static std::string classname;
    float mScale2;                     // sigma^2
  public:
    
  Radial(float const& scale) : mScale2(scale*scale) {};   
    
    std::string const& name() const { return classname; }
    float operator()(Eigen::VectorXf const& a, Eigen::VectorXf const& b) const;
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
class RKHS: public std::unary_function<Eigen::MatrixXd const&, Eigen::MatrixXd>
{
  int     mNumComponents;
  bool    mStandardize;
  
public:
  RKHS(int k, bool standardize) : mNumComponents(k), mStandardize(standardize) { }
  
  int number_of_components() const { return mNumComponents; }
   
  result_type operator()(Eigen::MatrixXd const& data) const;
};

#include "eigen_svd.Template.h"

#endif
