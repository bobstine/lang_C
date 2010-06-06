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
#include <Eigen/SVD>


class eigenSVD: public std::unary_function<Eigen::MatrixXd const&, Eigen::MatrixXd>
{
  int         mNumComponents;
  bool        mStandardize;
  
public:
  eigenSVD(int k, bool standardize) : mNumComponents(k), mStandardize(standardize) { }
  
  int number_of_components() const { return mNumComponents; }
   
  result_type operator()(Eigen::MatrixXd const& data) const;   // left singular vectors; prints associated singular values

 private:
  Eigen::MatrixXd make_random_linear_combinations (int k, Eigen::MatrixXd const& data) const;
  Eigen::MatrixXd standardize (Eigen::MatrixXd const& data) const;   // norms so that X'X = 1       
};


#endif
