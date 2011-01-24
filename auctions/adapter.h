#ifndef _ADAPTER_H_
#define _ADAPTER_H_

/*
 *  adapter.h
 *
 *  Created by Robert Stine on 2/22/08.
 *  Copyright 2008. All rights reserved.
 *
 */

#include "features.h"

#include <Eigen/Array>

#include <functional>



namespace Convert
{
  // Convert features <--> gsl vectors  [ find in git repository prior to 10/15/2010 ]
  
  // Convert features <--> eigen vectors  
  
  Eigen::MatrixXd
    features_into_eigen_matrix(std::vector<Feature> const& fv, int skipContextRows);
  
  std::vector<Feature>
    eigen_matrix_into_features(Eigen::MatrixXd const& mat, std::string namePrefix, int addContextRows);
}


// Template class

template <class OP>
class EigenAdapter: public std::unary_function<FeatureVector, FeatureVector>
{
 private:
  OP          mOp;
  std::string mNamePrefix;
  int         mContextRows;
  
 public:
  typedef  std::vector<Feature> FeatureVector;

  EigenAdapter (OP op, std::string prefix, int rows) : mOp(op), mNamePrefix(prefix), mContextRows(rows) { }
  
  FeatureVector operator()(FeatureVector const& fv)
    {
      Eigen::MatrixXd  in  = Convert::features_into_eigen_matrix(fv, mContextRows);
      Eigen::MatrixXd  out = mOp(in);
      FeatureVector result = Convert::eigen_matrix_into_features(out, mNamePrefix, mContextRows);
      return result;
    }
};		    


#endif

