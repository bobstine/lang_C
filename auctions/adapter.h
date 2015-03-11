#ifndef _ADAPTER_H_
#define _ADAPTER_H_

/*
 *  adapter.h
 *
 *  Created by Robert Stine on 2/22/08.
 *  Copyright 2008. All rights reserved.
 *
 */

#include "auction_base_types.h"

#include "features.h"

#include <Eigen/Dense>

#include <functional>



namespace Convert
{
  // Convert features <--> gsl vectors  [ find in git repository prior to 10/15/2010 ]
  
  // Convert features <--> eigen vectors  
  
  MATRIX
    features_into_eigen_matrix(std::vector<Feature> const& fv, int skipContextRows);
  
  std::vector<Feature>
    eigen_matrix_into_features(MATRIX const& mat, std::string namePrefix, int addContextRows);
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
      MATRIX  in  = Convert::features_into_eigen_matrix(fv, mContextRows);
      MATRIX  out = mOp(in);
      FeatureVector result = Convert::eigen_matrix_into_features(out, mNamePrefix, mContextRows);
      return result;
    }
};		    


#endif

