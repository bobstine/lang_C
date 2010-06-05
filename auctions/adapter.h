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

#include <gsl/gsl_matrix.h>
#include <Eigen/Array>

#include <functional>



namespace Convert
{
  // Convert features <--> gsl vectors
  
  gsl_matrix *
    features_into_gsl_matrix(std::vector<Feature> const& fv, int skipContextRows);
  
  std::vector<Feature>
    gsl_matrix_into_features(gsl_matrix const* mat, int addContextRows);

  
  // Convert features <--> eigen vectors  
  
  Eigen::MatrixXd
    features_into_eigen_matrix(std::vector<Feature> const& fv, int skipContextRows);
  
  std::vector<Feature>
    eigen_matrix_into_features(Eigen::MatrixXd const& mat, int addContextRows);
}


// Template class

template <class OP>
class GSL_adapter
{
 private:
  OP mOp;
  int mContextRows;
  
 public:
  typedef  std::vector<Feature> FeatureVector;

  GSL_adapter (OP op, int rows) : mOp(op), mContextRows(rows) { }
  
  FeatureVector operator()(FeatureVector const& fv)
    {
      gsl_matrix *in  = Convert::features_into_gsl_matrix(fv, mContextRows);
      //      std::cout << "ADPT: Converting (" << fv[0]->size() << "," << fv.size() << ") into (" << in->size1 << "," << in->size2 << ") for GSL.\n";
      gsl_matrix *out = mOp(in);
      FeatureVector result (Convert::gsl_matrix_into_features(out, mContextRows));
      gsl_matrix_free(in);
      gsl_matrix_free(out);
      return result;
    }
};		    



template <class OP>
class Eigen_adapter
{
 private:
  OP mOp;
  int mContextRows;
  
 public:
  typedef  std::vector<Feature> FeatureVector;

  Eigen_adapter (OP op, int rows) : mOp(op), mContextRows(rows) { }
  
  FeatureVector operator()(FeatureVector const& fv)
    {
      Eigen::MatrixXd in  = Convert::features_into_eigen_matrix(fv, mContextRows);
      Eigen::MatrixXd out = mOp(in);
      FeatureVector result (Convert::eigen_matrix_into_features(out, mContextRows));
      return result;
    }
};		    


#endif

