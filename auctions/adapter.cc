/*
 *  adapter.cc
 *
 *  Created by Robert Stine on 2/22/08.
 *  Copyright 2008. All rights reserved.
 *
 */

#include "adapter.h"

// for copy
#include <algorithm>

#include <iostream>

#include "gsl_iterator.h"

//   GSL     GSL     GSL     GSL     GSL     GSL     GSL     GSL     GSL     GSL     GSL     GSL     GSL     GSL

gsl_matrix *
Convert::features_into_gsl_matrix(std::vector<Feature> const& v, int skipContextRows)
{
  const int k (v.size());
  const int n (v[0]->size()-skipContextRows);
  gsl_matrix *m (gsl_matrix_alloc(n,k));
  
  for (int j=0; j<k; ++j)
    std::copy (v[j]->begin()+skipContextRows, v[j]->begin() + v[j]->size(), begin(&gsl_matrix_column(m,j).vector));
  return m;
}


std::vector<Feature>
Convert::gsl_matrix_into_features(gsl_matrix const* mat, int addContextRows)
{
  std::vector<Feature> result;

  int nRows ((int)mat->size1 + addContextRows);
  for (int j=0; j<(int)mat->size2; ++j)
  { Column col("gsl", nRows); 
    double *data (col->begin() + addContextRows);
    std::fill(col->begin(), col->end(), 0.0);                          // fill context rows with 0
    for(int i=addContextRows; i<(int)col->size(); ++i)
      *data++ = gsl_matrix_get(mat,i,j);
    result.push_back(Feature(col));
  }
  return result;
}

//   Eigen     Eigen     Eigen     Eigen     Eigen     Eigen     Eigen     Eigen     Eigen     Eigen     Eigen     Eigen


Eigen::MatrixXd
Convert::features_into_eigen_matrix(std::vector<Feature> const& fv, int skipContextRows)
{
  const int k (fv.size());
  const int n (fv[0]->size()-skipContextRows);
  Eigen::MatrixXd mat(n,k);
  
  for (int j=0; j<k; ++j)
  { FeatureABC::Iterator it=fv[0]->begin() + skipContextRows;
    for (int i=0; i<n; ++i)
    { mat(i,j) = *it;
      ++it;
    }
  }
  return mat;
}


std::vector<Feature>
Convert::eigen_matrix_into_features(Eigen::MatrixXd const& mat, int addContextRows)
{
  std::vector<Feature> result;

  int nRows (mat.rows() + addContextRows);
  for (int j=0; j<mat.cols(); ++j)
  { Column col("eigen", nRows); 
    double *data (col->begin() + addContextRows);
    std::fill(col->begin(), col->end(), 0.0);                          // fill context rows with 0
    for(int i=addContextRows; i<mat.rows(); ++i)
      *data++ = mat(i,j);
    result.push_back(Feature(col));
  }
  return result;
}
