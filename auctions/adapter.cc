/*
 *  adapter.cc
 *  auctions
 *
 *  Created by Robert Stine on 2/22/08.
 *  Copyright 2008. All rights reserved.
 *
 */

#include "adapter.h"


#include "range_ops.h"
#include "gsl_iterator.h"

gsl_matrix *
ConvertFeaturesIntoMatrix(std::vector<Feature> v)
{
  const int k (v.size());
  const int n (v[0]->size());
  gsl_matrix *m (gsl_matrix_alloc(n,k));    // Who cleans this up???
  
  for (int j=0; j<k; ++j)
    range_ops::copy (v[j]->range(), begin(&gsl_matrix_column(m,j).vector));
  return m;
}

