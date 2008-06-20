// $Id: covariance_matrix.h,v 1.2 2003/05/30 16:10:48 bob Exp $

/*
  30 May 03 ... Use dataset object, ranges.
  22 May 03 ... Created as feeder to the sequential regression code.
*/

#ifndef _COVARIANCE_MATRIX_H_
#define _COVARIANCE_MATRIX_H_

#include <iostream>
#include <vector>

#include "datasets.h"

#include "range.h"
#include "range_traits.h"
#include "make_range.h"


template <class Data>
class CovarianceMatrix {
  int  mDim;
  Data const& mData;
    
 public:

  typedef std::vector<double> Vector;
  
  CovarianceMatrix (Data const& data)
    : mDim(data.n_cols()), mData(data) {}

  int dim() const { return mDim; }
  int n_observations() const { return mData.n_rows(); }
  
  Vector
    row (int j) const
    {
      Vector cp (cross_products(j));
      for (int k=0; k<mDim; ++k)
	cp[k] = cp[k]/(n_observations() - 1);
      return cp;
    }

  Vector
    cross_products (int j) const
    {
      Vector cp (mDim, 0.0);
      for (int i=0; i<mData.n_rows(); ++i)
      { typename Data::CenteredDataRange devs (mData.centered_row(i));
	double                           devj (*(begin(devs)+j));
	for (int k=0; k<mDim; ++k)
	  cp[k] += devj * *(begin(devs)+k);
      }
      return cp;
    }

    
  Vector
    diagonal () const
    {
      Vector ss (sums_of_squares ());
      for (int k=0; k<mDim; ++k)
	ss[k] = ss[k]/(n_observations() - 1);
      return ss;
    }


  Vector
    sums_of_squares () const
    {
      Vector ss (mDim, 0.0);
      for (int i=0; i<mData.n_rows(); ++i)
      { typename Data::CenteredDataRange devs (mData.centered_row(i));
	for (int k=0; k<mDim; ++k)
	{ double  dev (*(begin(devs)+k));
	  ss[k] += dev * dev;
	}
      }
      return ss;
    }
};

#endif
