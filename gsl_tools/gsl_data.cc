/*
 *  gsl_data.cc
 *  seq_regr
 *
 *  Created by Robert Stine on 12/4/07.
 *  Copyright 2007. All rights reserved.
 *
 */

#include "gsl_data.h"
#include "debug.h"

#include <assert.h>
#include <iostream>


gsl_matrix* 
gslData::temp_mat(int nRows, int nCols) const
{
  assert((nRows < gslDataTempSize) && (nCols < gslDataTempSize));
  mTempMatView = gsl_matrix_view_array(mScratch,nRows,nCols);
  return &mTempMatView.matrix;
}

void
gslData::allocate(int nr, int nc, bool wts)
{
  debugging::debug("GSLD",0) << "Allocating " << nr << " rows and " << nc << " columns.\n";
  mXb      = gsl_vector_alloc(nr);
  mE       = gsl_vector_alloc(nr);
  mPermute = new int[nr];
  mX       = gsl_matrix_alloc(nr, nc);
  assert(mX != 0);
  mY       = gsl_vector_alloc(nr);
  if (wts) mWeights = gsl_vector_alloc(nr);
  else     mWeights = 0;
  mScratch = new double[gslDataTempSize*gslDataTempSize];
  for (int j=0; j< gslDataTempSize; ++j)
  { gsl_vector *v (gsl_vector_alloc(nr));
    if (v)
      mTempVec.push_back(v);
    else
      std::cerr << "GSLD: Error.  Cannot allocate vector to hold data.\n";
  }
}

void
gslData::free()
{
  debugging::debug("GSLD",0) << "Freeing memory.\n" ;
  if (mXb)      gsl_vector_free(mXb);
  if (mE)       gsl_vector_free(mE);
  if (mPermute) delete[] mPermute;
  if (mX)       gsl_matrix_free(mX);
  if (mY)       gsl_vector_free(mY);
  if (mWeights) gsl_vector_free(mWeights);
  if (mScratch) delete[] mScratch;
  for(unsigned int j=0; j<mTempVec.size(); ++j)
    gsl_vector_free(mTempVec[j]);
  debugging::debug("GSLD",0) << "Freeing memory completed.\n" ;
}
