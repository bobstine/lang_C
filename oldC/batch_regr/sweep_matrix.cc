// $Id: sweep_matrix.cc,v 1.7 2003/08/12 21:15:59 bob Exp $

/*
   22 May 03 ... Start of porting to sequence code.
*/
 
#include "sweep_matrix.h"

#include <iostream>
#include <functional>
#include <algorithm>

namespace {

  inline double sqr(double x) { return x*x; };
  
  inline double min(double x, double y) { return x<y ? x : y; };
  inline int min_int(int x, int y) { return x<y ? x : y; };
  
  inline double abs(double x) { return x>0 ? x : -x; };
}

// -------------------------------------------------------------------------

void
IndexedVector::replace_contents_from (IndexedVector const& iv)
{
  mIndex = iv.mIndex;
  for (int i=0; i<size(); ++i)
    mVector[i] = iv[i];
}
      
void
IndexedVector::convert_to_pivot_form ()  // divide by constant and set diagonal
{
  double pivot(diagonal());
  if (pivot <  SWEEP_NEAR_ZERO)
    std::clog << "  IV: Pivot " << mIndex << " near zero." << std::endl;
  else
  { for (int i=0; i<size(); ++i)
      mVector[i] = mVector[i]/pivot;
    mVector[mIndex] = -1.0/pivot;
  }
}

void
IndexedVector::sweep_out (IndexedVector const& kv)
{
  int k (kv.mIndex);
  // std::clog << "  IV: Sweeping var " << k << " from " << mIndex << std::endl;
  double vjk (mVector[k]);
  for (int i=0; i<size(); ++i)
    mVector[i] -=  vjk * kv[i];
  mVector[k] = - vjk * kv[k];
}

void
IndexedVector::write_to(std::ostream& os, int maxPrint) const
{
  os << " iv {" << mIndex << ", [ " ;
  for (int i=0; i<min_int(maxPrint, mVector.size()); ++i)
    os << mVector[i] << " ";
  os << "] " << mVector.size() << "}" ;
}

// -------------------------------------------------------------------------
	
void
SweepMatrix::initialize(Vector const& cp)
{
  std::clog << "SWPM: Initializing skip vector with " << cp.size() << " columns." << std::endl;
  // set up the skip vector to skip Y
  fill(mSkipVector.begin(), mSkipVector.end(), false);
  mSkipVector[0] = true;
  mNSkip = 1;
  //  initialize covs for sweep matrix
  mIncrSweepMat.push_back(IndexedVector(0,cp));
  mFullSweepMat.push_back(IndexedVector(0,cp));
}


void
SweepMatrix::mark_constant_columns ()
{
#ifndef NDEBUG
  std::clog << "SWPM: Marking constant columns." << std::endl;
#endif
  for (unsigned int i=1; i<mSkipVector.size(); ++i) // skip y in col 0
  {
    if ((false == mSkipVector[i]) &&
	(mResidualSS[i] < SWEEP_NEAR_ZERO))
    {
      mSkipVector[i] = true;
      ++mNSkip;
    }
  }
}


void
SweepMatrix::sweep_last_row()     // Remove previous predictors from this one
{
  // j denotes index associated with latest row added to sweep
  int newRow (mIncrSweepMat.size()-1);
  int j (mIncrSweepMat[newRow].mIndex);
  // Skip j in the future
  if (mSkipVector[j])
    std::clog << "SWPM: ERROR. Sweeping on previously swept column" << j << std::endl;
  else
  { mSkipVector[j] = true;
    ++mNSkip;
  }
  // sweep out prior variables, excluding y in row 0
  // prior rows are held in sweep pivot form (remove prior "rows" from "new")
  for (int row=1; row<newRow; ++row)
    mIncrSweepMat[newRow].sweep_out(mIncrSweepMat[row]); 
  // convert new row to sweep/pivot form
  mIncrSweepMat[newRow].convert_to_pivot_form();
  // adjust residual SS
  adjust_rss (mIncrSweepMat[newRow]);
  // sweep new row out of Y in incremental
  mIncrSweepMat[0].sweep_out(mIncrSweepMat[newRow]);
  // sweep new row from all of the fully swept array
  mFullSweepMat[0].replace_contents_from(mIncrSweepMat[0]);
  for (int row = 1; row<newRow; ++row)
    mFullSweepMat[row].sweep_out(mIncrSweepMat[newRow]);
  // Copy latest row to the fully swept matrix
  mFullSweepMat.push_back(mIncrSweepMat[newRow]);
}

void
SweepMatrix::adjust_rss (IndexedVector const& CPj)
{
  double pivot(CPj.diagonal());  // already in pivot form
  mResidualSS[0] += sqr(CPj[0])/pivot;
  for (int i=1; i < CPj.size(); ++i)
    if (false == mSkipVector[i])
      mResidualSS[i] += sqr(CPj[i])/pivot;
  // set associated diagonal value held in residual CP to neg of pivot
  mResidualSS[CPj.mIndex] = -pivot;
}


// ADD_PREDICTOR  ADD_PREDICTOR  ADD_PREDICTOR  ADD_PREDICTOR  ADD_PREDICTOR  ADD_PREDICTOR  

double
SweepMatrix::add_predictor(int j, Vector const& cov, bool forced)
{	
  // mark columns with no variation prior to search
  mark_constant_columns();
  // add next predictor from input list
  IndexedVector newcov(j,cov);
  mIncrSweepMat.push_back(newcov);
  // Sweep out effects from row just added
  sweep_last_row();
  if (forced) ++mQBase;
  return mIncrSweepMat[0][0]; // final RSS
}

//  FIT PROPERTIES  FIT PROPERTIES  FIT PROPERTIES  FIT PROPERTIES  FIT PROPERTIES  FIT PROPERTIES  

double
SweepMatrix::RSS () const
{
  return mResidualSS[0];
}

double
SweepMatrix::r_squared() const
{
  return 1.0 - RSS()/mRawSS[0];
}

bool
SweepMatrix::skip(int j) const
{
  return mSkipVector[j];
}

int
SweepMatrix::number_of_predictors () const
{
  return mIncrSweepMat.size()-1;
}

int
SweepMatrix::number_of_variables() const
{
  return mRawSS.size();
}

Vector
SweepMatrix::slopes() const
{
  Vector beta;
  for (int j=1; j<=number_of_predictors(); ++j)
    beta.push_back (mIncrSweepMat[0][mIncrSweepMat[j].mIndex]);
  return beta;
}

double
SweepMatrix::intercept() const
{
  double b0 (mAvg[0]);
  for (int j=1; j <=number_of_predictors(); ++j)
  {
    int k (mIncrSweepMat[j].mIndex);
    b0 -= mIncrSweepMat[0][k] * mAvg[k];
  }
  return b0;
}

std::vector<int>
SweepMatrix::predictors  () const
{
  std::vector<int> indices;
  for (int j=1; j<=number_of_predictors(); ++j)
    indices.push_back(mIncrSweepMat[j].mIndex);
  return indices;
}


//  WRITE_TO  WRITE_TO  WRITE_TO  WRITE_TO  WRITE_TO  WRITE_TO  WRITE_TO  WRITE_TO  WRITE_TO  WRITE_TO

void
SweepMatrix::write_to (std::ostream& os, int maxPrint) const
{
  os << std::endl
     << "  ---------------  Sweep Matrix  ----------------- " << std::endl
     << "    q = " << number_of_predictors() << std::endl
     << "  RSS = " << RSS()
     << "  TSS = " << mRawSS[0] << "     R2 = " << r_squared() << std::endl
     << "  Skip " ;
  for (int i=0; i<min_int(maxPrint, mSkipVector.size()); ++i)
    if (mSkipVector[i]) os << "1 "; else os << "0 ";
  os << std::endl
     << "  ------------------------------------------------ " << std::endl
     << "  RSS " ;
  for (int i=0; i<min_int(maxPrint, mResidualSS.size()); ++i)
    os << mResidualSS[i] << " ";
  os << std::endl;
  for (unsigned int i=0; i<mIncrSweepMat.size(); ++i)
    os << "  [" << i << "]" << mIncrSweepMat[i] << std::endl;
  os << "  ------------------------------------------------ " << std::endl;
  for (unsigned int i=0; i<mFullSweepMat.size(); ++i)
    os << "  [" << i << "]" << mFullSweepMat[i] << std::endl;
  os << "  ------------------------------------------------ " << std::endl
     << std::endl;
}



