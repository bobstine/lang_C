/*
 *  gsl_kalman.cc
 *  seq_regr
 *
 *  Created by Robert Stine on 3/4/08.
 *  Copyright 2008. All rights reserved.
 *
 */

#include "gsl_kalman.h"

#include "gsl_utils.h"

#include <assert.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_linalg.h>

#define DPRINT(label,item) std::cout << (label) << (item)


void 
KalmanFilter::set_parameters (gsl_matrix const* F, gsl_matrix const* G, gsl_matrix const* H) 
{ 
  mF = F;
  mG = G;
  mH = H; 
}

gsl_vector const* 
KalmanFilter::state_vector (int t) const
{ 
  return &gsl_matrix_const_row(mX,t).vector; 
}


void
KalmanFilter::write_parameters_to_stream(std::ostream &os) const
{
  os  << "KALF: dim state is " << mDimState << " with " << mDimObs 
      << "-d observations for " << mLength << " periods.\n";
  os  << " F" << mF << " G" << mG << " H" << mH;
  os  << " Q" << mQ << " R" << mR;
}

void
KalmanFilter::write_state_to_stream(std::ostream &os, int first, int last) const
{
  os << "KALF: State array from t = " << first << "..." << last << "\n";
  for (int t=first; t<=last; ++t)
  {
    os << "[" << t << "] ";
    for(int j=0; j<mDimState; ++j)
      os << gsl_matrix_get(mX,t,j) << " ";
    os << std::endl;
  }
}


void 
KalmanFilter::allocate()
{ 
  mX = gsl_matrix_calloc(1+mLength,mDimState);  // x0 is initial state
  mY = gsl_matrix_calloc(mLength  ,mDimObs);
  mP = gsl_matrix_calloc(mDimState, mDimState);
  mKt= gsl_matrix_calloc(mDimObs  , mDimState);
  mZ = gsl_matrix_calloc(mLength  , mDimObs);
  mS = gsl_matrix_calloc(mDimObs  , mDimObs);
  mQ = gsl_matrix_calloc(mDimState, mDimState);
  mR = gsl_matrix_calloc(mDimObs  , mDimObs);
  int mx ((mDimState > mDimObs) ? mDimState : mDimObs);
  mT1 = gsl_matrix_calloc(mx, mx);
  mT2 = gsl_matrix_calloc(mx, mx);
  mT3 = gsl_matrix_calloc(mx, mx);
  mTV = gsl_vector_calloc(mx);
  assert(mT2 != 0);
}

void
KalmanFilter::free() 
{
  if (mX)  gsl_matrix_free(mX);
  if (mY)  gsl_matrix_free(mY);
  if (mP)  gsl_matrix_free(mP);
  if (mKt) gsl_matrix_free(mKt);
  if (mZ)  gsl_matrix_free(mZ);
  if (mS)  gsl_matrix_free(mS);
  if (mQ)  gsl_matrix_free(mQ);
  if (mR)  gsl_matrix_free(mR);
  if (mT1) gsl_matrix_free(mT1);
  if (mT2) gsl_matrix_free(mT2);
  if (mT3) gsl_matrix_free(mT3);
  if (mTV) gsl_vector_free(mTV);
}

void
KalmanFilter::insert(gsl_vector const* x, gsl_matrix *m, int row)
{
  gsl_vector_memcpy(&gsl_matrix_row(m,row).vector, x);
}

void 
KalmanFilter::observe (gsl_vector const* y)
{
  // advance to t|t-1
  gsl_vector *xt(&gsl_matrix_row(mX,mTime).vector);
  gsl_matrix *FP(&gsl_matrix_submatrix(mT1,0,0,mDimState,mDimState).matrix);
  gsl_matrix_multiply_Ab(mF, &gsl_matrix_row(mX,mTime-1).vector, xt);// F x_{t-1}
  gsl_matrix_multiply_AS(mF, mP, FP);                                // F P 
  gsl_matrix_multiply_ABt(FP, mF, mP);                               // F P F'
  gsl_matrix_add(mP, mQ);                                            // F P F' + Q 
  DPRINT("=== FPF' + Q ", mP);
  // compute innovation z
  gsl_vector *hx (&gsl_vector_subvector(mTV,0,mDimObs).vector);
  gsl_vector *z  (&gsl_matrix_row(mZ,mTime).vector);
  gsl_vector_memcpy(z, y);
  gsl_matrix_multiply_Ab(mH, xt, hx);                                // H x 
  gsl_vector_sub(z, hx);                                             // y - H x
  DPRINT("=== y-Hx", z);
  // compute innovation variance
  gsl_matrix *HP (&gsl_matrix_submatrix(mT1,0,0,mDimObs,mDimState).matrix);
  gsl_matrix_multiply_AS(mH, mP, HP);                                // H P 
  gsl_matrix_multiply_ABt(HP, mH, mS);                               // H P H'
  gsl_matrix_add(mS, mR);                                            // F P F' + R
  // compute gain 
  gsl_matrix *Si (&gsl_matrix_submatrix(mT2,0,0,mDimObs, mDimObs).matrix);
  DPRINT("=== S ", mS);
  gsl_matrix_inverse(mS,Si);
  gsl_matrix_multiply_SA(Si, HP, mKt);                               // Si H P  (transpose of gain)
  DPRINT("=== Kt ", mKt);
  // update state and covariance
  gsl_blas_dgemv(CblasTrans, 1.0, mKt, z, 1.0, xt);                  // x + K y
  gsl_matrix *KH (&gsl_matrix_submatrix(mT1, 0,0, mDimState, mDimState).matrix);
  gsl_matrix_multiply_AtB(mKt, mH, KH);
  gsl_matrix_scale(KH,-1.0);
  for (int j=0; j<mDimState; ++j)                                    // I-K H
    gsl_matrix_set(KH,j,j,1.0+gsl_matrix_get(KH,j,j));
  DPRINT("=== I-KH", KH);
  gsl_matrix *P (&gsl_matrix_submatrix(mT2,0,0,mDimState, mDimState).matrix);
  gsl_matrix_memcpy(P,mP);
  gsl_matrix_multiply_AS(KH, P, mP);                                 // (I-K H)P
  DPRINT("=== Final P\n", mP);
  ++mTime;
}
  
  
  
  
  
  
