/* $Id: gsl_kalman.h,v 1.2 2008/03/06 03:58:42 bob Exp $
 *  gsl_kalman.h
 *  seq_regr
 *
 *  Created by Robert Stine on 3/4/08. Copyright 2008. All rights reserved.
 */

/*
 Code would benefit from...
 
 Not using the matrix G in the state equation; just using Q for variance.
 
 Recognize when the covariance matrix P has reached steady state, avoiding
 inverse calculations.  Of course, only possible when F, G, H remain fixed.
 
 Tigher matrix calculations, some efficiencies here rather than following
 the usual matrix/derivation expressions directly.
 
*/

#ifndef _GSL_KALMAN_H_
#define _GSL_KALMAN_H_

#include <gsl/gsl_matrix.h>

#include <iostream>

class KalmanFilter 
{
  const int mDimState;
  const int mDimObs;
  const int mLength;     // max number of time periods to observe
  int mTime;             // current time position
  gsl_matrix *mX;        // matrix of states,       one row per time period (row zero is initial)
  gsl_matrix *mY;        // matrix of observations, one row per time period (row zero empty)
  gsl_matrix *mP;        // covariance of state
  gsl_matrix *mKt;       // gain transpose
  gsl_matrix *mZ;        // matrix of innovations,  one row per time period
  gsl_matrix *mS;        // covariance matrix of current innovations
  
  gsl_matrix const* mF;  // held externally
  gsl_matrix const* mG;
  gsl_matrix const* mH;
  
  gsl_matrix *mQ;        // covariance matrix of noise in state equation
  gsl_matrix *mR;        // covariance matrix of noise in obs equation
  
  gsl_matrix *mT1, *mT2, *mT3; // temp space
  gsl_vector *mTV;
  
public:
    ~KalmanFilter() { free(); }
  
  KalmanFilter (int dimState, int dimObs, int length, gsl_vector const* x0, gsl_matrix const* P0, gsl_matrix const* Q, gsl_matrix const* R)
    : mDimState(dimState), mDimObs(dimObs), mLength(length), mTime(1) 
  { allocate(); insert(x0,mX,0); gsl_matrix_memcpy(mP,P0); gsl_matrix_memcpy(mQ, Q); gsl_matrix_memcpy(mR, R); }
  
  void set_parameters (gsl_matrix const* F, gsl_matrix const* G, gsl_matrix const* H);

  void observe (gsl_vector const* y);

  gsl_vector const* state_vector (int t) const;
  
  void write_parameters_to_stream (std::ostream &os) const;
  void write_state_to_stream(std::ostream &os, int first, int last) const;

private:
  void allocate();
  void free();
  void insert(gsl_vector const* x, gsl_matrix *m, int row);
};

inline
std::ostream &
operator<<(std::ostream &os, KalmanFilter const& kf)
{
  kf.write_parameters_to_stream(os);
  return os;
}

#endif