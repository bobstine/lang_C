/*
 *  gsl_kalman.test.cc
 *  seq_regr
 *
 *  Created by Robert Stine on 3/4/08.
 *  Copyright 2008. All rights reserved.
 *
 */

#include "gsl_kalman.h"

#include "gsl_utils.h"

#include <iostream>


double uniform() 
{
  return ((double)rand())/RAND_MAX;
}


int main()
{
  srand (1234);
  
  const int dimState (3);
  const int dimObs (2);
  const int T (100);

  gsl_matrix *F (gsl_matrix_calloc(dimState, dimState));
  gsl_matrix *G (gsl_matrix_calloc(dimState, dimState));
  gsl_matrix *H (gsl_matrix_calloc(dimObs, dimState));
  gsl_matrix_set (F, 0,0, .5); gsl_matrix_set(F,0,1,0.2); gsl_matrix_set(F,0,2,0.3);
  gsl_matrix_set (F, 1,0, 1.0);
  gsl_matrix_set (F, 2,1, 1.0);
  gsl_matrix_set_identity(G);
  gsl_matrix_set (H, 0,0,  1.0);
  gsl_matrix_set (H, 1,1,  1.0);
  
  gsl_matrix *Q  (gsl_matrix_alloc(dimState, dimState));
  gsl_matrix *R  (gsl_matrix_alloc(dimObs  , dimObs));
  gsl_matrix_set_identity (Q);
  gsl_matrix_set_identity (R);
  
  // initialize: x_0={0}, Poo = 100*I
  gsl_vector *state (gsl_vector_calloc(dimState)); 
  gsl_matrix *P00   (gsl_matrix_calloc(dimState,dimState)); 
  gsl_matrix_set_identity(P00);
  gsl_matrix_scale(P00,100);
  
  
  // build the Kalman filter
  KalmanFilter KF (dimState, dimObs, T, state, P00, Q, R);
  KF.set_parameters(F, G, H);
  std::cout << "TEST: Initialization of filter is completed\n";
  KF.write_parameters_to_stream(std::cout);
  KF.write_state_to_stream(std::cout, 0,2);
  
  // feed in random vector as observation 
  gsl_vector *obs   (gsl_vector_calloc(dimObs));  
  for(int t=1; t<=50; ++t)
  {
    for (int i=0; i<dimObs; ++i)
      gsl_vector_set(obs,i, uniform());
    KF.observe(obs);
  }
  KF.write_state_to_stream(std::cout, 0,50);

  return 0;
}