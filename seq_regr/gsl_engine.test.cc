/*
 *  gsl_engine.test.cc
 *  seq_regr
 *
 *  Created by Robert Stine on 1/6/08.
 *  Copyright 2008. All rights reserved.
 *
 */

#include "gsl_engine.h"


// printing facilities
#include "gsl_utils.h"
// random number generator
#include "random.h"


#include <iostream>


int
main (void)
{
  const int length (100);
  
  // make response related to first predictor; echo data file to output
  std::cout << "TEST: Making a uniform generator \n";
  MarsagliaGenerator uni1(17);
  MarsagliaGenerator uni2(3837);
  
  gsl_vector* x (gsl_vector_alloc(length));
  gsl_vector* y (gsl_vector_alloc(length));
  gsl_vector* w (gsl_vector_alloc(length));
  
  for (int i=0; i<length; ++i)
  { gsl_vector_set(w,i, (i+1) * 0.10);
    gsl_vector_set(x,i, i);
    if ( length*uni1() < i )  gsl_vector_set(y,i,1.0);
    else                      gsl_vector_set(y,i,0.0);
  }
  
  olsEngine engine (length);  // reads size from vector
  
  double yBar (engine.average(y));
  std::cout << "TEST: average y = " << yBar; 
  double ssY  (engine.sum_of_squares(y,yBar));
  std::cout << " with sum of squares  SS = " << ssY << std::endl;

  gsl_vector* smth (gsl_vector_alloc(length));
  int df (5);
  double ssSmth (engine.smooth(df, x, y, smth));
  double sBar (engine.average(smth));
  std::cout << "TEST: average smth = " << sBar; 
  double ssS  (engine.sum_of_squares(smth, sBar));
  std::cout << " with sum of squares  SS = " << ssS << std::endl;
  std::cout << "TEST: Smooth vector is \n:" << smth << std::endl;

  std::cout << std::endl;
  
  return 0;
}

