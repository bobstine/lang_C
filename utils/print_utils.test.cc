/*
 *  print_utils.test.cc
 *  utils
 *
 *  Created by Robert Stine on 1/14/08.
 *  Copyright 2008. All rights reserved.
 *
 */


#include "print_utils.h"
#include "gsl/gsl_vector.h"


#include <vector>
#include <iostream>


int 
main()
{
  const int n (5);
  
  std::vector<double> est(n);
  std::vector<double> se(n);
  for (int i=0; i<n; ++i)
  { est[i] = i;
    se[i] = i;
  }
  print_stat_summary_table(se.size(), est.begin(), se.begin(), std::cout);
  
  gsl_vector *v1 (gsl_vector_alloc(n));
  gsl_vector *v2 (gsl_vector_alloc(n));
  for (int i=0; i<n; ++i)
  { gsl_vector_set(v1,i,i);
    gsl_vector_set(v2,i,2*i);
  }
  print_stat_summary_table(se.size(), begin(v1), begin(v2), std::cout);
  
}
