/*
 *  print_utils.test.cc
 *  utils
 *
 *  Created by Robert Stine on 1/14/08.
 *  Copyright 2008. All rights reserved.
 *
 */


#include "print_utils.h"

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
  
  
}
