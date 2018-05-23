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
  const size_t n (5);
  const int    df (10);
  std::vector<std::string> names(n);
  std::vector<double> est(n);
  std::vector<double> se(n);
  std::vector<double> pv(n);
  for (size_t i=0; i<n; ++i)
  { names[i] = "test";
    est[i] = 2*i;
    se[i] = (i+2)/2;
    pv[i] = 0.1;
    if (i>0)
    { names[i] = names[i] + " --- " + names[i-1];
      pv[i] = pv[i-1] * pv[i];
    }
  }
  print_stat_summary_table((int)se.size(), names.begin(), est.begin(), se.begin(), df, std::cout);

  print_stat_summary_table_in_html((int)se.size(), names.begin(), est.begin(), se.begin(), df, std::cout);

}

