// $Id: cache.test.cc,v 3.1 2005/09/02 02:38:42 bob Exp $

#include "cache.h"

#include "column.h"
#include "anonymous_iterator.h"

#include <iostream>

// Yukko.  Need to find a place for the cache in a .cc file

// ? what's this do? std::map<Column const*, double> Cache<Column const, double>::sCache;

int
main ()
{
  const int n (10);
  
  // define data with name string and vector of numbers
  std::string nameX("x");
  std::string nameY("y");
  double*     x = new double[n];
  double*     y = new double[n];
  for (int i=0; i<n; ++i)
  { x[i] = i;
    y[i] = 2 * i;
  }
  
  // make columns
  Column  xColumn  (nameX.c_str(), x, x+n);
  Column  yColumn  (nameY.c_str(), y, y+n);
   
  // make cache
  Cache<const Column, double> centerCache = cache_member_function(&Column::average);

  // call cache function
  std::cout << centerCache(&xColumn) << std::endl;
  std::cout << centerCache(&yColumn) << std::endl;
  
  // make another cache.  it will not recompute
  Cache<const Column, double> centerCache2 = cache_member_function(&Column::average);
  std::cout << centerCache2(&xColumn) << std::endl;
  std::cout << centerCache2(&yColumn) << std::endl;

  return 0;
}

