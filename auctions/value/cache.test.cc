// $Id: cache.test.cc,v 3.0 2004/11/19 18:58:36 foster Exp $

#include "cache.h"

#include "column.h"
#include "anonymous_iterator.h"

#include <iostream>

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
  std::cout << xColumn << std::endl << yColumn << std::endl;
   
  // make a feature from a column, add some attributes
  ColumnFeature xf (xColumn);

  // build the feature map
  double c;
  c = Features::Center()(&xf); std::cout << "TEST: feature center = " << c<< std::endl;
  c = Features::Center()(&xf); std::cout << "TEST: feature center = " << c<< std::endl;
  c = Features::Center()(&xf); std::cout << "TEST: feature center = " << c<< std::endl;


  return 0;
}

