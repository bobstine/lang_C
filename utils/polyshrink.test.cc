/*
 *  polyshrink.test.cc
 *  utils
 *
 *  Created by Robert Stine on 11/9/07.
 *  Copyright 2007. All rights reserved.
 *
 */

#include "polyshrink.h"

#include <iostream>
#include <vector>
#include <algorithm>

int main(void)
{
  const int size (13);
  typedef std::vector<double>::iterator VecIter;
  
  std::vector<double> x(size);
  std::vector<double> y(size);
  
  std::ostream_iterator<double> output_stream (std::cout," ");

  // 2 to 10
  for (int i=0; i<size; ++i) 
    x[i] = (double) i - 2;
  std::cout << "\n\nInput vector: \n";
  std::copy(x.begin(), x.end(), output_stream); std::cout << std::endl;

  polyshrink(x, y);
  
  std::cout << "\n\nOutput vector: \n";
  std::copy(y.begin(), y.end(), output_stream);
  
  // big values
  for (int i=0; i<size; ++i) 
    x[i] = 10 + 2 * ((double)i);
  std::cout << "\n\nInput vector: \n";
  std::copy(x.begin(), x.end(), output_stream); std::cout << std::endl;
  
  polyshrink(x, y);
  
  std::cout << "\n\nOutput vector: \n";
  std::copy(y.begin(), y.end(), output_stream);
  
}
  

