//   $Id: calibrators.test.cc,v 1.2 2004/08/06 17:57:54 bob Exp $

#include "calibrators.h"

#include "random.h"
#include "range.h"
#include "range_ops.h"
#include "function_iterators.h"

#include <iostream>
#include <fstream>
#include <vector>

int
main()
{
  const int LENGTH (100);
  const int DF     (3);

  RandomGenerator rand(2462);

  std::ofstream output ("test/calibrate_dat.txt");
  if (output)
    std::cout << "TEST: Data written to file test/calibrate_dat.txt\n";
  else
    std::cout << "TEST: Could not open data file for output.\n";
  
  std::vector<double> x(LENGTH);
  std::vector<double> y(LENGTH);

  for (int i=0; i<LENGTH; ++i)
  { x[i] = rand.uniform(); 
    y[i] = x[i]*x[i] + 0.1*rand.normal();
    if (output)
      output << x[i] << " " << y[i] << std::endl;
    }
  if (output) output.close();
  
  BiweightCalibrator<std::vector<double>::const_iterator,std::vector<double>::const_iterator>
    bc(DF,x.begin(),y.begin(), LENGTH);
  std::cout << "TEST: " << bc << std::endl;
  
  std::cout << "TEST: biweight(0.05) = " << bc(0.05) << std::endl;
  std::cout << "TEST: biweight(0.25) = " << bc(0.25) << std::endl;
  std::cout << "TEST: biweight(0.50) = " << bc(0.50) << std::endl;
  std::cout << "TEST: biweight(0.75) = " << bc(0.75) << std::endl;
  
  std::cout << "TEST: fit is ... " << make_unary_range(bc, make_range(x)) << std::endl;
  
  return 0;
}
