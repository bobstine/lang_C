// $Id: datasets.test.cc,v 1.3 2003/08/12 15:30:39 bob Exp $

#include <iostream>
#include <string>
#include <vector>
#include <functional>

#include "range.h"
#include "range_traits.h"
#include "make_range.h"
#include "range_ops.h"

#include "datasets.h"

// std::string fileName ("test/covar.dat");
// bool transposed (false);



int
main (void)
{
  std::cout << "hello world." << std::endl;

  std::string fileName ("/Users/bob/raslisp/stepwise/C/wind/wind2/6weeks/est.dat");
  bool my_transposed = true;


  NumericDataset data(fileName, my_transposed);


  std::cout << "Averages: " << data.averages();
  
  std::cout << "------------------------" << std::endl
	    << "Echo of 2 lines of data read from file:" << std::endl;  
  for (int i=0; i<2; ++i)
    std::cout << "  [" << i << "]  "  << data.row(i);
  
  std::cout << "------------------------" << std::endl
	    << "Echo of centered data:" << std::endl;  
  for (int i=0; i<2; ++i)
    std::cout << "  [" << i << "]  "  << data.centered_row(i);

  return 0;
}
    
