//

#include <vector>
#include <iostream>

#include "print_utils.h"
#include "function_utils.h"
#include "stat_utils.h"

int
main(void)
{
  std::vector<double> x(10,1.1);

  std::cout << "Print a vector " << std::endl
	    << x << std::endl;

  std::cout << "Mean and SD of vector " << Stat_Utils::mands(x) << std::endl;
  
  return 0;
}
