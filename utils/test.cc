//

#include <vector>
#include <iostream>

#include "print_utils.h"
#include "function_utils.h"
#include "stat_utils.h"
#include "file_utils.h"
#include "bennett.h"

int
main(void)
{
  std::vector<double> x(10,1.1);

  std::cout << "Print a vector " << std::endl
	    << x << std::endl;
  std::cout << "-------------------------------------" << std::endl;
  
  std::cout << "Mean and SD of vector " << mands(x) << std::endl;
  std::cout << "-------------------------------------" << std::endl;

  // Read some files
  std::string fileName ("/Users/bob/raslisp/stepwise/C/wind/wind2/6weeks/est.dat");
  std::cout << fileName << " has " << File_Utils::count_lines(fileName) << " lines, with "
	    << File_Utils::count_fields(fileName) << " fields in line 0 and "
	    << File_Utils::count_fields(fileName, 3) << " fields in line 3"
	    << std::endl;
  std::cout << "-------------------------------------" << std::endl;
  
  // This version gives back zero (approximately)
  double p(bennett_p_value (3.5, 2.0));
  std::cout << "p-value = " << p << std::endl;
  std::cout << "p-value = " << bennett_p_value (3.5, 2.0, 1.0) << std::endl;
  std::cout << "l-bound = " << bennett_bound (3.5, 2.0, 1.0, p) << std::endl;
  std::cout << "----" << std::endl;
  // This one is more like a usual interval
  p = 0.05;
  std::cout << "p-value = " << p << std::endl;
  std::cout << "p-value = " << bennett_p_value (12.5, 2.0, 1.0) << std::endl;
  std::cout << "l-bound = " << bennett_bound (12.5, 2.0, 1.0, p) << std::endl;

  // Critical values
  p = 0.01; std::cout << "quantile at p= " << p << " is " << bennett_critical_value(.016, p) << std::endl;
  p = 0.001; std::cout << "quantile at p= " << p << " is " << bennett_critical_value(.016, p) << std::endl;
  p = 0.0001; std::cout << "quantile at p= " << p << " is " << bennett_critical_value(.014, p) << std::endl;

  return 0;
}
