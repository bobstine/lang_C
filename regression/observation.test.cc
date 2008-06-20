// $Id: observation.test.cc,v 1.3 2003/02/09 20:03:13 bob Exp $

#include "observation.h"

int main (void)
{
  std::cout << "Enter the data to record in an observation as wt x1 x2 x3...: " << std::endl;
  Observation obs1(std::cin);
  std::cout << obs1 << std::endl;

  std::cout << "Enter the data to record: " << std::endl;
  Observation obs2;
  std::cin >> obs2;
  std::cout << obs2 << std::endl;

  std::cout << "Appending values to last observation " << std::endl;
  obs2.append_value (7.7);
  obs2.append_value (-7.7);
  std::cout << obs2 << std::endl;
  
  return 0;
}
    
