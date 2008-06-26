// $Id: threshold.test.cc,v 1.1 2003/06/20 04:29:05 bob Exp $

#include <iostream>

#include "threshold.h"

int main()
{
  CodeProbability prob;
  
  std::cout << "Code prob for j = 1  " << prob( 1) << std::endl;
  std::cout << "Code prob for j = 2  " << prob( 2) << std::endl;
  std::cout << "Code prob for j = 5  " << prob( 5) << std::endl;
  std::cout << "Code prob for j = 10 " << prob(10) << std::endl;
  std::cout << "Code prob for j = 20 " << prob(20) << std::endl;


  Threshold tau;

  for (int j=1; j<10; ++j)
    std::cout << "Threshold for j = " << j << " is " << tau(j) << std::endl;

}
