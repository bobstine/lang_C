// $Id: cyclic_iterator.test.cc,v 1.3 2005/12/07 03:49:40 bob Exp $-*- c++ -*-

#include <vector>
#include <algorithm>
#include <numeric>
#include <iostream>

#include "cyclic_iterator.h"

int main()
{

  std::vector<double> test;
  test.push_back(1.0);
  test.push_back(2.0);
  test.push_back(3.0);
  test.push_back(4.0);

  // Iterate through a vector by explict ++ operation
  cyclic_iterator it  (test.begin(), test.end());
  for(int i = 0;i <10;++i)
  {
    std::cout << *it << " " ;
    ++it;
  }
  std::cout << std::endl;

  // Compute dot product using an iterator as an argument
  std::vector<double> test2;
  test2.push_back(1.0);
  test2.push_back(1.0);

  cyclic_iterator it2 (test2.begin(), test2.end());

  std::cout << "inner product is "
       << std::inner_product(test.begin(), test.end(), it2, 0.0)
       << std::endl;

  // Test periodic iterator
  periodic_iterator pIter1(10,13);
  for (int i=0; i<10; ++i, ++pIter1)
    std::cout << *pIter1 << " ";
  std::cout << std::endl;
  
  periodic_iterator pIter2(10,10);
  for (int i=0; i<10; ++i, ++pIter2)
    std::cout << *pIter2 << " ";
  std::cout << std::endl;
  
  
  { // Test constant iterator
    constant_iterator<int> cIter(10);
    for (int i=0; i<10; ++i, ++cIter)
      std::cout << *cIter << " ";
    std::cout << std::endl;
  }
    
  { 
    constant_iterator<bool> cIter(true);
    for (int i=0; i<10; ++i, ++cIter)
      std::cout << *cIter << " ";
    std::cout << std::endl;
  }
  
  
}
