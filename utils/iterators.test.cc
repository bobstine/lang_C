// $Id: cyclic_iterator.test.cc,v 1.3 2005/12/07 03:49:40 bob Exp $-*- c++ -*-

#include <vector>
#include <list>
#include <algorithm>
#include <numeric>
#include <iostream>

#include "iterators.h"

int main()
{
  std::vector<double> test;
  std::list<double> test2;
  for(int i = 0 ; i<3; ++i)
  { test.push_back(2 * (i+1));
    test2.push_back(1 + 2*i);
  }
  
  { // test of the iterator over to containers
    typedef std::vector<double>::const_iterator Iterator;
    typedef std::list<double>::const_iterator Iterator2;
    
    JoinIterator<Iterator, Iterator2> start(test.begin(), test2.begin());
    JoinIterator<Iterator, Iterator2> stop (test.end(),   test2.end());
    
    int count (0);
    for (JoinIterator<Iterator, Iterator2> i = start; i != stop; ++i)  // this step introduces a copy constructor
    { std::cout << *i << " ";
      if (++count > 100) break;   // to stop if runs away when debugging
    }
    std::cout << std::endl;

  }
  // Note that *all* of the following tests use an integer limit to control
  // the duration of the loop, so one never has to compare whether an
  // iterator has reached the "end".   Not very STL compliant.

  // Iterate through a vector by explict ++ operation
  cyclic_iterator it  (test.begin(), test.end());
  for(int i = 0;i <10;++i)
  {
    std::cout << *it << " " ;
    ++it;
  }
  std::cout << std::endl;

  // Compute dot product using an iterator as an argument
  std::vector<double> testpair;
  testpair.push_back(1.0);
  testpair.push_back(1.0);

  cyclic_iterator it2 (testpair.begin(), testpair.end());

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
