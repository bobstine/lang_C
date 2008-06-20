// $Id: cyclic.test.cc,v 1.1 2000/04/25 15:35:55 bob Exp $-*- c++ -*-
#include <vector.h>
#include <algo.h>
#include <numeric>

#include "cyclic.h"

int main()
{

  vector<double> test;
  test.push_back(1.0);
  test.push_back(2.0);
  test.push_back(3.0);
  test.push_back(4.0);
  
  Cyclic_iterator it(test.begin(), test.end());

  // Iterate through a vector by explict ++ operation
  for(int i = 0;i <10;++i)
  {
    cout << *it << " " ;
    ++it;
  }
  cout << endl;

  vector<double> test2;
  test2.push_back(1.0);
  test2.push_back(1.0);

  Cyclic_iterator it2 (test2.begin(), test2.end());

  // Compute dot product using an iterator as an argument
  cout << "inner product is "
       << inner_product(test.begin(), test.end(), it2, 0.0)
       << endl;
}
