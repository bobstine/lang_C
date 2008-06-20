// $Id: range_traits.test.cc,v 1.1.1.1 2002/06/16 14:54:14 bob Exp $

#include "range_traits.h"

#include <vector>
#include <iostream>

int main()
{
  std::vector<double> iz (5);
  iz[0]=10; iz[1]=1; iz[2]=2; iz[3]=3; iz[4]=4;
  std::cout << "\nBeginning of iz = " << *iz.begin() << std::endl;
  std::cout << std::endl;
  
  {
    std::pair<std::vector<double>::iterator, std::vector<double>::iterator>
      my_range = make_pair(iz.begin(),iz.end());
    range_traits<std::pair<std::vector<double>::iterator,std::vector<double>::iterator> >::iterator
      i = begin(my_range);
    std::cout << *i << std::endl;
    std::cout << *begin(my_range) << std::endl;
  }
  
  // Cannot yet do the general matching for begin
  // range_traits<vector<double> >::iterator j = begin(iz);
  // cout << *j << endl;
  
  std::cout << "DONE." << std::endl;
  return 0;
};


