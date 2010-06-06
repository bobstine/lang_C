// $Id: range_traits.test.cc,v 1.9 2003/12/02 21:00:25 bob Exp $

#include "range_traits.h"

#include <vector>
#include <iostream>

int main()
{
  std::vector<double> iz;
  for(int i = 0; i<5; ++i)
    iz.push_back(10 * i);
  std::cout << "\nBeginning of iz = "
	    << *iz.begin() << std::endl << std::endl;
  
  { // traits from container
    range_traits< std::vector<double> >::range          r (Ranges::make_range(iz));
    range_traits< std::vector<double> >::const_iterator j (begin(r));
    std::cout << "Beginning of range r: " << *j << std::endl;
  }
  
  { // traits from range itself
    range_traits< Ranges::range<std::vector<double>::const_iterator> >::range          r (Ranges::make_range(iz));
    range_traits< Ranges::range<std::vector<double>::const_iterator> >::const_iterator j (begin(r));
    std::cout << "Beginning of range r: " << *j << std::endl;
  }

  {  // assignable
    range_traits< std::vector<double>* >::range          r (Ranges::make_range(&iz));
    range_traits< std::vector<double>* >::iterator       j = begin(r);
    std::cout << "Beginning of range r: " << *j << std::endl;
  }

  std::cout << "DONE." << std::endl;
  return 0;
};


