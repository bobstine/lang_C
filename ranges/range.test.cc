#include "range.h"

#include <iostream>
#include <vector>
#include <list>
#include <string>
#include <iterator>
#include <algorithm>  // for copy

//  OUTPUT OUTPUT OUTPUT OUTPUT OUTPUT OUTPUT OUTPUT OUTPUT OUTPUT OUTPUT OUTPUT OUTPUT OUTPUT

template <class Iter>
inline std::ostream&
operator<<(std::ostream& os, const range<Iter>& r)
{
  typedef typename std::iterator_traits<Iter>::value_type local_type;
  std::copy( begin(r), end(r),  std::ostream_iterator<local_type>(os, " "));
  os << std::endl;
  return os;
}


int main()
{
  std::vector<double> ii;
  std::vector<double> iz;
  for(int i=0; i<5; ++i)
  { ii.push_back(i+1);
    iz.push_back(10 * i);
  }
  range<std::vector<double>::const_iterator> rng(iz.begin(),iz.end());
  std::cout << "\n Direct access to ends: " << *begin(rng) << " -- " << *(end(rng)-1) << std::endl;
  std::cout << "\n Direct access to ends: " << *begin(iz) << " -- " << *(end(iz)-1) << std::endl;
  std::cout << "\n Direct access to ends: " << *begin(&iz) << " -- " << *(end(&iz)-1) << std::endl;
  std::cout << "\n Length of range      : " << end(rng)-begin(rng) << std::endl;
  std::cout << "\n Test with 5 doubles  :  " << rng;
  std::cout << "\n Test with 5 doubles  :  " << make_range(rng);
  std::cout << "\n Test with 5 doubles  :  " << make_range(iz);
  std::cout << "\n Test with 5 doubles  :  " << make_range(make_pair(iz.begin(),iz.end()));
  std::cout << "\n Test with 5 doubles  :  " << make_range(iz.begin(),iz.end());

  std::cout << "\n Test join two        :  " << join_ranges(make_range(iz), make_range(ii));
  
  {
    std::cout << "\nTest assignment to range" << std::endl;
    
    std::vector<double> X(5), Zero(5);
    for (unsigned int i=0; i<X.size(); ++i)
      X[i] = i+10;
    range<std::vector<double>::iterator> writeableX (make_range(&X));
    std::cout << "writeable X is     " << make_range(writeableX);
    *begin(writeableX) = 99.0;
    std::cout << "writeable X is now " << make_range(writeableX);
    std::copy(begin(Zero), end(Zero), begin(writeableX));
    std::cout << "writeable X is now " << make_range(writeableX);
  }

  std::cout << std::endl << "DONE." << std::endl;
  return 0;
}
