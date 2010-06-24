/*
  Play with the lambda functions in C++0x

  22 Jun 2010

*/

#include <iostream>
#include <vector>
#include <algorithm>

std::ostream&
operator<<(std::ostream& os, std::vector<double> const& x)
{
  os << "{";
  for (unsigned int i=0; i<x.size()-1; ++i)
    os << x[i] << " ";
  os << x[x.size()-1] << "}";
  return os;
}


int main(int, char**)
{

  const int n (10);
  std::vector<double> v;

  for (int i=0; i<n; ++i)
    v.push_back( (double) i * 200);

  std::transform(v.begin(), v.end(), v.begin(),
		[](double x)->double { return 2 * x; }
		 );
  
  std::cout << v << std::endl;
  
}
