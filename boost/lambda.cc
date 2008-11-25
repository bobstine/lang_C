#include <boost/lambda/lambda.hpp>
#include <algorithm>

#include <vector>
#include <iostream>

////
using namespace boost::lambda;
////

int main()
{
  std::vector<double> x(10);
  
  std::for_each(
		x.begin(), x.end(),
		_1 = 10
		);
  std::for_each(
		x.begin(), x.end(),
		std::cout << _1 << " "
		);
  
  std::cout << "\nDone.\n";
}
