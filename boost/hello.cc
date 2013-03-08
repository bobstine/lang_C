#include <boost/lambda/lambda.hpp>
#include <boost/tuple/tuple.hpp>
#include <boost/tuple/tuple_io.hpp>
#include <iostream>
#include <iterator>
#include <algorithm>

int main()
{

  {
    boost::tuple<int, int, int> x (1,2,3);
    std::cout << "x = " << x << std::endl;
  }
  
  {
    using namespace boost::lambda;
  
    typedef std::istream_iterator<int> in;
    
    std::for_each(
		  in(std::cin), in(), std::cout << (_1 * 3) << " " );
    
    std::cout << "HERE\n";
  }
}
