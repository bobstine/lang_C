#include <iostream>
#include <iterator>
#include <vector>

template <class Iter>
inline
std::ostream&
operator<<(std::ostream& os, std::pair<Iter,Iter> p)
{
  std::copy(p.first, p.second,
	    std::ostream_iterator<typename std::iterator_traits<Iter>::value_type>(os, " "));
  os << std::endl;
  return os;
}

int main()
{
  std::vector<double> x(5);
  for(int i=0; i<5; ++i) x[i]=i;

  typedef std::vector<double>::const_iterator Iter;
  std::pair<Iter,Iter> p1;

  p1 = std::pair<Iter,Iter>(x.begin(), x.end());

  std::cout << p1 << std::endl;
  
  std::copy(p1.first, p1.second, std::ostream_iterator<double>(std::cout," "));
  std::cout << std::endl;
  std::cout << std::endl;

  return 0;
}
