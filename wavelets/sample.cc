#include <vector.h>
#include <iostream>

class member_templates
{
public:
  template<class T> void printit(const T& it) const;
  template<class T> void printall(T,T) const;
  
  
};

template<class T> void
member_templates::printit(const T& it) const
{
  cout << it << endl;
}

template<class T> void
member_templates::printall(T begin, T end) const
{
  copy(begin,end,ostream_iterator<typename iterator_traits<T>::value_type>(cout," "));
}

int
main()
{
  member_templates print;
  int i = 7;
  double d = 10.;
  print.printit(i);
  print.printit(d);
  vector<int> vec;
  vec.push_back(1);
  vec.push_back(2);
  vec.push_back(3);
  vec.push_back(4);
  print.printall(vec.begin(),vec.end());
}

  
