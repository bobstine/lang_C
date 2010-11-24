#include <list>
#include <deque>
#include <iostream>
#include <assert.h>

//typedef std::deque<int> Container;
typedef std::list<int> Container;

void
check(Container::const_reverse_iterator it, const Container& c)
{
  bool found = false;
  std::cout << "[";
  for(Container::const_reverse_iterator i = c.rbegin(); i != c.rend(); ++i)
    {
      if(i == it)
	{
	  std::cout << "found iter";
	  found = true;
	}
    }
  Container::const_reverse_iterator previous = it;
  previous--;
  previous++;
  assert(previous == it);
  if(!found)
    std::cout << "not found";
  if(it == c.rend())
    std::cout << " (at end)";
  std::cout << "]\n";

}

typedef Container::const_iterator Iterator;

int main()
{
  int more=3;
  
  Container aList;
  Iterator theEnd (aList.end());
  Iterator it     (aList.begin());

  std::cout << "Initial  : " << (it == theEnd) << std::endl;
  
  aList.push_back(1);
  std::cout << "Post-push: " << (it == theEnd) << std::endl;
  

  return 0;
}

							
  
