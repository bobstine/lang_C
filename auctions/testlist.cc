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
  Iterator it     (aList.end());  // point to one before the item to deref

  std::cout << "Initial  : " << (  it == theEnd) << std::endl;
  std::cout << "     ++  : " << (++it == theEnd) << std::endl;
  
  aList.push_back(1);
  std::cout << "Post-push: " << (  it == theEnd) << std::endl;

  Iterator copy (it);             // need to use a copy to look ahead or else wraps
  while(++copy != theEnd)
  {
    std::cout << "         *++it = " << *(++it) << std::endl;
  }
  std::cout << "Push 3 more\n";
  aList.push_back(2);
  aList.push_back(3);
  aList.push_back(4);
  copy = it;
  while(++copy != theEnd)
  {
    std::cout << "         *++it = " << *(++it) << std::endl;
  }
  

  return 0;
}

							
  
