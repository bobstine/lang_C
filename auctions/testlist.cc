#include <list>
#include <deque>
#include <iostream>
#include <assert.h>

//typedef std::deque<int> Container;
typedef std::list<int> Container;

void
check(Container::const_iterator it, const Container& c)
{
  bool found = false;
  std::cout << "[";
  for(Container::const_iterator i = c.begin(); i != c.end(); ++i)
    {
      if(i == it)
	{
	  std::cout << "found iter";
	  found = true;
	}
    }
  Container::const_iterator previous = it;
  previous--;
  previous++;
  assert(previous == it);
  if(!found)
    std::cout << "not found";
  if(it == c.end())
    std::cout << " (at end)";
  std::cout << "]\n";

}

int main()
{
  int more=3;
  
  Container aList;
  for (int i=0; i<more; ++i) { aList.push_back(i); };
  std::cout << "size = " << aList.size() << std::endl;

  Container::const_iterator it (aList.begin());
  check(it,aList);

  while(it != aList.end()) {  std::cout << " reading " << *it << std::endl; ++it; }
  check(it,aList);

  for (int i=0; i<more; ++i) { aList.push_back(i); }; std::cout << "size = " << aList.size() << std::endl;
  check(it,aList);
  while(it != aList.end()) {  std::cout << " reading " << *it << std::endl; ++it; }
  for (int i=0; i<more; ++i) { aList.push_back(i); }; std::cout << "size = " << aList.size() << std::endl;
  while(it != aList.end()) {  std::cout << " reading " << *it << std::endl; ++it; }
  for (int i=0; i<more; ++i) { aList.push_back(i); }; std::cout << "size = " << aList.size() << std::endl;

  return 0;
}

							
  
