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

int main()
{
  int more=3;
  
  Container aList;

  if(--aList.end()==aList.begin())
    std::cout << "--end == begin \n";
  else
    std::cout << "--end != begin\n";

  if((++aList.end())==aList.begin())
    std::cout << "++end == begin \n";
  else
    std::cout << "++end != begin\n";
  
  for (int i=0; i<more; ++i) { aList.push_back(i); };
  std::cout << "size = " << aList.size() << std::endl;

  if((++aList.end())==aList.begin())
    std::cout << "++end == begin \n";
  else
    std::cout << "++end != begin\n";
  


  Container::const_reverse_iterator it (aList.rbegin());
  check(it,aList);

  while(it != aList.rend()) {  std::cout << " reading " << *it << std::endl; ++it; }
  check(it,aList);

  for (int i=0; i<more; ++i) { aList.push_back(i); }; std::cout << "size = " << aList.size() << std::endl;
  check(it,aList);
  while(it != aList.rend()) {  std::cout << " reading " << *it << std::endl; ++it; }
  for (int i=0; i<more; ++i) { aList.push_back(i); }; std::cout << "size = " << aList.size() << std::endl;
  while(it != aList.rend()) {  std::cout << " reading " << *it << std::endl; ++it; }
  for (int i=0; i<more; ++i) { aList.push_back(i); }; std::cout << "size = " << aList.size() << std::endl;

  return 0;
}

							
  
