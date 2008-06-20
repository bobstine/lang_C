// $Id: envelope.test.cc,v 1.1 2003/11/26 16:55:47 bob Exp $

#include "envelope.h"

#include <iostream>
#include <list>


int main ()
{
  Envelope<double> env1(2.2);
  Envelope<double> env2(env1);
  std::cout << "Current count is " << Envelope<double>::count() << std::endl;
  
  {
    std::list< Envelope<double> > myList;
    myList.push_back(env1);
    myList.push_back(env2);
    
    // this count will be 4 since they are *copied* onto the list
    std::cout << "Current count is " << Envelope<double>::count() << std::endl;
    
    for(int i = 0; i<4; ++i)
      myList.push_back(Envelope<double>(i));
    
    // this count is 8 since the four added here are added directly  onto the list
    std::cout << "Current count is " << Envelope<double>::count() << std::endl; 
  }
  
  // now the count returns to 2 since the list goes away
  std::cout << "Current count is " << Envelope<double>::count() << std::endl;
  
  return 0;
}
