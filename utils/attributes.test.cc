#include "attributes.h"

#include <iostream>

int
main()
{
  Attributes  attr{"a=1,b=2,c  = 3"};

  std::cout << "TEST: attr is " << attr << std::endl;

  std::cout << "TEST: attr[NA] = " << attr["na"] << std::endl;
  std::cout << "      attr[a]  = " << attr["a"]  << std::endl;
  
  attr.add_attribute("new", "value");
  std::cout << "TEST: after adding new=value, attr is " << attr << " with size " << attr.size() << std::endl;

  attr.erase_attribute("b");
  std::cout << "TEST: after erasing attrbute b, attr is " << attr << " with size " << attr.size() << std::endl;
  
  return 0;
}
