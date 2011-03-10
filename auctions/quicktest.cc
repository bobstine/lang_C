#include <iostream>
 
int main()
{
  std::string str ("123456789");

  str = "0" + str + "a" + str + "b" + str;

  std::cout << str << std::endl << "Length = " << str.length() << " and size = " << str.size() << std::endl;
  
  if(str.length() > 15)
    std::cout << str.substr(0,15) << std::endl;
  else
    std::cout << str;
}
