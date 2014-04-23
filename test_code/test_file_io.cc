#include <iostream>
#include <fstream>
#include <string>

int main()
{
  // first without condensing small POS
  {
    std::ifstream fs("/Users/bob/C/text/test_in");
    if (!fs)
      std::cout << "TEST: Could not open file.\n";
    else
    { int counter = 0;
      while (fs && counter<10)
      { std::string str;
	std::cout << " ------------- HERE -------main------" << std::endl;
	fs >> str;
	std::cout << " Read the string... " << str << std::endl;
	++counter;
      }
      std::cout << " ------------- HERE -------main------" << std::endl;
    }
  }
  return 0;
}
