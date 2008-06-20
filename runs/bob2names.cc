// $Id: bob2names.cc,v 3.0 2004/11/19 18:58:36 foster Exp $

#include <iostream>
#include <string>
#include <vector>

int
main()
{
  int n;
  std::cin >> n >> std::ws;
  std::vector<std::vector<double> > data;
  while(!std::cin.eof())
    {
      std::string name;
      getline(std::cin,name);
      std::cout << name << std::endl;
      double trash;
      for(int i = 0; i < n;++i)
	std::cin >> trash;
      std::cin >> std::ws;
    }
  return 0;
}
