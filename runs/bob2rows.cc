// $Id: bob2rows.cc,v 3.0 2004/11/19 18:58:36 foster Exp $

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
      std::vector<double> column(n);
      for(int i = 0; i < n;++i)
	std::cin >> column[i];
      data.push_back(column);
      std::cin >> std::ws;
    }
  for(int i = 0; i < n;++i)
    {
      for(int j = 0; j < data.size(); ++j)
	std::cout << data[j][i] << " ";
      std::cout << std::endl;
    }
  return 0;
}
