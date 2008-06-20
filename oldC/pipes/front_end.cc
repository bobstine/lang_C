// $Id: front_end.cc,v 1.1.1.1 2003/12/05 21:35:05 bob Exp $-*- c++ -*-


#include <iostream>
#include <fstream>
#include <string>

int
main()
{
  std::string name_front2main("front2main");
  std::string name_main2front("main2front");


  {
    std::ofstream front2main(name_front2main.c_str());
    while(std::cin)
      {
	std::string s;
	getline(std::cin,s);
	front2main << s << std::endl;
      }
  }
  std::ifstream main2front(name_main2front.c_str());
  while(main2front)
    {
      std::string s;
      getline(main2front,s);
      std::cout << s << std::endl;
    };
};
