// $Id: front_end_cont.cc,v 1.1 2003/06/25 18:09:45 bob Exp $-*- c++ -*-


#include <iostream>
#include <fstream>
#include <string>

int
main()
{
  std::string name_front2main("front2main");
  std::string name_main2front("main2front");
  {
    int ik = 0;
    std::ofstream front2main(name_front2main.c_str());
    std::cin >> ik;
    front2main << ik << std::endl;
  }
  {
    int ok = 0;
    std::ifstream main2front(name_main2front.c_str());
    main2front >> ok;
    std::cout << ok << std::endl;
  };
};
