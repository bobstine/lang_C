// $Id: main_program.cc,v 1.1.1.1 2003/12/05 21:35:05 bob Exp $-*- c++ -*-


#include <iostream>
#include <string>

#include <fstream>
#include <string>

int
main()
{
  std::string name_front2main("front2main");
  std::string name_main2front("main2front");

  while(true)
    {
      double result = 0;
      std::ifstream front2main(name_front2main.c_str());
      while(front2main)
	{
	  std::string s;
	  getline(front2main,s);
	  std::cout << "read something:" << s << std::endl;
	  result += s.length();
	}	
      std::ofstream main2front(name_main2front.c_str());
      main2front << result << std::endl;
    }
}

