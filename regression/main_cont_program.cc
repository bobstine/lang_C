// $Id: main_cont_program.cc,v 1.1 2003/06/25 18:09:45 bob Exp $-*- c++ -*-


#include <iostream>
#include <string>

int
main()
{
  int result = 0;
  while(true)
	{
	  int k;
	  std::clog << "main about to get line" << std::endl;
	  std::cin >> k;
	  std::clog << "read something: k=" << k << std::endl;
	  result += k;
	  std::cout << result << std::endl;
	}
}

