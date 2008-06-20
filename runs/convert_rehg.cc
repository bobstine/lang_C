// $Id: convert_rehg.cc,v 3.0 2004/11/19 18:58:36 foster Exp $

#include <iostream>
#include <string>

// The output file should look like:
//
//
//      10000
//      Y
//      1 1 1 1 ... 0 0 0 ...
//      X1
//      1.23 2.3 ...
//      X2
//      .03 .02 ...
//
//      etc
//
//

int
main()
{
  // First say what "n" is
  std::cout << 10000 << std::endl;   

  // Now create the row of Y's
  std::cout << "Y" << std::endl;
  for(int i = 0; i < 5000;++i)
    std::cout << "1 ";
  for(int i = 0; i < 5000;++i)
    std::cout << "0 ";
  std::cout << std::endl;

  // Now create the Xs, one X per row
  int x_index = 1;
  while(!std::cin.eof())
    {
      std::cout << "X" << x_index << std::endl;
      std::string line;
      getline(std::cin,line);
      std::cout << line << std::endl;
      ++x_index;
      std::cin >> std::ws;
    }

  return 0;
}
