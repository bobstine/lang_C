// $Id: convert_digits.cc,v 1.1 2006/02/17 19:49:18 foster Exp $

#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include <iterator>
#include <assert.h>


//  usage:
//
//          convert_digits 5000 ~/digits_training
//
//  The integer passed in is the number of columns NOT n.
//

int
main(int argc, char** argv)
{
  assert(argc == 3);  

  int p;
  {
    std::stringstream p_stream(argv[1]);
    p_stream >> p;
  };
  std::string base_file_name = argv[2]; 
  std::ifstream data(base_file_name.c_str());


  std::vector<int> y;
  std::vector<std::vector<double> > xs;
  std::string row_string;
  std::getline(data,row_string);
  int num_rows_read = 0;
  while(!data.eof())
    {
      int tmp = 0;
      data >> tmp;
      data >> std::ws;
      y.push_back(tmp);
      std::vector<double> row(p,0); 
      {
	std::string row_string;
	std::getline(data,row_string);
	std::stringstream in(row_string);
	while(!in.eof())
	  {
	    int index;
	    char delimeter;
	    double data;
	    in >> index >> delimeter >> data;
	    assert(delimeter == ':');
	    row[index] = data;
	  }
      }
      data >> std::ws;
		
      xs.push_back(row);
      ++num_rows_read;
    }


  // First say what "n" is
  int n = y.size();
  std::cout << n << std::endl;   

  std::cout << "Y" << std::endl;
  //  copy(y.begin(),y.end(),std::ostream_iterator<int>(std::cout," "));
  for(std::vector<int>::const_iterator i = y.begin(); i != y.end(); ++i)
    if(*i > 0)
      std::cout <<"1 ";
    else
      std::cout <<"0 ";
  std::cout << std::endl;
    

  for(int i = 0; i < p; ++i)
    {
      std::cout << "X" << i+1 << std::endl;
      for(int j = 0; j < n; ++j)
	std::cout << xs[j][i] << " ";
      std::cout << std::endl;
    }

  return 0;
}
