// $Id: convert_add.cc,v 3.2 2006/02/10 19:39:39 foster Exp $

#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include <iterator>
#include <assert.h>


//  usage:
//
//          convert_nips x_file y_file
//

int
main(int argc, char** argv)
{
  assert(argc == 3); 
  int p = 2593;

  std::string x_file_name = argv[1]; 
  std::string y_file_name = argv[2]; 

  std::ifstream y_data(y_file_name.c_str());
  std::ifstream xs_data(x_file_name.c_str());
  std::vector<int> y;
  typedef std::istream_iterator<int> my_in;
  copy(my_in(y_data),my_in(), back_inserter(y));

  // First say what "n" is
  int n = y.size();
  //  n = 10 * (n / 10);  // fix bug???
  std::cout << n << std::endl;   

  std::cout << "Y" << std::endl;
  for(std::vector<int>::const_iterator i = y.begin(); i != y.begin() + n; ++i)
    if(*i > 0)
      std::cout <<"1 ";
    else
      std::cout <<"0 ";
  std::cout << std::endl;
    
  std::vector<std::vector<double> > xs;
  int num_rows_read = 0;
  while(!xs_data.eof())
    {
      std::vector<double> row(p,0); 
      for(int i = 0; i < p; ++i)
	xs_data >> row[i];
      std::string waste;
      std::getline(xs_data,waste);
		
      xs.push_back(row);
      ++num_rows_read;

      if(num_rows_read > n)
	{
	  std::cerr << "oops... wrong number of rows or bad row was encountered:" << num_rows_read << " > " << n
		    << "(" << row.size() << ") "
		    << xs_data.eof() << xs_data.bad() << std::endl;
	  //	  std::copy(row.begin(),row.end(),std::ostream_iterator<double>(std::cerr," "));
	  std::cerr << std::endl;
	}
    }

  for(int i = 0; i < p; ++i)
    {
      std::cout << "X" << i+1 << std::endl;
      for(int j = 0; j < n; ++j)
	std::cout << xs[j][i] << " ";
      std::cout << std::endl;
    }

  return 0;
}
