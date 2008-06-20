// $Id: convert_nips_2004.cc,v 3.0 2004/11/19 18:58:36 foster Exp $

#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include <iterator>
#include <assert.h>


//  usage:
//
//          convert_nips  5000 data/NIPS2004/.../foo_train
//
//  The integer passed in is the number of columns NOT n.
//

int
main(int argc, char** argv)
{
  assert(argc == 3);   // should be "n" followed by "file_name"

  int p;
  {
    std::stringstream p_stream(argv[1]);
    p_stream >> p;
  };

  std::string base_file_name = argv[2]; 
  std::string y_file_name = base_file_name + ".targets";
  std::ifstream y_data(y_file_name.c_str());
  std::string xs_file_name = base_file_name + ".inputs";
  std::ifstream xs_data(xs_file_name.c_str());

  std::vector<double> y;
  typedef std::istream_iterator<int> my_in;
  copy(my_in(y_data),my_in(), back_inserter(y));
  bool binary = true;
  for(std::vector<double>::const_iterator i = y.begin(); i != y.end(); ++i)
    if((*i != -1) && (*i != 1))
      binary = false;

  // First say what "n" is
  int n = y.size();
  std::cout << n << std::endl;   

  std::cout << "Y" << std::endl;
  //  copy(y.begin(),y.end(),std::ostream_iterator<int>(std::cout," "));
  if(binary)
    for(std::vector<double>::const_iterator i = y.begin(); i != y.end(); ++i)
      if(*i > 0)
	std::cout <<"1 ";
      else
	std::cout <<"0 ";
  else
    for(std::vector<double>::const_iterator i = y.begin(); i != y.end(); ++i)
      std::cout << *i << " ";
    
  std::cout << std::endl;
    
  std::vector<std::vector<double> > xs;
  while(!xs_data.eof())
    {
      std::vector<double> row(p,0); 
      for(int i = 0; i < p; ++i)
	xs_data >> row[i];
      xs_data >> std::ws;
      xs.push_back(row);
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
