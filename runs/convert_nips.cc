// $Id: convert_nips.cc,v 3.0 2004/11/19 18:58:36 foster Exp $

#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include <iterator>
#include <assert.h>


//  usage:
//
//          convert_nips dense 5000 data/NIPS2003/.../foo_train
//          convert_nips sparse 500 data/NIPS2003/.../foo_train
//          convert_nips binary 500 data/NIPS2003/.../foo_train
//
//  Only accepts either "dense" or "sparse" as keywords.
//
//  The integer passed in is the number of columns NOT n.
//

int
main(int argc, char** argv)
{
  assert(argc == 4);   // should be "dense||sparse" followed "n" followed by "file_name"

  std::string dense_sparse = argv[1];
  assert((dense_sparse == "dense") || (dense_sparse == "sparse") || (dense_sparse == "binary"));
  bool is_dense = (dense_sparse == "dense");
  bool is_binary = (dense_sparse == "binary");

  int p;
  {
    std::stringstream p_stream(argv[2]);
    p_stream >> p;
  };

  std::string base_file_name = argv[3]; 
  std::string y_file_name = base_file_name + ".labels";
  std::ifstream y_data(y_file_name.c_str());
  std::string xs_file_name = base_file_name + ".data";
  std::ifstream xs_data(xs_file_name.c_str());

  std::vector<int> y;
  typedef std::istream_iterator<int> my_in;
  copy(my_in(y_data),my_in(), back_inserter(y));

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
    
  std::vector<std::vector<double> > xs;
  int num_rows_read = 0;
  while(!xs_data.eof())
    {
      std::vector<double> row(p,0); 
      if(is_dense)
	{
	  for(int i = 0; i < p; ++i)
	    xs_data >> row[i];
	  std::string waste;
	  std::getline(xs_data,waste);
	}
      else
	if(is_binary)
	  {
	    std::string row_string;
	    std::getline(xs_data,row_string);
	    std::stringstream in(row_string);
	    while(!in.eof())
	      {
		int index;
		in >> index;
		row[index] = 1;
	      }
	  }
	else
	  {
	    std::string row_string;
	    std::getline(xs_data,row_string);
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
      xs_data >> std::ws;
		
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
