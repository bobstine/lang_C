// $Id: bobcat.cc,v 3.0 2004/11/19 18:58:36 foster Exp $

#include <iostream>
#include <fstream>
#include <string>
#include <math.h>
#include <vector>
#include <assert.h>

// Usage: bobcat data.1 data.2 data.3 data.4 | validate

int
main(int argc, char** argv)
{
  int num_inputs = argc - 1;
  std::vector<std::istream*> p_input(num_inputs);
  std::vector<int> n_input(num_inputs);

  for(int i = 1; i < argc; ++i)
    {
      std::string file_name =  argv[i];
      if(file_name == "-")
	p_input[i-1] = &std::cin;
      else
	p_input[i-1] = new std::ifstream(argv[i]);
    }

  int n = 0;
  for(int i = 0; i < num_inputs;++i)
    {
      (*p_input[i]) >> n_input[i] >> std::ws;
      n += n_input[i];
    }
  std::cout << n << std::endl;

  while(!p_input[0]->eof())
    {
      std::string name;
      getline(*p_input[0],name);
      for(int i = 1; i < num_inputs; ++i)
	{
	  std::string alternate_name;
	  getline(*p_input[i],alternate_name);
	  assert(alternate_name == name);
	}
      std::cout << name << std::endl;

      for(int i = 0; i < num_inputs;++i)
	{
	  for(int j = 0; j < n_input[i]; ++j)
	    {
	      double value;
	      (*p_input[i]) >> value;
	      std::cout << value << " ";
	    }
	  (*p_input[i]) >> std::ws;
	}
      std::cout << std::endl;
    }

  for(int i = 0; i < num_inputs;++i)
    assert(p_input[i]->eof());

  return 0;
}
