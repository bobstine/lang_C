// $Id: fold_generator.cc,v 3.1 2006/02/10 02:49:59 foster Exp $

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <math.h>
#include <vector>
// See convert_rehg for a easier to understand program.



// BUG REPORT:
// It doesn't work with a non-divisable number:
// I changed the n from 351 to 350. It worked fine. thanks,
//-Jing


  



int
main(int argc, char** argv)
{
  std::string file_name = argv[1]; 
  std::ifstream raw_data(file_name.c_str());

  int num_files;
  if(argc == 2)
    num_files = 5;
  else
    {
      std::stringstream s(argv[2]);
      s >> num_files;
    };

  file_name += ".";
  std::vector<std::ofstream*> pf(num_files);
  for(int i = 0; i < num_files; ++i)
    {
      std::stringstream s;
      s << i;
      pf[i] = new std::ofstream((file_name+s.str()).c_str());
    }

  int n;
  raw_data >> n >> std::ws;

  std::vector<int> which_fold(n);
  std::vector<int> fold_count(num_files);
  for(std::vector<int>::iterator i = which_fold.begin(); i != which_fold.end(); ++i)
    { // Generates close to the same number in each bin
      double U = double(rand())/RAND_MAX;
      double remaining = which_fold.end() - i;
      int fold_index = -1;
      while((U > 0) && fold_index < num_files)
	{
	  ++fold_index;
	  U = U - (double(n/num_files) - fold_count[fold_index])/remaining;
	}
      if(fold_index == num_files)  // avoid the round off problems
	fold_index = num_files -1;
      *i = fold_index;
      ++fold_count[fold_index];
    }

  // write the sample size
  for(int fold_index = 0; fold_index < num_files; ++fold_index)
    (*pf[fold_index]) << fold_count[fold_index] << std::endl;
     
  // Now create the row of Y's
  while(!raw_data.eof())
    {
      std::string name;
      getline(raw_data,name);
      for(int fold_index = 0; fold_index < num_files; ++fold_index)
	(*pf[fold_index]) << name << std::endl;
      for(int i = 0; i < n;++i)
	{
	  double value;
	  raw_data >> value;
	  *pf[which_fold[i]] << value << " ";
	}
      raw_data >> std::ws;
      for(int fold_index = 0; fold_index < num_files; ++fold_index)
	(*pf[fold_index]) << std::endl;
    }
  return 0;
}
