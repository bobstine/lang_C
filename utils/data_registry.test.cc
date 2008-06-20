// $Id: data_registry.test.cc,v 1.1 2004/08/23 21:32:53 bob Exp $


#include "data_registry.h"

#include <vector>
#include <iostream>


int main()
{
  int n(10);
  /*  std::vector<double> x1 (n);
      std::vector<double> x2 (n);
      std::vector<double> x3 (n);

      bool flag (false);
  */

  double *x1 = new double[n];
  double *x2 = new double[n];
  double *x3 = new double[n];

  bool flag (true);              // release pointers when ref count goes to zero
  for(int i=0;i<n; ++i)
  { x1[i] = i;
    x2[i] = 2*i;
    x3[i] = 3 * i;
  }

  //  DataRegistry< std::vector<double> > registry;
  DataRegistry< double > registry;

  int tag1 (registry.insert_data(x1, flag));
  std::cout << "First tag is " << tag1 << std::endl;
  registry.insert_data(x2, flag);
  registry.insert_data(x3, flag);

  std::cout << registry;

  std::cout << "data[3] for second vector is " << registry.data(2)[3] << std::endl;
  std::cout << "data[3] for invalid vector is " << registry.data(4) << std::endl;
  
  registry.add_reference(1);
  registry.add_reference(1);
  registry.add_reference(1);

  std::cout << registry;
  
  registry.remove_reference(2);
  std::cout << "data[3] for second vector after removed is " << registry.data(2) << std::endl;
  
  /*
  
  void remove_reference (key_type key)
    {
      if (mKeyCount[key] == 0)
	std::cerr << "DREG: Attempt to release a key that had no external references.\n";
      else -- mKeyCount[key];
      if (mKeyCount[key] == 0)
	delete mMap[key];
    }
  
  */
  return 0;
  
};
