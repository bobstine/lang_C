#include "dynamic_array.h"

#include <iostream>
#include <vector>

int 
main()
{
  
  {
    int first = 10;
    int last =  21;
    DynamicArrayBase<double> dab (first, last);
    for (int i=first; i<= last; ++i)    dab.assign(i, (double)i);
    std::cout <<  "dab[first]=" << dab[first] << "   dab[15]=" << dab[15] <<std::endl;
    std::cout <<  dab  << std::endl;
  }

  {
    int first =  5;
    int last =  21;

    DynamicArray<double> da (first, last);
    for (int i=first; i<= last; ++i) da.assign(i, (double)i);
    std::cout <<  "da[first]=" << da[first] << "   da[15]=" << da[15] <<std::endl;
    std::cout <<  da  << std::endl;
  }


  {
    int first = -10;
    int last =   14;

    DynamicArray<double> da (first, last);
    DynamicArray<double> da3 (first, last);
    for (int i=first; i<= last; ++i) da.assign(i, (double)i);
    {
      std::vector< DynamicArray<double> > x;
      x.push_back(da);
      x.push_back(da3);
      x.push_back(da);
      std::cout << "TEST: DA after 3 pushed on vector: " << da << std::endl;
    }
    std::cout << "TEST: DA after vector out of scope: " << da << std::endl;
    DynamicArray<double> da2 (da);
    std::cout << "TEST: DA2 after copy construct vector: " << da2 << std::endl;
  }
  

  return 0;
}
