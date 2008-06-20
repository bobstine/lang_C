// $Id: gsl_iterator.test.cc,v 1.3 2008/01/16 22:51:45 bob Exp $-*- c++ -*-

#include "gsl_iterator.h"
#include "gsl/gsl_vector.h"

#include <vector>
#include <algorithm>
#include <numeric>
#include <iostream>

int main()
{

  std::vector<double> test;
  test.push_back(1.0);
  test.push_back(2.0);
  test.push_back(3.0);
  test.push_back(4.0);


  // test gsl vector read/write
  gsl_vector *pV (gsl_vector_alloc(test.size()));
  gsl_vector_iterator vIter (pV);

  for (unsigned int i=0; i<test.size(); ++i)
  {   *vIter = test[i];
      ++vIter;
  }

  for (unsigned int i=0; i<test.size(); ++i)
    std::cout << "gsl_vector_get: g[" << i << "] = " << gsl_vector_get(pV,i) << std::endl;
  std::cout << std::endl;
  
  vIter = pV;
  for (unsigned int i=0; i<test.size(); ++i, ++vIter)
    std::cout << "*vIter        : g[" << i << "] = " << *vIter << std::endl;
  std::cout << std::endl;
  
  for (unsigned int i=0; i<test.size(); ++i, ++vIter)
    std::cout << "vIter[i]      : g[" << i << "] = " << vIter[i] << std::endl;
  std::cout << std::endl;
  
  vIter[0] = 200;
  vIter[3] = 400;
  
  std::cout << "With indexed insertion at from and back: ";
  std::copy(begin(pV), end(pV), std::ostream_iterator<double>(std::cout, " "));
  std::cout << std::endl;
  
  std::cout << "\n\n Constant vector through view ...\n";
  gsl_vector_const_view         view (gsl_vector_const_subvector(pV,0,4));
  gsl_vector_const_iterator viewIter (&view.vector);
  for (unsigned int i=0; i<test.size(); ++i, ++viewIter)
    std::cout << "viewIter[i]   : v[" << i << "] = " << viewIter[i] << std::endl;
  std::cout << std::endl;
  
  
}
