#include "column.Template.h"
#include "print_utils.h"
#include "debug.h"

#include <cstdio>
#include <iostream>
#include <fstream>
#include <vector>

int
main()   
{
  using std::string;
  using debugging::debug;
  debugging::debug_init(std::clog, 2);
  
  typedef float Scalar;
  
  std::cout << "\n\nTEST: dynamic columns \n";
  
  int n = 100;
  std::vector<Scalar> x(n);
  std::vector<Scalar> y(n);
  for (int i=0; i<n; ++i)
  { x[i] = (Scalar) 7.0 * (Scalar)i;
    y[i] = (Scalar)i;
  }
  
  Column<Scalar> c1 ("c1", "description=1", n, x.begin());
  Column<Scalar> c2 (c1);
  Column<Scalar> c3 (c2);
  Column<Scalar> c4 ("c4", "description=4", n, y.begin());
  std::vector<Scalar>::const_iterator it = x.begin();
  Column<Scalar> c5 ("c5", Attributes("lambda=sqrt"), n, it, [](Scalar x)->Scalar { return (Scalar) sqrt((Scalar)x); } );
  
  std::cout << "TEST: inserted data\n";
  std::cout << c1 << std::endl;
  std::cout << c2 << std::endl;
  std::cout << c3 << std::endl;
  std::cout << c4 << std::endl;
  std::cout << c5 << std::endl;
  
  std::cout << "\nTEST: Integer columns...\n";
  IntegerColumn ic1 (c1);
  std::cout << ic1 << std::endl;
  
  //  test making an empty column, then putting something in it
  std::cout << "TEST: empty column\n";
  Column<Scalar> empty;
  std::cout << "TEST: empty size " << empty->size() << std::endl;
  empty = c1;
  c2 = empty;
  std::cout << "TEST: empty after assignment " << empty << std::endl;

  return 0;
}

  
