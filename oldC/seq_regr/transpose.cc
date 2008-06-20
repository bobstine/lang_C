//  $Id: transpose.cc,v 1.1 2004/04/09 23:39:13 bob Exp $

#include "column.h"

#include <iostream>
#include <fstream>
#include <vector>

int
main()
{
  // open the file as a column stream
  FileColumnStream columnStream("subset.dat");
  int n = columnStream.n();
  std::cout << "TRSP: n = " << columnStream.n() << std::endl;
  if (n <= 0) return -1;
  
  // read the features as name then row of data
  std::vector< std::iterator_traits<FileColumnStream>::value_type > columnVector;
  insert_columns_from_file("subset.dat",std::back_inserter(columnVector));

  // write the columns out in tab delimited format
  std::ofstream output ("transposed.dat");
  for (unsigned int j=0; j<columnVector.size(); ++j)
    output << columnVector[j].name() << '\t';
  output << std::endl;
  for (int i=0; i<n; ++i)
  { 
    for (unsigned int j=0; j<columnVector.size(); ++j)
      output << columnVector[j].element(i) << '\t';
    output << std::endl;
  }
  
  return 0;
}  
  
