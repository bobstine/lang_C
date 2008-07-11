//  $Id: rows_to_columns.cc,v 1.2 2007/12/20 03:59:21 bob Exp $

#include "column.h"

#include <iostream>
#include <fstream>
#include <vector>

#define FILENAME "/Users/bob/Desktop/rows.dat"

int
main()
{
  // open the file as a column stream
  FileColumnStream columnStream(FILENAME);
  
  // read n from the first line of the file
  int n (columnStream.n());
  std::cout << "TRSP: n = " << n << std::endl;
  if (n <= 0) return -1;
  
  // read the features as name on one line, followed by a row of data as next line
  std::vector< std::iterator_traits<FileColumnStream>::value_type > columnVector;
  insert_columns_from_file(FILENAME,std::back_inserter(columnVector));

  // write the columns out in tab delimited format
  std::ofstream output ("/Users/bob/Desktop/transposed.dat");
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
  
