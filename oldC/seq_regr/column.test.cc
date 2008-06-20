//  $Id: column.test.cc,v 1.8 2004/08/25 22:15:28 bob Exp $

#include "column.h"

#include <cstdio>
#include <iostream>
#include <vector>

int
main()
{
  // read the columns on at a time via the iterator interface
  FileColumnStream columnStream("bank_small.dat");
  std::cout << "TEST: n = " << columnStream.n() << std::endl;

  std::vector< std::iterator_traits<FileColumnStream>::value_type > columnVector;

  columnVector.push_back(*columnStream); ++columnStream;
  columnVector.push_back(*columnStream); ++columnStream;

  std::cout << columnVector[0] << std::endl;
  std::cout << columnVector[1] << std::endl;

  for (int j=0; j<30; ++j, ++columnStream) // try to read past end of column
  {
    Column x = *columnStream;
    if (x.size() == 0)
    { std::cout << "TEST: Column stream is empty" << std::endl;
      break;
    }
    else
      std::cout << x << std::endl;
  }
  columnStream.close_file();

  // or just read them all into a vector
  insert_columns_from_file("bank_small.dat",std::back_inserter(columnVector));

  // or read them from a name file and a data file of rows.
  // These two are made by Dean's c4.5 syntax program test. (NEED to add number of rows.)
  // run as cat test/nrows test/c45_test.rows | column.test.exec`
  int nRows (5);
  std::vector<Column> cols;
  std::cout << "TEST: Read "
	    << insert_columns_from_stream(stdin, "test/c45_test.names", nRows, std::back_inserter(cols))
	    << " columns from stdin.\n";
  for (unsigned int j=0; j<cols.size(); ++j)
    std::cout << "     " << cols[j] << std::endl;
  return 0;
}  
  
