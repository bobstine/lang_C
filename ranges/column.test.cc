//  $Id: column.test.cc,v 1.11 2007/12/16 17:59:27 bob Exp $

#include "column.h"
#include "print_utils.h"

#include <cstdio>
#include <iostream>
#include <vector>

int
main()   
{

  {
    std::cout << "TEST: dynamic columns \n";

    int n = 100;
    std::vector<double> x(n);
    std::vector<double> y(n);
    for (int i=0; i<n; ++i)
    { x[i] = 7.0 * i;
      y[i] = i;
    }

    Column c1 ("c1", n, x.begin());
    Column c2 (c1);
    Column c3 (c2);
    Column c4 ("c4", n, y.begin());
    Column c5 (c4);
    
    std::cout << "TEST: inserted data\n";
    std::cout << c1 << std::endl;
    std::cout << c2 << std::endl;
    std::cout << c3 << std::endl;
    std::cout << c4 << std::endl;
    std::cout << c5 << std::endl;

  }

  std::cout << "TEST: Starting file portion.  Opening the column stream.\n";
  
  // read the columns one at a time via the iterator interface
  FileColumnStream columnStream("/Users/bob/C/gsl_tools/data/bank_small.dat");
  std::cout << "TEST: Length of elements in column stream is n = " << columnStream.n() << std::endl;

  
  std::vector< std::iterator_traits<FileColumnStream>::value_type > columnVector;

  columnVector.push_back(*columnStream); ++columnStream;
  columnVector.push_back(*columnStream); ++columnStream;

  std::cout << columnVector[0] << std::endl;
  std::cout << columnVector[1] << std::endl;

  for (int j=0; j<30; ++j, ++columnStream) // try to read past end of columns
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
  std::cout << "TEST: column vector has " << columnVector.size() << " columns.\n";
  

  // or just read them all into a vector
  std::pair<int,int> dim;
  std::vector<Column> yColumns;
  std::vector<Column> xColumns;
  dim = insert_columns_from_file("/Users/bob/C/gsl_tools/data/bank_small.dat", 1, back_inserter(yColumns), back_inserter(xColumns));

  // dim = insert_columns_from_file("/Users/bob/C/seq_regr/data/bank_small.dat",std::back_inserter(columnVector));
  std::cout << "TEST: x column vector has " << xColumns.size() << " columns; dims read as "  << dim << std::endl;
  int nRows (yColumns[0].size());
  std::cout << "TEST: Y column named " << yColumns[0] << " holds " << nRows << " rows.\n";

  // or read them from a name file and a data file of rows.
  // These two are made by Dean's c4.5 syntax program test. (NEED to add number of rows.)
  // run as cat test/nrows test/c45_test.rows | column.test.exec`
  if(false)
  { int nRows (5);
    std::vector<Column> cols;
    std::cout << "TEST: Read "
              << insert_columns_from_stream(stdin, "test/c45_test.names", nRows, std::back_inserter(cols))
              << " columns from stdin.\n";
    for (unsigned int j=0; j<cols.size(); ++j)
      std::cout << "     " << cols[j] << std::endl;
  }
   
  return 0;
}
  
