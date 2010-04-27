//  $Id: column.test.cc,v 1.11 2007/12/16 17:59:27 bob Exp $

#include "column.h"
#include "print_utils.h"
#include "debug.h"


#include <cstdio>
#include <iostream>
#include <fstream>
#include <vector>

int
main()   
{
  debugging::debug_init(std::clog, -1);
  
  {
    std::cout << "\n\nTEST: dynamic columns \n";

    int n = 100;
    std::vector<double> x(n);
    std::vector<double> y(n);
    for (int i=0; i<n; ++i)
    { x[i] = 7.0 * i;
      y[i] = i;
    }

    Column c1 ("c1", "description 1", n, x.begin());
    Column c2 (c1);
    Column c3 (c2);
    Column c4 ("c4", "description 2", n, y.begin());
    Column c5 (c4);
    
    std::cout << "TEST: inserted data\n";
    std::cout << c1 << std::endl;
    std::cout << c2 << std::endl;
    std::cout << c3 << std::endl;
    std::cout << c4 << std::endl;
    std::cout << c5 << std::endl;
  }


  {     // read the columns one at a time via the iterator interface    
    std::cout << "\n\nTEST: File portion of test.\n";
    
    {
      std::cout << "\n\nTEST: Read from file into a column vector manually.\n";

      std::ifstream fileStream("/Users/bob/C/ranges/column_test.dat");
      ColumnStream columnStream(fileStream, "column_test.dat");

      std::vector<Column> columnVector;
      std::cout << "TEST: Length of elements in column stream is n = " << columnStream.n() << std::endl;    
      for (int j=0; j<30; ++j, ++columnStream) // try to read past end of columns
      { Column x = *columnStream;
	if (x->size() == 0)
	{ std::cout << "TEST: Column stream is empty" << std::endl;
	  break;
	}
	else
	{
	  columnVector.push_back(x);
	  std::cout << x << std::endl;
	}
      }
      std::cout << "TEST: column vector has " << columnVector.size() << " columns.\n";
    }


    {
      std::cout << "\n\nTEST: Read from file into a single column vector using back_inserter.\n";

      std::ifstream fileStream("/Users/bob/C/ranges/column_test.dat");
      std::vector<Column> columnVector;
      std::pair<int,int> dim;
      dim = insert_columns_from_stream(fileStream, std::back_inserter(columnVector));

      std::cout << "TEST: column vector has " << columnVector.size() << " columns.\n";
    }

    {
      std::cout << "\n\nTEST: Read from file into a two column vectors.\n";

      std::ifstream fileStream("/Users/bob/C/ranges/column_test.dat");
      std::vector<Column> xColumns, yColumns;
      std::pair<int,int> dim;
      dim = insert_columns_from_stream(fileStream, 2, std::back_inserter(yColumns), std::back_inserter(xColumns));
      std::cout << "TEST: dim for x columns is " << dim << std::endl;
    }

    
    
    // or just read them all into a vector


    std::cout << "\n\nTEST: Second portion of file test.  Inserting from file.\n";
    
    std::pair<int,int> dim;
    std::vector<Column> yColumns;
    std::vector<Column> xColumns;


    dim = insert_columns_from_file("/Users/bob/C/ranges/column_test.dat", 1, std::back_inserter(yColumns), std::back_inserter(xColumns));
    
    std::cout << "TEST: x column vector has " << xColumns.size() << " columns; dims read as "  << dim << std::endl;
    int nRows (yColumns[0]->size());
    std::cout << "TEST: Y column named " << yColumns[0] << " holds " << nRows << " rows.\n";


    
    // or read them from a name file and a data file of rows.
    // These two are made by Dean's c4.5 syntax program test. (NEED to add number of rows.)
    // run as cat test/nrows test/c45_test.rows | column.test.exec`
    /*
      if(false)
    { int nRows (5);
      std::vector<Column> cols;
      std::cout << "TEST: Read "
		<< insert_columns_from_stream(std::cin, "test/c45_test.names", nRows, std::back_inserter(cols))
		<< " columns from stdin.\n";
      for (unsigned int j=0; j<cols.size(); ++j)
	std::cout << "     " << cols[j] << std::endl;
    }
    */
    return 0;
  }
}
  
