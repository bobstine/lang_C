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
  debugging::debug_init(std::clog, 2);

  typedef float Scalar;
  
  if (true)
  {
    std::cout << "\n\nTEST: dynamic columns \n";

    int n = 100;
    std::vector<Scalar> x(n);
    std::vector<Scalar> y(n);
    for (int i=0; i<n; ++i)
    { x[i] = (Scalar) 7.0 * (Scalar)i;
      y[i] = (Scalar)i;
    }
    
    Column<Scalar> c1 ("c1", "description 1", n, x.begin());
    Column<Scalar> c2 (c1);
    Column<Scalar> c3 (c2);
    Column<Scalar> c4 ("c4", "description 2", n, y.begin());
    Column<Scalar> c5 (c4);
    
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
  }

  {     // read the columns one at a time via the iterator interface    
    std::cout << "\n\nTEST: File portion of test.\n";
    
    if (false)
    {
      std::cout << "\n\nTEST: Read from file into a column vector manually.\n";

      std::ifstream fileStream("/Users/bob/C/ranges/column_test.dat");
      ColumnStream<Scalar> columnStream(fileStream, "column_test.dat");

      std::vector<Column<Scalar>> columnVector;
      std::cout << "TEST: Length of elements in column stream is n = " << columnStream.n() << std::endl;    
      for (int j=0; j<30; ++j, ++columnStream) // try to read past end of columns
      { Column<Scalar> x = *columnStream;
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


    if (false)
    {
      std::cout << "\n\nTEST: Read from file into a single column vector using back_inserter.\n";

      std::ifstream fileStream("/Users/bob/C/ranges/column_test.dat");
      std::vector<Column<Scalar>> columnVector;
      std::pair<int,int> dim;
      dim = insert_columns_from_stream(fileStream, std::back_inserter(columnVector));
      std::cout << "TEST: column vector has " << columnVector.size() << " columns.\n";
    }

    if (false)
    {
      std::cout << "\n\nTEST: Read from file into labeled back-insert iterators.\n";

      std::ifstream fileStream("/Users/bob/C/ranges/column_test.dat");
      std::vector<Column<Scalar>> xColumns;
      std::vector<Column<Scalar>> yColumns;
      std::map<std::string, std::back_insert_iterator< std::vector<Column<Scalar>> > > insertMap;
      insertMap.insert(std::make_pair("y", std::back_inserter(yColumns)));   // insert as pair since no default for back inserter
      insertMap.insert(std::make_pair("x", std::back_inserter(xColumns)));
      insert_columns_from_stream(fileStream, insertMap); 
      std::cout << "TEST: read " << yColumns.size() << " y columns and read " << xColumns.size() << " x columns\n";
    }

    
    
    // or just read them all into a vector
    
    {
      std::cout << "\n\nTEST: Second portion of file test.  Inserting from file.\n";
    
      std::pair<int,int> dim;
      std::vector<Column<Scalar>> yColumns;
      std::vector<Column<Scalar>> xColumns;
      
      // dim = insert_columns_from_file("/Users/bob/C/ranges/column_test.dat", 1, std::back_inserter(yColumns), std::back_inserter(xColumns));
      // std::cout << "TEST: x column vector has " << xColumns.size() << " columns; dims read as "  << dim << std::endl;

      dim = insert_columns_from_file("/home/bob/C/projects/prep_error/auction_data/multinomial/Y_of",  std::back_inserter(yColumns));
      std::cout << "TEST: Read Y file with dim = " << dim.first << "x" << dim.second << std::endl;
      int nRows (yColumns[0]->size());
      std::cout << "TEST: Y column named " << yColumns[0] << " holds " << nRows << " rows.\n";
      dim = insert_columns_from_file("/home/bob/C/projects/prep_error/auction_data/multinomial/X",  std::back_inserter(yColumns));
      std::cout << "TEST: Read X file with dim = " << dim.first << "x" << dim.second << std::endl;
    }
      
      
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
  
