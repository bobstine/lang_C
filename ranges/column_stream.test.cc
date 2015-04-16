#include "column_stream.Template.h"
#include "column.Template.h"
#include "print_utils.h"
#include "debug.h"

#include <cstdio>
#include <iostream>
#include <fstream>
#include <vector>
#include <iterator>


int
main()   
{
  using std::string;
  using debugging::debug;
  debugging::debug_init(std::clog, 5);
  
  typedef float Scalar;
  

  if (true)
  {
    std::cout << "\n\nTEST: NLP-style variables from maps.\n";
    // make a small test dictionary; note code will use full dictionary
    std::map<string,std::vector<Scalar>> dict;
    std::vector<Scalar> v (5); v[0]=10.0; v[1]=11.0; v[2]=12.0; v[3]=13.0; v[4]=14.0;
    dict["w1"] = v;
    for(size_t i=0; i<v.size(); ++i)  v[i] += 10;
    dict["w2"] = v;
    for(size_t i=0; i<v.size(); ++i)  v[i] += 10;
    dict["w3"] = v;
    for(size_t i=0; i<v.size(); ++i)  v[i] += 10;
    dict["w4"] = v;
    for(size_t i=0; i<v.size(); ++i)  v[i] += 10;  
    dict["w5"] = v;
    for(size_t i=0; i<v.size(); ++i)  v[i] += 100;  
    dict["OOV"] = v;  // need to define OOV and NA items   (unsure why auto generates a compiler warning on unused)
    for(size_t i=0; i<v.size(); ++i) v[i] = std::nanf("missing"); 
    dict["NA"] = v;
    std::cout << "TEST: dictionary item dict[w1]= \n          " << dict["w1"] << std::endl;
    std::cout << "TEST: dictionary item dict[w2]= \n          " << dict["w2"] << std::endl;

    std::vector<Column<Scalar>> columns;
    const string testFile = "test.data/column_stream_test.dat";
    std::ifstream is{testFile};
    const size_t minCategorySize = 10;
    std::pair<size_t,size_t> nInserted = insert_columns_from_stream (is, minCategorySize, dict, std::back_inserter(columns));

    std::cout << "TEST: Values of third variable " << columns[3]->name() << "[" << columns[3]->size() << "]: \n";
    for(size_t i=0; i<(size_t) columns[3]->size(); ++i)
      std::cout << columns[3]->element(i) << " ";
    std::cout << std::endl;

    std::cout << "TEST: Values of sixth variable " << columns[6]->name() << "[" << columns[6]->size() << "]: \n";
    for(size_t i=0; i<(size_t) columns[6]->size(); ++i)
      std::cout << columns[6]->element(i) << " ";
    std::cout << std::endl;
    
    std::cout << "TEST: Inserted " << nInserted.second << " columns from file having " << nInserted.first
	      << "; produces vector<columns> with size " << columns.size() << " (which better match number inserted).\n";
    std::cout << "    1st is \n          " << columns[0] << std::endl;
    std::cout << "    2nd is \n          " << columns[1] << std::endl;
    std::cout << "    3rd is \n          " << columns[2] << std::endl;
    for (size_t j=3; j<columns.size(); ++j)
      std::cout << "    " << j+1 << " is \n          " << columns[j] << std::endl;
  }

  if(false)
  { 
    std::cout << "\n\nTEST: File portion of test.\n";
    
    if (false)
      {
      std::cout << "\n\nTEST: Read from file into a column vector manually.\n";

      std::ifstream fileStream("test.data/column_stream_test.dat");
      ColumnStream<Scalar> columnStream(fileStream, "test.data/column_stream_test.dat");

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

      std::ifstream fileStream("test.data/column_stream_test.dat");
      std::vector<Column<Scalar>> columnVector;
      std::pair<int,int> dim;
      //      dim = insert_columns_from_stream(fileStream, std::back_inserter(columnVector));
      std::cout << "TEST: column vector has " << columnVector.size() << " columns.\n";
    }

    if (false)
    {
      std::cout << "\n\nTEST: Read from file into labeled back-insert iterators.\n";

      std::ifstream fileStream("test.data/column_stream_test.dat");
      std::vector<Column<Scalar>> xColumns;
      std::vector<Column<Scalar>> yColumns;
      std::map<std::string, std::back_insert_iterator< std::vector<Column<Scalar>> > > insertMap;
      insertMap.insert(std::make_pair("y", std::back_inserter(yColumns)));   // insert as pair since no default for back inserter
      insertMap.insert(std::make_pair("x", std::back_inserter(xColumns)));
      insert_columns_from_stream(fileStream, insertMap); 
      std::cout << "TEST: read " << yColumns.size() << " y columns and read " << xColumns.size() << " x columns\n";
    }
  } 
    
  // or just read them all into a vector
  
  if (false)
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
}
    
