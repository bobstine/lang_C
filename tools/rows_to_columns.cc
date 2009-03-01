/*
 *  rows_to_columns.cc
 *  tools
 *
 *  Created by Robert Stine on 3/1/2009.
 *  Copyright 2009. All rights reserved.
 *
 */

#include <iostream>
#include <string>
#include <vector>
#include <iterator>
#include <algorithm>

#include <cstdio>

int 
main()
{
  // Read lengths and number of columns from first line
  int nRow, nCol;
  std::cin >> nRow >> nCol;
  std::clog << "RtoC: Writing " << nRow << " cases for " << nCol << " columns onto standard output.\n";

  // make space for numerical data
  std::vector< std::string > varNames (nCol);
  std::vector< std::vector<double> > data (nCol);
  
  // read rest of data
  std::string inputLine;
  for(int col = 0; col < nCol; ++col)
  { // flush rest of line
    getline(std::cin, inputLine);
    // read var name
    getline(std::cin, varNames[col]);
    // read data for this column
    for (int i=0; i<nRow; ++i)
    { double x;
      std::cin >> x;
      data[col].push_back(x);
    }
  }

  // write data back to standard output
  std::copy(varNames.begin(), varNames.end(), std::ostream_iterator< std::string >(std::cout," "));
  std::cout << std::endl;
  for (int row=0; row<nRow; ++row)
  { for (int col=0; col<nCol; ++col)
      std::cout << data[col][row] << " ";
    std::cout << std::endl;
  }
  return 0;
}

