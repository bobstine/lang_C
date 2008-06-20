/*
 *  columns_to_rows.cc
 *  utils
 *
 *  Created by Robert Stine on 1/2/08.
 *  Copyright 2008. All rights reserved.
 *
 */

#include <iostream>
#include <sstream>
#include <string>
#include <vector>

int 
main()
{
  // Read variable names from first line of the input (no embedded blanks)  
  std::string        firstLine;
  std::vector< std::string > theNames;
  if (getline(std::cin, firstLine)) {
    std::istringstream inLine (firstLine);
    std::string name;
    while (inLine >> name)
      theNames.push_back(name);
  }
  else return 1;
  int nCols (theNames.size());
  std::clog << "CtoR: Reading " << nCols << " variables ("
    << theNames[0] << ", " << theNames[1] << ", ... " << theNames[nCols-1]
    << ") from standard input.\n";
  
  // now read rest of data into vector of rows
  std::vector< std::vector<double> > theData;
  while (std::cin) {
    std::vector<double> row (nCols);
    for (int j=0; j<nCols; ++j)
      std::cin >> row[j];
    theData.push_back(row);
  }
  
  //                                                          Output looks like...
  // write the number of rows to the file                            n
  int nRows (theData.size());
  std::clog << "CtoR: Writing " << nRows << " cases for each variable onto standard output.\n";
  std::cout << nRows << std::endl;
  for (int j=0; j<nCols; ++j) {
    // write each name on a separate line                            varname_j
    std::cout << theNames[j] << std::endl;
    // then write the data as a long line                            x_1j x_2j ... x_nj
    for (int i=0; i<nRows; ++i)
      std::cout << theData[i][j] << " ";
    std::cout << std::endl;
  }
  return 0;
}

