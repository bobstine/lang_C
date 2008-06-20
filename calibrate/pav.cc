// $Id: pav.cc,v 1.1 2002/05/17 14:15:13 bob Exp $

#include <iostream>
#include <vector>
#include "cell.h"

/*
  This program reads a sorted two column file in the layout

          y-hat_1 y_1
	  y-hat_2 y_2
	  ...
	  y-hat_n y_n

  where the values have been sorted on the first column.

  The output of this program is list of cells that can be
  read by the associated calibration program.
*/

typedef std::pair<double, int> Observation;

istream&
operator>> (istream& input, Observation& obs)
{
  input >> obs.first >> obs.second;
  return input;
}
ostream&
operator<< (ostream& output, const Observation& obs)
{
  output << " <" << obs.first << " , " << obs.second << "> ";
  return output;
}

main()
{
  vector<Observation> data;

  // Read the input data from standard input
  Observation obs;
  while (cin >> obs)
    data.push_back(obs);
  int n = data.size();
  std::cerr << "Read " << n << " input pairs.\n";
  
  // Initialize pool adjacent violators
  CellStack stack;
  int i = 0;
  while (data[i].second == 0) ++i;
  Cell* activeCell = new Cell(data[i-1].first, i);
  // std::cerr << "Initial cell is " << *activeCell << endl;
  
  // Build the monotone list
  for( ; i < n ; ++i) {
    // std::cerr << "Top of main loop with stack " << stack << endl;
    if (data[i].second == 1)
    { stack.push(activeCell);
      activeCell = new Cell(data[i].first,1,1);
    } else {
      activeCell->include(data[i].first, data[i].second);
    }
    if (activeCell->probability() < stack.probability())
    { // std::cerr << "Merging with active cell " << *activeCell << endl;
      stack.merge_top_into(activeCell);
      std::cerr << "Merged cell " << *activeCell << endl;
    }
  }
  stack.merge_top_into(activeCell);
  stack.push(activeCell);
  
  // Print it
  std::cerr << "Final pool adjacent violators stack ... \n";
  std::cout << stack;

}
