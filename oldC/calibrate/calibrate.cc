// $Id: calibrate.cc,v 1.3 2002/05/17 14:15:13 bob Exp $

#include <iostream>
#include <vector>
#include "cell.h"

/*
  This program reads a list of pooled adjacent violator cells
  (as produced by pav), then applies them to data in the form

          y-hat_1 y_1
	  y-hat_2 y_2
	  ...
	  y-hat_n y_n

  where the values have been sorted on the first column. The
  output of this program is a new list of values, with y-hats
  replaced by the calibrated values.
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
<<<<<<< calibrate.cc
  // Read the calibration cells from std input
  CellStack stack;
  Cell* cell;
  int n;
  std::cin >> n;
  for(int i=0; i<n; ++i)
    {
      cell = new Cell(0.0);
      cell->read(std::cin);
      stack.push(cell);
    }

  // Add guard cell to avoid going over the end
  Cell* guardCell = new Cell(0.0);
  *guardCell = *cell;
  guardCell->set_boundary(100.0);
  stack.push(guardCell);
  std::cerr << "Read stack (with added boundary): \n" << stack << endl;
  
  // Generate predictions of input data
=======
  vector<Observation> data;

  // Read the input data from standard input
  double priorPred = -99999999.9;
>>>>>>> 1.2
  Observation obs;
<<<<<<< calibrate.cc
  int j=0;
=======
  
>>>>>>> 1.2
  while (cin >> obs)
<<<<<<< calibrate.cc
    {
      while (obs.first > stack[j]->boundary()) ++j;
      cout << stack[j]->probability() << " "        // calibrated y-hat
	   << obs.second              << endl;      // y
=======
  {
    if (obs.first < priorPred)
      cerr << "Error: Data are not in order...\n";
    data.push_back(obs);
    priorPred = obs.first;
  }
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
      activeCell->include(data[i]);
>>>>>>> 1.2
    }
}

    

	     
  
