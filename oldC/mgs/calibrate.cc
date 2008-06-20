// $Id: calibrate.cc,v 1.1 2002/04/30 21:33:07 bob Exp $

#include <iostream>
#include <vector>

/*
  This program reads a sorted two column file in the layout

          y-hat_1 y_1
	  y-hat_2 y_2
	  ...
	  y-hat_n y_n

  where the values have been sorted on the first column. The
  output of this program is a new list of values, now triples
  that add a column of calibrated y-hat values.

*/

typedef std::pair<double, int> Observation;

class Cell {
  double mBoundary;
  int mCount;
  int mSum;
public:
  Cell(double boundary)
    : mBoundary(boundary), mCount(1), mSum(0) { }
  Cell(double boundary, int count)
    : mBoundary(boundary), mCount(count), mSum(0) { }
  Cell(double boundary, int count, int sum)
    : mBoundary(boundary), mCount(count), mSum(sum) { }

  double boundary() const
    {
      return mBoundary;
    }
  double probability() const
    {
      double sum = mSum; return sum/mCount;
    }
  
  void include (Observation &obs)
    {
      mBoundary = obs.first;
      ++mCount;
      mSum = mSum + obs.second;
    }

  void combineWith(const Cell* cell)
    {
      if (cell->mBoundary > mBoundary) 
	mBoundary = cell->mBoundary;
      mCount = mCount + cell->mCount;
      mSum = mSum + cell->mSum;
    }
  
  void write (std::ostream& output) const
    {
      output << "[" << mBoundary << "] "
	     << mCount << " , " << mSum
	     << "   (" << probability() << ") ";
    }
};

ostream&
operator<< (ostream& output, const Cell& cell)
{
  cell.write(output);
  return output;
}


class CellStack {
  vector<Cell*> mVector;
public:
  CellStack ()
    : mVector() { }

  double probability()
    {
      return mVector.back()->probability();
    }
  Cell *operator[](int j) const
    {
      return mVector[j];
    }
  
  Cell* pop ()
    {
      Cell* result = mVector.back();
      mVector.pop_back();
      return result;
    }
  
  void push (Cell* cell)
    {
      mVector.push_back(cell);
    }
  
  void merge_top_into (Cell *cell)
    {
      while (probability() > cell->probability())
      { Cell* top = pop();
	cell->combineWith(top);
	delete top;
      }
    }

  void  write (ostream& output) const
    {
      output << "Cell stack with " << mVector.size() << " elements:\n";
      for (int i=0; i<mVector.size(); ++i)
      {	mVector[i]->write(output);
	output << endl;
      }
    }
};

istream&
operator>> (istream& input, Observation& obs)
{
  input >> obs.first >> obs.second;
  return input;
}


ostream&
operator<< (ostream& output, const CellStack& stack)
{
  stack.write(output);
  return output;
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
  std::cout << "Read " << n << " input pairs.\n";
  
  // Initialize pool adjacent violators
  CellStack stack;
  int i = 0;
  while (data[i].second == 0) ++i;
  Cell* activeCell = new Cell(data[i-1].first, i);
  cout << "Initial cell is " << *activeCell << endl;
  
  // Build the monotone list
  for( ; i < n ; ++i) {
    cout << "Top of main loop with stack " << stack << endl;
    if (data[i].second == 1)
    { stack.push(activeCell);
      activeCell = new Cell(data[i].first,1,1);
    } else {
      activeCell->include(data[i]);
    }
    if (activeCell->probability() < stack.probability())
    { cout << "Merging with active cell " << *activeCell << endl;
      stack.merge_top_into(activeCell);
      cout << "Merged cell " << *activeCell << endl;
    }
  }
  stack.merge_top_into(activeCell);
  stack.push(activeCell);
  
  // Print it
  cout << "Final stack ... " << stack;
  
  // Generate predictions of original data
  for (int i=0, j=0; i<n; ++i)
  { while (data[i].first > stack[j]->boundary()) ++j;
    cout << data[i].first << " "              // y-hat
	 << data[i].second << " "             // y
	 << stack[j]->probability() << endl;  // calibrated y-hat
  }
}

    

	     
  
