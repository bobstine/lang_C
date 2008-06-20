// $Id: cell.h,v 1.1 2002/05/17 14:15:13 bob Exp $

#include <iostream>
#include <vector>

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

  void set_boundary(double b)
    {
      mBoundary = b;
    }
  double boundary() const
    {
      return mBoundary;
    }
  double probability() const
    {
      double sum = mSum; return sum/mCount;
    }
  
  void include (const double pred, const int y)
    {
      mBoundary = pred;
      ++mCount;
      mSum = mSum + y;
    }

  void combineWith(const Cell* cell)
    {
      if (cell->mBoundary > mBoundary) 
	mBoundary = cell->mBoundary;
      mCount = mCount + cell->mCount;
      mSum = mSum + cell->mSum;
    }

  void read (std::istream& input)
    {
      input >> mBoundary >> mCount >> mSum;
    }
  
  void write (std::ostream& output) const
    {
      output << mBoundary << " " << mCount << " " << mSum;
    }
};


inline istream&
operator>> (istream& input, Cell& cell)
{
  cell.read(input);
  return input;
}

inline ostream&
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
      output << mVector.size() << std::endl;
      for (int i=0; i<mVector.size(); ++i)
      {	mVector[i]->write(output);
	output << endl;
      }
    }
};


inline ostream&
operator<< (ostream& output, const CellStack& stack)
{
  stack.write(output);
  return output;
}

