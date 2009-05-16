// $Id: column.h,v 1.19 2008/02/15 21:50:23 bob Exp $

#ifndef _COLUMN_H_
#define _COLUMN_H_

/*
 A column is essentially a decorated range of doubles. These are typically defined by
 reading from a stream. The column item provides
     - name string
     - range
     - basic stats, including the average, number that are unique, min, and max
	 
 A column stream is a forward iterator whose * operator provides a Column item. When
 initially defined from data, the column allocates and then manages a ref-counted pointer.

 13 May 09 ... Dynamic column storage.
 15 Nov 07 ... Diddle with getting a pair back from reading data.
 15 Mar 03 ... Created.
*/


#include <utility>
#include <iterator>
#include <set>
#include <map>
#include <vector>

#include <iostream>


#include "range.h"

// pointer that manages memory for columns that allocate dynamic memory

class ColumnDataPtr
{
 private:
  int    * mRefCount;
  double * mBegin;
  double * mEnd;

 public:

  ~ColumnDataPtr()                      { --*mRefCount; if(0 == *mRefCount) { delete mRefCount; delete[] mBegin; }}

  ColumnDataPtr(size_t n)               : mRefCount(new int), mBegin(new double[n]), mEnd(mBegin+n) { *mRefCount = 1; assert(mBegin != NULL); }

  ColumnDataPtr(ColumnDataPtr const& d) : mRefCount(d.mRefCount), mBegin(d.mBegin), mEnd(d.mEnd)    { ++ *mRefCount; }


  ColumnDataPtr& operator= (ColumnDataPtr const& d)  { mRefCount = d.mRefCount; ++ *mRefCount; mBegin = d.mBegin; mEnd = d.mEnd; return *this; }

  double * begin() const { return mBegin;}
  double * end()   const { return mEnd;  }
  
};


class Column
{
private:
  std::string         mName;
  int                 mN;
  double              mAvg;
  double              mMin, mMax;
  int                 mUnique;          // number distinct values
  ColumnDataPtr       mData;

 public:
  ~Column() {  }
  
 Column() : mName(""), mN(0), mAvg(0), mMin(0), mMax(0), mUnique(0), mData(0) {}

  // copy constructor
 Column(Column const& c)  // why do these get copied SO much
   : mName(c.mName+"_c"), mN(c.mN), mAvg(c.mAvg), mMin(c.mMin), mMax(c.mMax), mUnique(c.mUnique), mData(c.mData) {}

 // empty space; will need to call 'update' after filling contents
 Column(char const* name, int n)
    : mName(name), mN(n), mAvg(0.0), mMin(0.0), mMax(0.0), mUnique(0), mData(n) { }

  
 // fill from file (template specialization)  [ kludge to avoid template problem ]
 Column(char const* name, size_t n, FILE *fp)
    : mName(name), mN(n), mAvg(0.0), mMin(0.0), mMax(0.0), mUnique(0), mData(n)
  { double *x (mData.begin());   // std::cout << "COLM: filling pointer " << x << " in column " << mName << " from file.\n";
    while(n--)
      fscanf(fp, "%lf", x++);  
    init_properties();
  }

  // fill from iterator
  template <class Iter>
  Column(char const* name, size_t n, Iter source)
    : mName(name), mN(n), mAvg(0.0), mMin(0.0), mMax(0.0), mUnique(0), mData(n)
  { double *x (mData.begin());  // std::cout << "COLM: filling pointer " << x << " in column " << mName << " via iterator.\n";
    while(n--)
    { *x = *source;
      ++x; ++source;
    }
    init_properties();
  }
  
  Column& operator= (Column const& c);

  int             size()          const { return mN; }
  std::string     name()          const { return mName; }
  double          average()       const { return mAvg; }
  double          scale()         const { return (mMax - mMin)/6.0; }
  double          min()           const { return mMin; }
  double          max()           const { return mMax; }
  int             unique()        const { return mUnique; }
  double          element(int i)  const { return *(mData.begin()+i); }
  double*         begin()         const { return mData.begin(); }
  double*         end()           const { return mData.end(); }
  range<double*>  writable_range()const { return make_range(begin(), end()); }
  range<double const*> range()    const { return make_range(begin(), end()); }
  
  bool            is_constant()   const { return mUnique == 1; }
  bool            is_dummy()      const { return ((mUnique == 2) && (mMin == 0) && (mMax == 1)); }
  
  void            update()              { init_properties(); }
  void            print_to (std::ostream &os) const;
 private:
  void            init_properties();
};

inline
std::ostream&
operator<<(std::ostream& os, Column const& column)
{
  column.print_to(os);
  return os;
}



namespace {
  const int maxNameLength = 128 ;    // max length of a variable name read from file
}


class FileColumnStream : public std::iterator<std::forward_iterator_tag, Column>
{
  std::string      mFileName;
  int              mN;
  int              mCount;
  char             mCurrentName[maxNameLength];
  Column           mCurrentColumn;
  FILE*            mFile;
  
 public:
  ~FileColumnStream() { if (mFile) fclose(mFile); }
  
  FileColumnStream (std::string const& fileName)
    :
    mFileName(fileName), mN(0), mCount(0), mCurrentName(), mCurrentColumn(), mFile(0)
    { open_file(); read_next_column_from_file(); }
  
  Column            operator*()  const { return mCurrentColumn; }
  FileColumnStream& operator++()        { ++mCount; read_next_column_from_file(); return *this; }

  int               position()   const { return mCount; }
  int               n()          const { return mN;     }
  void              close_file()       { fclose(mFile); }
  
 private:
  bool open_file();
  bool read_next_column_from_file();
};

// These two read columns from a file, optionally using the first ny as y's (column at a time)
// They return the number of observations and number of columns (dim of usual data array)

std::pair<int,int>
insert_columns_from_file (std::string const& fileName, 
                          std::back_insert_iterator< std::vector<Column> > it);

std::pair<int,int>
insert_columns_from_file (std::string const& fileName, int ny,
                          std::back_insert_iterator< std::vector<Column> > yIt,
                          std::back_insert_iterator< std::vector<Column> > xIt);

// This version makes columns from an input stream (one row at a time)

int
insert_columns_from_stream (FILE *input, std::string const& nameFile, int nRows,
                            std::back_insert_iterator< std::vector<Column> > it);

#endif
