// $Id: column.h,v 1.19 2008/02/15 21:50:23 bob Exp $

#ifndef _COLUMN_H_
#define _COLUMN_H_

/*
 A column is essentially a decorated range of doubles. These are typically
 defined by reading from a stream. The column item provides
     - name string
     - range
     - basic stats, including the average, number that are unique, min, and max
	 
 A column stream is a forward iterator whose * operator provides a
 Column item. The implemented example, called here a FileColumnStream 
 allocates the space for the data when data is read from a file.  It 
 does not free this space (currently).
 
 15 Nov 07 ... Diddle with getting a pair back from reading data.
 15 Mar 03 ... Created.
*/

#include "range.h"
using namespace Ranges;

#include <utility>
#include <iterator>
#include <set>
#include <vector>


class Column
{
private:
  std::string     mName;
  double          mAvg;
  double          mMin, mMax;
  int             mUnique;    // number distinct values
  range<double*>  mRange;     // space managed by someone else; would be nice to manage internally ???

 public:
  ~Column() { }
  
  Column(Column const& c)
    : mName(c.mName), mAvg(c.mAvg), mMin(c.mMin), mMax(c.mMax), mUnique(c.mUnique), mRange(c.mRange) { }
  
  Column(char const* name, double *begin, double *end)
    : mName(name), mAvg(0.0), mMin(0.0), mMax(0.0), mUnique(0), mRange(begin,end) { init_fields(); }
  
  Column(char const* name, double avg, double min, double max, int uniq, double *begin, double *end)
    : mName(name), mAvg(avg), mMin(min), mMax(max), mUnique(uniq), mRange(begin,end) { }
  
  Column& operator= (Column const& c);

  int             size()          const { return end(mRange) - Ranges::begin(mRange); }
  std::string     name()          const { return mName; }
  double          average()       const { return mAvg; }
  double          scale()         const { return (mMax - mMin)/6.0; }
  double          min()           const { return mMin; }
  double          max()           const { return mMin; }
  int             unique()        const { return mUnique; }
  double          element(int i)  const { return *(Ranges::begin(mRange)+i); }
  double*         begin()         const { return Ranges::begin(mRange); }
  range<double*>  range()         const { return mRange; }
  double*         memory()        const { return Ranges::begin(mRange); }
  
  bool            is_constant()   const { return mUnique == 1; }
  bool            is_dummy()      const { return ((mUnique == 2) && (mMin == 0) && (mMax == 1)); }

  void            update()              { init_fields(); }
  void            print_to (std::ostream &os) const;
 private:
  void            init_fields();
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
  double           mCurrentAvg, mMin, mMax;
  std::set<double> mUniqueSet;
  double*          mCurrentColumn;
  FILE*            mFile;
  
 public:
  ~FileColumnStream() { if (mCurrentColumn) delete mCurrentColumn; }
  
  FileColumnStream (std::string const& fileName)
    :
    mFileName(fileName), mN(0), mCount(0), mCurrentName(),
    mCurrentAvg(0.0), mMin(0.0), mMax(0.0), mUniqueSet(), mCurrentColumn(0), mFile(0)
    { open_file(); read_next_column_from_file(); }
  
  Column            operator*()  const { return Column(mCurrentName,
                                                       mCurrentAvg,
                                                       mMin, mMax, mUniqueSet.size(),
                                                       mCurrentColumn, mCurrentColumn+mN); }
  FileColumnStream& operator++()        { ++mCount; read_next_column_from_file(); return *this; }

  int               position()   const { return mCount; }
  int               n()          const { return mN; }
  void              close_file()       { fclose(mFile); }
  
 private:
  bool open_file();
  bool read_next_column_from_file();
};

// These two read columns from a file, optionally using the first ny as y's (column at a time)
// They return the number of observations and number of columns (dim of usual data array)

std::pair<double,double>
insert_columns_from_file (std::string const& fileName, 
                          std::back_insert_iterator< std::vector<Column> > it);

std::pair<double,double>
insert_columns_from_file (std::string const& fileName, int ny,
                          std::back_insert_iterator< std::vector<Column> > yIt,
                          std::back_insert_iterator< std::vector<Column> > xIt);

// This version makes columns from an input stream (one row at a time)

int
insert_columns_from_stream (FILE *input, std::string const& nameFile, int nRows,
                            std::back_insert_iterator< std::vector<Column> > it);

#endif
