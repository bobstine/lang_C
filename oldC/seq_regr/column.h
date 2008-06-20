// $Id: column.h,v 1.12 2004/08/25 22:15:28 bob Exp $

#ifndef _COLUMN_H_
#define _COLUMN_H_

/*

  A column stream is any forward iterator whose * operator provides a
  Column item.  The column item must provide

     - a name string
     - a range
     - an average
	 
  15 Mar 03 ... Created.

*/

#include "range.h"

#include <utility>
#include <iterator>
#include <set>
#include <vector>


class Column
{
  std::string     mName;
  double          mAvg;
  double          mMin, mMax;
  int             mUnique;    // number distinct values
  range<double*>  mRange;     // space managed by someone else; might be nice to have that be more internal???

 public:
  ~Column() { }
  
  Column()
    : mName(""), mAvg(0.0), mMin(0.0), mMax(0.0), mUnique(0), mRange() {  }

  Column(Column const& c)
    : mName(c.mName), mAvg(c.mAvg), mMin(c.mMin), mMax(c.mMax), mUnique(c.mUnique), mRange(c.mRange) { }
  
  Column(char const* name, double *begin, double *end)
    : mName(name), mAvg(0.0), mMin(0.0), mMax(0.0), mUnique(0), mRange(begin,end) { init_fields(); }
  
  Column(char const* name, double avg, double min, double max, int uniq, double *begin, double *end)
    : mName(name), mAvg(avg), mMin(min), mMax(max), mUnique(uniq), mRange(begin,end) { }

  int             size()          const { return end(mRange) - begin(mRange); }
  std::string     name()          const { return mName; }
  double          average()       const { return mAvg; }
  double          scale()         const { return (mMax - mMin)/6.0; }
  double          min()           const { return mMin; }
  double          max()           const { return mMin; }
  int             unique()        const { return mUnique; }
  double          element(int i)  const { return *(begin(mRange)+i); }
  range<double*>  range()         const { return mRange; }
  double*         memory()        const { return begin(mRange); }
  
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
  const int maxNameLength = 128 ;
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
    { open_file(); read_from_file(); }
  
  Column            operator*()  const { return Column(mCurrentName,
						    mCurrentAvg,
						    mMin, mMax, mUniqueSet.size(),
						    mCurrentColumn, mCurrentColumn+mN); }
  FileColumnStream& operator++()        { ++mCount; read_from_file(); return *this; }

  int               position()   const { return mCount; }
  int               n()          const { return mN; }
  void              close_file()       { fclose(mFile); }
  
 private:
  bool open_file();
  bool read_from_file();
};

// These two read columns from a file, optionally using the first k as y's (column at a time)

int
insert_columns_from_file (std::string const& fileName, std::back_insert_iterator< std::vector<Column> > it);

int
insert_columns_from_file (std::string const& fileName, int ny,
			  std::back_insert_iterator< std::vector<Column> > yIt,
			  std::back_insert_iterator< std::vector<Column> > xIt);

// This versions make columns from an input stream (row at a time)

int
insert_columns_from_stream (FILE *input, std::string const& nameFile, int nRows,
			    std::back_insert_iterator< std::vector<Column> > it);

#endif
