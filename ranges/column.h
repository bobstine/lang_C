// $Id: column.h,v 1.19 2008/02/15 21:50:23 bob Exp $

#ifndef _COLUMN_H_
#define _COLUMN_H_

/*
 A Column puts a name on a numerical range and the various statistics of that range.
 Access to properties of the column come via operator->.

 11 Nov 09 ... Add description field to allow attributes when reading stream format data.
 25 May 09 ... Better form of dynamic storage (see Coplein, p67-68)
 13 May 09 ... Dynamic column storage.
 15 Nov 07 ... Diddle with getting a pair back from reading data.
 15 Mar 03 ... Created.
*/


#include <utility>
#include <iterator>
#include <vector>
#include <assert.h>

#include <iostream>

#include "read_utils.h"
#include "range.h"



class Column;

class ColumnData
{
  friend class Column;

 private:
  std::string mName;
  std::string mDescription;       // store whatever you want here
  int         mN;
  double      mAvg;
  double      mMin, mMax;
  int         mNumUnique; 
  double *    mBegin;
  double *    mEnd;
  int         mRefCount;

 private:
  ~ColumnData()                     { delete[] mBegin; }

 ColumnData(size_t n)            : mBegin(new double[n]), mEnd(mBegin+n), mRefCount(1) { mN = n; assert(mBegin != NULL); }

 public:
  std::string     name()          const { return mName; }
  std::string     description()   const { return mDescription; }
  int             size()          const { return mN; }
  double          average()       const { return mAvg; }
  double          scale()         const { return (mMax - mMin)/6.0; }
  double          min()           const { return mMin; }
  double          max()           const { return mMax; }
  int             num_unique()    const { return mNumUnique; }
  double          element(int i)  const { return *(mBegin+i); }
  double*         begin()         const { return mBegin; }
  double*         end()           const { return mEnd; }
  range<double*>  writable_range()const { return make_range(mBegin, mEnd); }
  range<double const*> range()    const { return make_range(mBegin, mEnd); }
  
  bool            is_constant()   const { return mNumUnique == 1; }
  bool            is_dummy()      const { return ((mNumUnique == 2) && (mMin == 0) && (mMax == 1)); }
  void            update()              { init_properties(); }
  
  void            print_to(std::ostream& os) const;
 private:
  void            init_properties();
};



// Envelope

const int maxColumnNameLength ( 128);

const int maxColumnDescLength (1024);

class Column
{
 private:
  ColumnData       *  mData;            // having pointer makes it possible to have const but change ref count

 public:
  ~Column() { if(--mData->mRefCount <= 0) delete mData; }
  
 Column()                                     : mData( new ColumnData(0) ) {  }

 Column(Column const& c)                      : mData(c.mData) { ++c.mData->mRefCount; }

 Column(char const* name, int n)              : mData( new ColumnData(n) ) { mData->mName = name; mData->mDescription = " ";}  // need to init properties
  
 Column(char const* name, char const* description, size_t n, FILE *fp) : mData( new ColumnData(n) )
  { mData->mName = name;
    mData->mDescription = description;
    double *x (mData->mBegin);   // std::cout << "COLM: filling pointer " << x << " in column " << mName << " from file.\n";
    while(n--)
      fscanf(fp, "%lf", x++);  
    mData->init_properties();
  }

  template <class Iter>
    Column(char const* name, char const* description, size_t n, Iter source) : mData( new ColumnData(n) )
  { mData->mName = name;
    mData->mDescription = description;
    double *x (mData->mBegin);  // std::cout << "COLM: filling pointer " << x << " in column " << mName << " via iterator.\n";
    while(n--)
    { *x = *source;
      ++x; ++source;
    }
    mData->init_properties();
  }
  
  Column& operator= (Column const& c);

  ColumnData* operator->()                const { return mData; }
  
  void        print_to (std::ostream &os) const { os << "Column " ; mData->print_to(os); }
};

inline
std::ostream&
operator<<(std::ostream& os, Column const& column)
{
  column.print_to(os);
  return os;
}



class FileColumnStream : public std::iterator<std::forward_iterator_tag, Column>
{
  std::string      mFileName;
  int              mN;
  int              mCount;
  char             mCurrentName[maxColumnNameLength];
  char             mCurrentDesc[maxColumnDescLength];
  Column           mCurrentColumn;
  FILE*            mFile;
  
 public:
  ~FileColumnStream() { if (mFile) fclose(mFile); }
  
  FileColumnStream (std::string const& fileName)
    :
    mFileName(fileName), mN(0), mCount(0), mCurrentName(), mCurrentDesc(), mCurrentColumn(), mFile(0)
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
