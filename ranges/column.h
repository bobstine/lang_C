#ifndef _COLUMN_H_
#define _COLUMN_H_

/*
 A Column puts a name on a numerical range and the various statistics of that range.
 Access to properties of the column come via operator->.

 27 Apr 10 ... Column streams to use an input stream rather than file; columns have a 'role'
 11 Nov 09 ... Add description field to allow attributes when reading stream format data.
 25 May 09 ... Better form of dynamic storage (see Coplein, p67-68)
 13 May 09 ... Dynamic column storage.
 15 Nov 07 ... Diddle with getting a pair back from reading data.
 15 Mar 03 ... Created.
*/


#include <utility>
#include <iterator>
#include <vector>
#include <map>
#include <assert.h>

#include <iostream>
#include <math.h>

#include "read_utils.h"
#include "range.h"
#include "debug.h"



class Column;
class IntegerColumn;


//     Double Column     Double Column     Double Column     Double Column     Double Column     Double Column     


class ColumnData
{
  friend class Column;

 private:
  std::string mName;
  std::string mRole;              // such as y, x or context
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

 ColumnData(size_t n)            : mBegin(new double[n]), mEnd(mBegin+n), mRefCount(1) { mN = (int)n; assert(mBegin != NULL); }

 public:
  std::string     name()          const { return mName; }
  std::string     role()          const { return mRole; }
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
  Ranges::range<double*>  writable_range()const { return Ranges::make_range(mBegin, mEnd); }
  Ranges::range<double const*> range()    const { return Ranges::make_range(mBegin, mEnd); }
  
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
  ColumnData       *  mData;            // pointer makes it possible to have const but change ref count

 public:
  ~Column() { if(--mData->mRefCount <= 0) delete mData; }
  
 Column()                                     : mData( new ColumnData(0) ) {  }

 Column(Column const& c)                      : mData(c.mData) { ++c.mData->mRefCount; }

 Column(char const* name, int n)              : mData( new ColumnData(n) ) {
    mData->mName = name;
    mData->mRole="";
    mData->mDescription=""; }

 Column(char const* name, char const* description, size_t n, FILE *fp) : mData( new ColumnData(n) )
  { mData->mName = name;
    mData->mRole = extract_role_from_string(description);
    mData->mDescription = description;
    double *x (mData->mBegin);  
    while(n--)
      fscanf(fp, "%lf", x++);  
    mData->init_properties();
  }

 Column(std::string name, std::string description, size_t n, std::istream& is) : mData( new ColumnData(n) )
  { size_t expect (n);
    mData->mName = name;
    mData->mRole = extract_role_from_string(description);
    mData->mDescription = description;
    double *x (mData->mBegin);  
    while(n-- && is >> *x)
      ++x;
    ++n;
    if(n)
      debugging::debug("CLMN",-1) << "Error. Incomplete column read of column '" << name << "'. Expecting " << expect << " but read " << expect-n << std::endl;
    std::string rest;
    getline(is, rest);             // dump rest of data line
    mData->init_properties();
  }

  template <class Iter>
    Column(char const* name, char const* description, size_t n, Iter source) : mData( new ColumnData(n) )
  { mData->mName = name;
    mData->mRole = extract_role_from_string(description);
    mData->mDescription = description;
    double *x (mData->mBegin);
    while(n--)
    { *x = *source;
      ++x; ++source;
    }
    mData->init_properties();
  }
  
  Column& operator= (Column const& c);
  
  ColumnData* operator->()                const { return mData; }
  
  void        print_to (std::ostream &os) const { os << "Column " ; mData->print_to(os); }

 private:

  std::string  extract_role_from_string(std::string const& desc) const;
  
};

inline
std::ostream&
operator<<(std::ostream& os, Column const& column)
{
  column.print_to(os);
  return os;
}


// Read columns as a stream from a stream input object:
//        First line of stream gives number of observations expected for each column.
//        Seems to work okay so long as this number is >= to the number present.
//        Then come triples of lines for each column: name, description, data
class ColumnStream : public std::iterator<std::forward_iterator_tag, Column>
{
  std::istream&    mStream;
  std::string      mStreamName;
  int              mN;
  int              mCount;
  std::string      mCurrentName;
  std::string      mCurrentDesc;
  Column           mCurrentColumn;

  
 public:
  ~ColumnStream() {  }
  
  ColumnStream (std::istream& is, std::string name)
    :  mStream(is), mStreamName(name), mN(0), mCount(0), mCurrentName(), mCurrentDesc(), mCurrentColumn()
    { if (is) { initialize(); read_next_column(); } }

  std::string   currentName()        const { return mCurrentName; }
  std::string   currentDescription() const { return mCurrentDesc; }
  
  Column        operator*()          const { return mCurrentColumn; }
  ColumnStream& operator++()               { ++mCount; read_next_column(); return *this; }

  int           position()           const { return mCount; }
  int           n()                  const { return mN;     }
  
 private:
  void initialize();
  bool read_next_column();
};

// Return number of observations and number of columns
std::pair<int,int>
  insert_columns_from_stream (std::istream& is,
			      std::back_insert_iterator< std::vector<Column> > it);

// Return collection of column vectors based on named variables
// Names obtained from columnVector field as *first* pair of strings on description line
typedef std::map<std::string, std::back_insert_iterator< std::vector<Column> > > NamedColumnInsertMap;

void
insert_columns_from_stream (std::istream& is, NamedColumnInsertMap insertMap);




// This version makes columns from an input file using C io ratther than C++

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
  FileColumnStream& operator++()       { ++mCount; read_next_column_from_file(); return *this; }

  int               position()   const { return mCount; }
  int               n()          const { return mN;     }
  void              close_file()       { fclose(mFile); }
  
 private:
  bool open_file();
  bool read_next_column_from_file();
};



std::pair<int,int>
insert_columns_from_file (std::string const& fileName, 
                          std::back_insert_iterator< std::vector<Column> > it);


std::pair<int,int>
insert_columns_from_file (std::string const& fileName, int ny,
                          std::back_insert_iterator< std::vector<Column> > yIt,
                          std::back_insert_iterator< std::vector<Column> > xIt);

int
insert_columns_from_file (FILE *is, std::string const& nameFileName, int nRows,
			  std::back_insert_iterator< std::vector<Column> > it);

//    Integer Columns     Integer Columns     Integer Columns     Integer Columns     Integer Columns     

class IntegerColumnData
{
  friend class IntegerColumn;
  
 private:
  std::string mName;
  int mSize;
  int *mBegin;
  int *mEnd;
  int mRefCount;
  
  ~IntegerColumnData()   { delete[] mBegin; }

 IntegerColumnData(std::string name, size_t n)  : mName(name), mSize(n), mBegin(new int[n]), mEnd(mBegin+n), mRefCount(1) { assert(mBegin != NULL); }

 public:
  std::string     name()     const { return mName; }
  int             size()     const { return mSize; }
  int *           begin()    const { return mBegin; }
  int *           end()      const { return mEnd; }
};

class IntegerColumn
{
 private:
  IntegerColumnData       *mData; 

 public:
  ~IntegerColumn() { if(--mData->mRefCount <= 0) delete mData; }
  
 IntegerColumn()                                     : mData( new IntegerColumnData("",0) ) {  }
 IntegerColumn(IntegerColumn const& c)               : mData(c.mData) { ++c.mData->mRefCount; }
 IntegerColumn(Column const& c)                      : mData( new IntegerColumnData(c->name(),c->size()) ) { transfer_from_double(c->begin()); }
  
  IntegerColumnData* operator->()                const { return mData; }

  void print_to(std::ostream& os) const
  { int counter (mData->size());
    if (counter > 10) counter = 10;
    os << mData->name() << "[" << mData->size() << "]";
    int * pInt (mData->begin());
    while(counter--) os << " " << *pInt++;
  }
  
 private:
  void transfer_from_double (double *pDouble)
  { int counter (mData->size());
    int *pDest  (mData->begin()); 
    while(counter--) *pDest++ = floor(*pDouble++);
  }
};

inline
std::ostream&
operator<<(std::ostream& os, IntegerColumn const& column)
{
  column.print_to(os);
  return os;
}


#endif
