#ifndef _COLUMN_H_
#define _COLUMN_H_

/*
 A Column puts a name on a numerical range and the various statistics of that range.
 Access to properties of the column come via operator->.

 10 Mar 14 ... Template over field of numbers
 24 Aug 10 ... Add integer columns.
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
#include <set>
#include <assert.h>

#include <iostream>
#include <math.h>

#include "read_utils.h"
#include "range.h"
#include "debug.h"


template <class F>
class Column;

class IntegerColumn;


//     ColumnData     ColumnData     ColumnData     ColumnData     ColumnData     ColumnData     ColumnData     ColumnData     


template <class F>
class ColumnData
{
  friend class Column<F>;

 public:
  typedef F Scalar;
  
 private:
  std::string mName;
  std::string mRole;              // such as y, x or context
  std::string mDescription;       // store whatever you want here
  int         mN;
  Scalar      mAvg;
  Scalar      mMin, mMax;
  int         mNumUnique;
  std::set<F> mUniqueElements; // only held if mNumUnique is small
  Scalar *    mBegin;
  Scalar *    mEnd;
  int         mRefCount;
 
 private:
  ~ColumnData()                 { delete[] mBegin; }

 ColumnData(size_t n)            : mBegin(new Scalar[n]), mEnd(mBegin+n), mRefCount(1) { mN = (int)n; assert(mBegin != NULL); }

 public:
  std::string     name()          const { return mName; }
  std::string     role()          const { return mRole; }
  std::string     description()   const { return mDescription; }
  int             size()          const { return mN; }
  Scalar          average()       const { return mAvg; }
  Scalar          scale()         const { return (Scalar) ((mMax - mMin)/(Scalar)6.0); }
  Scalar          min()           const { return mMin; }
  Scalar          max()           const { return mMax; }
  int             num_unique()    const { return mNumUnique; }
  Scalar          element(int i)  const { return *(mBegin+i); }
  Scalar*         begin()         const { return mBegin; }
  Scalar*         end()           const { return mEnd; }
  Ranges::range<Scalar*>       writable_range()  const { return Ranges::make_range(mBegin, mEnd); }
  Ranges::range<Scalar const*> range()           const { return Ranges::make_range(mBegin, mEnd); }
  std::set<Scalar>             unique_elements() const { return mUniqueElements; }
  
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

template <class F>
class Column
{
 private:
  ColumnData<F>       *  mData;            // pointer makes it possible to have const but change ref count

 public:
  ~Column() { if(--mData->mRefCount <= 0) delete mData; }
  
  Column();
  Column(Column<F> const& c);
  Column(char const* name, int n);

  Column(std::string name, std::string description, size_t n, std::istream& is);
  Column(char const* name, char const* description, size_t n, FILE *fp);
  template <class Iter>
    Column(char const* name, char const* description, size_t n, Iter source);
  template<class Iter, class Function>
    Column(std::string name, std::string description, size_t n, Iter iter, Function const& f);
  
  Column& operator= (Column const& c);
  
  ColumnData<F> * operator->()            const { return mData; }
  
  void        init_properties()                 { mData->init_properties(); }
  void        print_to (std::ostream &os) const { os << "Column " ; mData->print_to(os); }

 private:

  std::string  extract_role_from_string(std::string const& desc) const;
  
};

template<class F>
inline
std::ostream&
operator<<(std::ostream& os, Column<F> const& column)
{
  column.print_to(os);
  return os;
}


// Read columns as a stream from a stream input object:
//        First line of stream gives number of observations expected for each column.
//        Seems to work okay so long as this number is >= to the number present.
//        Then come triples of lines for each column: name, description, data
template <class F>
class ColumnStream : public std::iterator<std::forward_iterator_tag, Column<F> >
{
  std::istream&    mStream;
  std::string      mStreamName;
  int              mN;
  int              mCount;
  std::string      mCurrentName;
  std::string      mCurrentDesc;
  Column<F>        mCurrentColumn;
  
 public:
  ~ColumnStream() {  }
  
  ColumnStream (std::istream& is, std::string name)
    :  mStream(is), mStreamName(name), mN(0), mCount(0), mCurrentName(), mCurrentDesc(), mCurrentColumn()
    { if (is) { initialize(); read_next_column(); } }

  std::string      currentName()        const { return mCurrentName; }
  std::string      currentDescription() const { return mCurrentDesc; }
  
  Column<F>        operator*()          const { return mCurrentColumn; }
  ColumnStream<F>& operator++()               { ++mCount; read_next_column(); return *this; }

  int              position()           const { return mCount; }
  int              n()                  const { return mN;     }
  
 private:
  void initialize();
  bool read_next_column();
};

// Return number of observations and number of columns
template<class F>
std::pair<int,int>
  insert_columns_from_stream (std::istream& is,
			      std::back_insert_iterator< std::vector<Column<F>> > it);

// Return collection of column vectors based on named variables
// Names obtained from columnVector field as *first* pair of strings on description line
template<class F>
void
insert_columns_from_stream (std::istream& is,
			    std::map<std::string, std::back_insert_iterator< std::vector<Column<F>> > > insertMap);


// This version makes columns from an input file using C io rather than C++ (faster this way)

template <class F>
class FileColumnStream : public std::iterator<std::forward_iterator_tag, Column<F> >
{
  std::string      mFileName;
  int              mN;
  int              mCount;
  char             mCurrentName[maxColumnNameLength];
  char             mCurrentDesc[maxColumnDescLength];
  Column<F>        mCurrentColumn;
  FILE*            mFile;
  
 public:
  ~FileColumnStream() { if (mFile) fclose(mFile); }
  
  FileColumnStream (std::string const& fileName)
    :
    mFileName(fileName), mN(0), mCount(0), mCurrentName(), mCurrentDesc(), mCurrentColumn(), mFile(0)
    { open_file(); read_next_column_from_file(); }
  
  Column<F>            operator*()  const { return mCurrentColumn; }
  FileColumnStream<F>& operator++()       { ++mCount; read_next_column_from_file(); return *this; }

  int                  position()   const { return mCount; }
  int                  n()          const { return mN;     }
  void                 close_file()       { fclose(mFile); }
  
 private:
  bool open_file();
  bool read_next_column_from_file();
};


template<class F>
std::pair<int,int>
insert_columns_from_file (std::string const& fileName, 
                          std::back_insert_iterator< std::vector<Column<F>> > it);


template<class F>
std::pair<int,int>
insert_columns_from_file (std::string const& fileName, int ny,
                          std::back_insert_iterator< std::vector<Column<F>> > yIt,
                          std::back_insert_iterator< std::vector<Column<F>> > xIt);

template<class F>
int
insert_columns_from_file (FILE *is, std::string const& nameFileName, int nRows,
			  std::back_insert_iterator< std::vector<Column<F>> > it);


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

 IntegerColumnData(size_t n, std::string name = " ")  : mName(name), mSize((int)n), mBegin(new int[n]), mEnd(mBegin+n), mRefCount(1) { assert(mBegin != NULL); }
  
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
  
 IntegerColumn()                                     : mData( new IntegerColumnData(0) ) {  }
 IntegerColumn(Column<double> const& c)              : mData( new IntegerColumnData(c->size(),c->name()) ) { transfer_from_double(c->begin()); }
 IntegerColumn(Column<float>  const& c)              : mData( new IntegerColumnData(c->size(),c->name()) ) { transfer_from_float (c->begin()); }
 IntegerColumn(IntegerColumn const& c)               : mData(c.mData) { ++c.mData->mRefCount; }
  
  IntegerColumnData* operator->()                const { return mData; }

  void print_to(std::ostream& os)                const;
  
 private:
  void transfer_from_double (double *pDouble);
  void transfer_from_float  (float  *pFloat);
};

inline
std::ostream&
operator<<(std::ostream& os, IntegerColumn const& column)
{
  column.print_to(os);
  return os;
}


#endif
