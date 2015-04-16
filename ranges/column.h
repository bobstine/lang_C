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
#include <set>
#include <assert.h>

#include <string>
#include <iostream>

#include "read_utils.h"
#include "range.h"
#include "attributes.h"

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
  Attributes  mAttributes;
  int         mN;
  Scalar      mAvg;
  Scalar      mMin, mMax;
  int         mNumUnique;
  std::set<F> mUniqueElements;    // only held if mNumUnique is small
  Scalar *    mBegin;
  Scalar *    mEnd;
  int         mRefCount;
 
 private:
  ~ColumnData()                 { delete[] mBegin; }

 ColumnData(size_t n)            : mBegin(new Scalar[n]), mEnd(mBegin+n), mRefCount(1) { mN = (int)n; assert(mBegin != NULL); }

 public:
  std::string     name()                const { return mName; }
  std::string     role()                const { return mAttributes["role"]; }
  Attributes      attributes()          const { return mAttributes; }
  int             size()                const { return mN; }
  Scalar          average()             const { return mAvg; }
  Scalar          scale()               const { return (Scalar) ((mMax - mMin)/(Scalar)6.0); }
  Scalar          min()                 const { return mMin; }
  Scalar          max()                 const { return mMax; }
  int             num_unique()          const { return mNumUnique; }
  Scalar*         begin()               const { return mBegin; }
  Scalar*         end()                 const { return mEnd; }
  Scalar          element(size_t i)     const { return *(mBegin+i); }
  void            set_element(int i, F value) { *(mBegin+i) = value; }
  
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

  // none of these init the column properties (since no data)
  Column();
  Column(Column<F> const& c);
  Column(std::string name, Attributes const& attr, size_t n);
  Column(std::string name, std::string description, size_t n)  : Column(name, Attributes(description), n) { }
  Column(std::string name, int n)                              : Column(name, Attributes(), n) { }
  // these initialize properties
  Column(std::string name, Attributes const& attr, size_t n, std::istream& is);
  Column(char const* name, char const* description, size_t n, FILE *fp);
  template <class Iter>
    Column(std::string name, Attributes const& attr, size_t n, Iter source);
  template<class Iter, class Function>
    Column(std::string name, Attributes const& attr, size_t n, Iter iter, Function const& f);
  template <class Iter>
    Column(char const* name, char const* description, size_t n, Iter source) : Column(std::string(name), Attributes(description), n, source) { }
  
  Column& operator= (Column const& c);
  
  ColumnData<F> * operator->()            const { return mData; }

  void        init_properties()                 { mData->init_properties(); }
  void        print_to (std::ostream &os) const { os << "Column " ; mData->print_to(os); }
};

template<class F>
inline
std::ostream&
operator<<(std::ostream& os, Column<F> const& column)
{
  column.print_to(os);
  return os;
}


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
