// -*- c++ -*-

#ifndef _COLUMN_TEMPLATE_H_
#define _COLUMN_TEMPLATE_H_

#include "column.h"
#include "debug.h"

#include <map>

#include <cstdlib>
#include <cstdio>
#include <cstring>

#include <iostream>

using debugging::debug;

//   Column   Column   Column   Column   Column   Column   Column   Column   Column   Column   Column   Column

template<class F>
Column<F>::Column()
  : mData( new ColumnData<F>(0) ) {  }

template<class F>
Column<F>::Column(Column<F> const& c)
  : mData(c.mData)  { ++c.mData->mRefCount; }


template <class F>
Column<F>::Column(std::string name, Attributes const& attr, size_t n)
  : mData( new ColumnData<F>(n) )
{
  mData->mName = name;
  mData->mAttributes = attr;
}


template<class F>  inline  std::string scan_string(F const);
template<>         inline  std::string scan_string(double const) { return "%lf"; }
template<>         inline  std::string scan_string(float const) { return "%f"; }


template<class F>
Column<F>::Column(std::string name, Attributes const& attr, size_t n, std::istream& is) : mData( new ColumnData<F>(n) )
{
  mData->mName = name;
  mData->mAttributes = attr;
  F *x (mData->mBegin);
  while(n--)
  { is >> *x;
    ++x;
  }
  std::string restOfLine;
  std::getline (is,restOfLine);  // flush rest of line
  mData->init_properties();
}

template<class F>
Column<F>::Column(char const* name, char const* description, size_t n, FILE *fp)
: mData( new ColumnData<F>(n) )
{
  mData->mName = name;
  mData->mAttributes = Attributes( description );
  F *x (mData->mBegin);
  int result = 0;
  std::string str = scan_string(*x);
  const char* format = str.c_str();
  while(n--)
    result = fscanf(fp, format, x++);   // watch for this one!
  if (result != 1)
  { std::cerr << "CLMN: *** ERROR *** Could not read sought column element with n=" << n << std::endl;
    std::cerr << "                    Last values read are " << *(x-2) << ", " << *(x-1) << ", " << (*x) << std::endl;
  }
  mData->init_properties();
}



template <class F>
template <class Iter>
Column<F>::Column(std::string name, Attributes const& attr, size_t n, Iter it)
  : Column<F>(name, attr, n)
{
  F *x (mData->mBegin);
  while(n--)
  { *x++ = *it;
    ++it;
  }
  mData->init_properties();
}


template<class F>
template <class Iter, class Function>
Column<F>::Column(std::string name, Attributes const& attr, size_t n, Iter iter, Function const& func) : mData( new ColumnData<F>(n) )
{
  mData->mName = name;
  mData->mAttributes = attr;
  F *x (mData->mBegin);
  while(n--) {
    *x++ = func(*iter);
    ++iter;
  }
  mData->init_properties();
}


template <class F>
Column<F>& 
Column<F>::operator= (Column<F> const& c)
{
  --mData->mRefCount;
  if(mData->mRefCount <= 0 && mData != c.mData) delete  mData;    // delete if not the same
  mData = c.mData;
  ++mData->mRefCount;
  return *this;
}

//  ColumnData    ColumnData    ColumnData    ColumnData    ColumnData    ColumnData    ColumnData    ColumnData

template <class F>
void
ColumnData<F>::print_to (std::ostream &os) const
{ 
  F *x (mBegin);
  int     n (mN);
  os << mName << "  [" << mAttributes <<";  "
     << mNumUnique << "/" << n << ", " << mMin << "<" << mAvg << "<" << mMax <<  "]   {";
  if (mUniqueElements.size() > 0)
  { os << "unique: "; 
    for (auto x : mUniqueElements)
      os << " " << x ;
  }
  else
  { os << "elemts: "
       << x[0] << ", " << x[1] << ", " << x[2] << ", ..., " << x[n-1];
  }
  os << "}";
}


template <class F>
void
ColumnData<F>::init_properties ()
{
  if ((!mBegin) || (mN==0)) return;         // nothing to do 
  
  F *x = mBegin;
  std::set<F> uniq;
  double sum = 0.0;
  mMin = mMax = *x;
  while (x != end())
  { sum += *x;
    if (*x > mMax)
      mMax = *x;
    else if (*x < mMin)
      mMin = *x;
    uniq.insert(*x);
    ++x;
  }
  mAvg = (F)(sum/(double)mN);
  mNumUnique = (int) uniq.size();
  if (mNumUnique < 10)               // save the set if count is small
    mUniqueElements = uniq;
}



#endif
