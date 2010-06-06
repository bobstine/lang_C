#ifndef _GSL_ITERATOR_H_
#define _GSL_ITERATOR_H_

#include "range.h"

#include <gsl/gsl_vector.h>

#include <iterator>


/*
 
  3 Dec 07 ... Handle const vectors, add traits
 26 Jan 04 ... Created

 Begin and end functions defined inline in this file to work
 with ranges.  Code correctly works with a gsl_vector that has
 been defined from rows and columns of a matrix.
  
*/

namespace GSL {
  
class gsl_vector_iterator: public std::iterator<std::random_access_iterator_tag,double>
{
  gsl_vector *mV;
  int         mPosition;
  
public:
  gsl_vector_iterator(gsl_vector *v)
    : mV(v), mPosition(0) { }
  gsl_vector_iterator(gsl_vector_iterator const& it)
    : mV(it.mV), mPosition(it.mPosition) { }

  double&       operator* ()                const { return *gsl_vector_ptr(mV,mPosition); }
  double&       operator[](int n)           const { return *gsl_vector_ptr(mV, n); }
  
  gsl_vector_iterator& operator++()                      { ++mPosition; return *this; }
  gsl_vector_iterator  operator++(int)                   { gsl_vector_iterator it(*this); ++mPosition; return it; }
  gsl_vector_iterator  operator+ (int i)           const { gsl_vector_iterator it(*this); it += i; return it; }
  gsl_vector_iterator& operator+=(int i)                 { mPosition += i; return *this; }
  
  gsl_vector_iterator& operator--()                      { --mPosition; return *this; }
  gsl_vector_iterator  operator--(int)                   { gsl_vector_iterator it(*this); --mPosition; return it; }
  gsl_vector_iterator  operator- (int i)           const { gsl_vector_iterator it(*this); it -= i; return it; }
  gsl_vector_iterator& operator-=(int i)                 { mPosition -= i; return *this; }

  bool   operator< (gsl_vector_iterator const& it) const { return mPosition <  it.mPosition; }
  bool   operator<=(gsl_vector_iterator const& it) const { return mPosition <= it.mPosition; }
  bool   operator> (gsl_vector_iterator const& it) const { return mPosition >  it.mPosition; }
  bool   operator>=(gsl_vector_iterator const& it) const { return mPosition >= it.mPosition; }

  bool   operator!=(gsl_vector_iterator const& it) const { return mV != it.mV || mPosition != it.mPosition; }
  bool   operator==(gsl_vector_iterator const& it) const { return mV == it.mV && mPosition == it.mPosition; }

  std::iterator_traits< gsl_vector_iterator >::difference_type
    operator- (gsl_vector_iterator const& it) const { return mPosition - it.mPosition; }
};



class gsl_vector_const_iterator: public std::iterator<std::random_access_iterator_tag,double>
{
  gsl_vector const* mV;
  int               mPosition;
public:
  gsl_vector_const_iterator(gsl_vector const* v)
    : mV(v), mPosition(0) { }
  gsl_vector_const_iterator(gsl_vector_const_iterator const& it)
    : mV(it.mV), mPosition(it.mPosition) { }
  
  double        operator* ()                const { return *gsl_vector_const_ptr(mV,mPosition); }
  double        operator[](int n)           const { return *gsl_vector_const_ptr(mV, n); }
  
  gsl_vector_const_iterator& operator++()                      { ++mPosition; return *this; }
  gsl_vector_const_iterator  operator++(int)                   { gsl_vector_const_iterator it(*this); ++mPosition; return it; }
  gsl_vector_const_iterator  operator+ (int i)           const { gsl_vector_const_iterator it(*this); it += i; return it; }
  gsl_vector_const_iterator& operator+=(int i)                 { mPosition += i; return *this; }
  
  gsl_vector_const_iterator& operator--()                      { --mPosition; return *this; }
  gsl_vector_const_iterator  operator--(int)                   { gsl_vector_const_iterator it(*this); --mPosition; return it; }
  gsl_vector_const_iterator  operator- (int i)           const { gsl_vector_const_iterator it(*this); it -= i; return it; }
  gsl_vector_const_iterator& operator-=(int i)                 { mPosition -= i; return *this; }
  
  bool   operator< (gsl_vector_const_iterator const& it) const { return mPosition <  it.mPosition; }
  bool   operator<=(gsl_vector_const_iterator const& it) const { return mPosition <= it.mPosition; }
  bool   operator> (gsl_vector_const_iterator const& it) const { return mPosition >  it.mPosition; }
  bool   operator>=(gsl_vector_const_iterator const& it) const { return mPosition >= it.mPosition; }
  
  bool   operator!=(gsl_vector_const_iterator const& it) const { return mV != it.mV || mPosition != it.mPosition; }
  bool   operator==(gsl_vector_const_iterator const& it) const { return mV == it.mV && mPosition == it.mPosition; }
  
  std::iterator_traits< gsl_vector_const_iterator >::difference_type
    operator- (gsl_vector_const_iterator const& it) const { return mPosition - it.mPosition; }
};

// Begin and end functions

inline
gsl_vector_iterator
begin(gsl_vector *pV)
{
  return gsl_vector_iterator(pV);
}

inline
gsl_vector_const_iterator
begin(gsl_vector const* pV)
{
  return gsl_vector_const_iterator(pV);
}


inline
gsl_vector_iterator
end(gsl_vector *pV)
{
  gsl_vector_iterator it(pV);
  it += pV->size;
  return it;
}


inline
gsl_vector_const_iterator
end(gsl_vector const* pV)
{
  gsl_vector_const_iterator it(pV);
  it += pV->size;
  return it;
}

inline
Ranges::range<gsl_vector_iterator>
make_range(gsl_vector *pV)
{
  return Ranges::range<gsl_vector_iterator>(begin(pV), end(pV));
}


inline
Ranges::range<gsl_vector_const_iterator>
make_range(gsl_vector const *pV)
{
  return Ranges::range<gsl_vector_const_iterator>(begin(pV), end(pV));
}


} // namespace

using namespace GSL;

#endif
