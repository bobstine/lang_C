//  $Id: scalar_iterator.h,v 1.1 2003/11/26 14:11:50 bob Exp $

/*

Scalar iterators decorate a scalar value, such as a double,
  as an iterator that refers to the same value.

  20 Mar ... Created to support extension of range multiply.

*/

#ifndef _SCALAR_ITERATOR_H_
#define _SCALAR_ITERATOR_H_

#include <iterator>

template<typename Scalar>
class scalar_iterator
{
  
  Scalar mX;

public:

  typedef scalar_iterator<Scalar> type_of_this;
  
  scalar_iterator (Scalar x)
    : mX(x) { }

  Scalar operator*() const { return mX; }
  
  scalar_iterator<Scalar>  operator++() { return *this; }
  scalar_iterator<Scalar>  operator--() { return *this; }

  scalar_iterator<Scalar>  operator++(int) {
    scalar_iterator<Scalar> result(*this);    return result;
  }
  scalar_iterator<Scalar>  operator--(int) {
    scalar_iterator<Scalar> result(*this);    return result;
  }

  scalar_iterator<Scalar>  operator+(int)  const  {
    scalar_iterator<Scalar> result(*this);    return result;
  }
  scalar_iterator<Scalar>  operator-(int n)  const  {
    scalar_iterator<Scalar> result(*this);    return result;
  }

  scalar_iterator<Scalar>&   operator+=(int) { return *this; }
  scalar_iterator<Scalar>&   operator-=(int) { return *this; }
 
  int    operator-(const type_of_this* ) { return 0; }

  // We dont want these called... so allow to gen an error.
  // bool operator<(const type_of_this) const { return true; }
  // bool operator>(const type_of_this) const { return false; }
  // bool operator!=(const type_of_this& it) const { return mX != it.mX; }
  // bool operator==(const type_of_this& it) const { return mX == it.mX; }
};


namespace std
{
  template<class Scalar>
    class std::iterator_traits< scalar_iterator<Scalar> > 
    {
    public:
      typedef std::random_access_iterator_tag    iterator_category;
      typedef Scalar                             value_type;
      typedef int                                difference_type;
      typedef Scalar*                            pointer;
      typedef Scalar&                            reference;
    };
}
      

#endif



