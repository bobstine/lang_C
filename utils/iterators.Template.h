#include <vector>

#include "iterators.h"

template <class T>
cyclic_iterator<T>&
cyclic_iterator<T>::operator++() 
{
  ++mCurrent;
  if (mCurrent == mEnd) mCurrent = mBegin;
  return *this;
}

template <class T>
T
cyclic_iterator<T>::operator*() const
{
  return *mCurrent;
}
