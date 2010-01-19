#include <vector>

#include "iterators.h"

cyclic_iterator& cyclic_iterator::operator++() 
{
  ++mCurrent;
  if (mCurrent == mEnd) mCurrent = mBegin;
  return *this;
}


double& cyclic_iterator::operator*() const
{
  return *mCurrent;
}

///////////////////////////  EOF  /////////////////////////////////
