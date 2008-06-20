// $Id: cyclic_iterator.cc,v 1.3 2005/12/07 03:49:40 bob Exp $-*- c++ -*-

#include <vector>

#include "cyclic_iterator.h"

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
