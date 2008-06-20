// $Id: cyclic.cc,v 1.1 2000/02/04 19:36:03 bob Exp $-*- c++ -*-

#include <vector.h>

#include "cyclic.h"

Cyclic_iterator& Cyclic_iterator::operator++()
{
  ++mCurrent;
  if (mCurrent == mEnd) mCurrent = mBegin;
  return *this;
}


double& Cyclic_iterator::operator*()
{
  return *mCurrent;
}

///////////////////////////  EOF  /////////////////////////////////
