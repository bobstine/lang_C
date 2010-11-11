/*
 *  feature_stream.cc
 *  auctions
 *
 *  Created by Robert Stine on 1/18/08.
 *  Copyright 2008. All rights reserved.
 *
 */

#include "feature_streams.h"


//  LagIterator     LagIterator     LagIterator     LagIterator     LagIterator     LagIterator     LagIterator

LagIterator&
LagIterator::operator++()
{
  ++mLag; --mRemaining;
  if ((mLag > mMaxLag) && (mRemaining>0))
    mLag = 1;  // go around again
  return *this;
}



