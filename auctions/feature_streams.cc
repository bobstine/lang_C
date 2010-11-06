/*
 *  feature_stream.cc
 *  auctions
 *
 *  Created by Robert Stine on 1/18/08.
 *  Copyright 2008. All rights reserved.
 *
 */

#include "feature_streams.h"


//  BaseStream     BaseStream     BaseStream     BaseStream     BaseStream     BaseStream     BaseStream


//  LagIterator     LagIterator     LagIterator     LagIterator     LagIterator     LagIterator     LagIterator

LagIterator&
LagIterator::operator++()
{
  ++mLag; --mRemaining;
  if ((mLag > mMaxLag) && (mRemaining>0))
    mLag = 1;  // go around again
  return *this;
}







//   PolynomialStream     PolynomialStream     PolynomialStream     PolynomialStream     PolynomialStream     PolynomialStream     PolynomialStream     


//  Cross-product stream    Cross-product stream    Cross-product stream    Cross-product stream    Cross-product stream

bool
CrossProductStream::can_build_more_features(FeatureList const& accepted, FeatureList const& rejected) const
{
  if (accepted.size() == rejected.size())
    return false;
  else
    return true;
}



void
CrossProductStream::update_iterators ()
{

}



void
CrossProductStream::build_next_feature()
{
  /*  debugging::debug("CPST",3) << name() << std::endl;
  while (false)
  { Feature  xs (*mSlowIterator);
    Feature  xf (*mFastIterators[0]);
    Feature candidate(xs,xf);
    if ( (!candidate->is_constant()) &&
	 (!found_name_among_features(feature_name(), "model features")) )
    { set_head(candidate);
      return;
    }
  }
  */
}


