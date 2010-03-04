/*
 *  expert.cc
 *  auctions
 *
 *  Created by Robert Stine on 1/18/08.
 *  Copyright 2008 __MyCompanyName__. All rights reserved.
 *
 */

#include "experts.h"



void 
ExpertABC::payoff (double w)    
{ 
  mBidHistory.append_bid_outcome( (w>0.0) );
  mAlpha += w; 
  if (mAlpha<0.0)
    std::cout << "XPRT:  *** Error. ***   Expert " << name() << " has negative alpha=" << mAlpha << std::endl;
}




Expert&
Expert::operator=(Expert const& e)
{
  if( mpExpert // watch for an empty expert
      && (--mpExpert->mRefCount <= 0) && (mpExpert != e.mpExpert) ) delete mpExpert;
  mpExpert = e.mpExpert;
  mpExpert->mRefCount++;
  return *this;
}
