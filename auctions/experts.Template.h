#ifndef _EXPERTS_TEMPLATE_H_
#define _EXPERTS_TEMPLATE_H_

#include "experts.h"

////     TEMPLATE     TEMPLATE     TEMPLATE     TEMPLATE     TEMPLATE     TEMPLATE     TEMPLATE     TEMPLATE

template<class Bidder, class Stream>
SCALAR
  StreamExpert<Bidder,Stream>::place_bid (BiddingHistory const& state)
{
  debugging::debug("XPRT",4) << name() << " asked to place bid: mAlpha=" << mAlpha << std::endl;
  if ( (mAlpha>0.0) && has_feature() )
  { SCALAR b (mBidder.bid(mLastBidAccepted, mAlpha, mStream, mBidHistory, state)); 
    SCALAR m (max_bid()); 
    mCurrentBid = (b<m) ? b:m;
  }    
  else
    mCurrentBid = 0.0;
  return mCurrentBid;
}


template<class Bidder, class Stream>
void
  StreamExpert<Bidder,Stream>::print_to(std::ostream& os) const
{
  ExpertABC::print_to(os);
  // the rest might be too much
  os << ", stream ";
  mStream.print_to(os);
}

#endif
