////     TEMPLATE     TEMPLATE     TEMPLATE     TEMPLATE     TEMPLATE     TEMPLATE     TEMPLATE     TEMPLATE 


template<class Bidder, class Stream>
double
  StreamExpert<Bidder,Stream>::place_bid (AuctionState const& state)
{
  //  debugging::debug(0) << "XPRT: " << name() << " gets bid: mAlpha=" << mAlpha << std::endl;
  if ( (mAlpha>0.0) && (has_feature(state))  )
  { double b (mBidder.bid(mLastBidAccepted, mAlpha, mStream, mBidHistory, state)); 
    double m (max_bid()); 
    mCurrentBid = (b<m) ? b:m;
  }    
  else
    mCurrentBid = 0.0;
  return mCurrentBid;
}
