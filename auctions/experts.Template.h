////     TEMPLATE     TEMPLATE     TEMPLATE     TEMPLATE     TEMPLATE     TEMPLATE     TEMPLATE     TEMPLATE 


template<class Bidder, class Stream>
double
  StreamExpert<Bidder,Stream>::place_bid (BiddingHistory const& state)
{
  debugging::debug("XPRT",4) << name() << " gets bid: mAlpha=" << mAlpha << std::endl;
  if ( (mAlpha>0.0) && has_feature() )
  { double b (mBidder.bid(mLastBidAccepted, mAlpha, mStream, mBidHistory, state)); 
    double m (max_bid()); 
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
  // os << " -- ";  mStream.print_to(os);  // thought this was too much
}
