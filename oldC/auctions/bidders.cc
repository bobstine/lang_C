// $Id: bidders.cc,v 1.7 2004/04/14 17:48:38 bob Exp $

#include "bidders.h"
#include <math.h>

namespace {
  double min(double a, double b)  { return (a<b) ? a : b; }
}


//  BidderABC  BidderABC  BidderABC  BidderABC  BidderABC  BidderABC  BidderABC  BidderABC  

void
BidderABC::print_to (std::ostream& os) const
{
  os << " " << mName << " with alpha " << mAlpha << ", ";
  mRecommender->print_to(os);
}


//  ConstantBidder  ConstantBidder  ConstantBidder  ConstantBidder  ConstantBidder  ConstantBidder  

double
ConstantBidder::bid_amount () const
{
  double nLeft (recommender()->number_remaining());
  return alpha()/nLeft;
}


//  GeometricBidder  GeometricBidder  GeometricBidder  GeometricBidder  GeometricBidder  GeometricBidder  

double
GeometricBidder::bid_amount() const
{
  return mRate * alpha();
}


//  BidCollector  BidCollector  BidCollector  BidCollector  BidCollector  BidCollector  BidCollector

double
BidCollector::operator()(BidderABC* bidder)
{
  if (bidder->has_bid())
  { BidderABC::Bid bid(bidder->bid());
    mAmounts[bid.first] += bid.second;
    mBidders[bid.first].push_back(bidder);
    return bid.second;
  }
  else
  { std::cout << "BDDR: Bidder " << bidder->name() << " does not have a bid.\n";
    return 0.0;
  }
}

BidderABC::Bid
BidCollector::high_bid () const
{ double maxBid(0.0);
  BidderABC::FeaturePtr f(0) ;
  for (AmountMap::const_iterator it=mAmounts.begin(); it != mAmounts.end(); ++it)
    if (it->second > maxBid)
    { maxBid = it->second;
      f = it->first;
    }
  return std::make_pair(f,maxBid);
}

BidCollector::BidderVector
BidCollector::not_bidders_on_feature(BidderABC::FeaturePtr f) const
{
  BidderVector bv;
  for (BidderMap::const_iterator it=mBidders.begin(); it != mBidders.end(); ++it)
    if (it->first != f)
      std::copy(it->second.begin(), it->second.end(), std::back_inserter(bv));
  return bv;
}
