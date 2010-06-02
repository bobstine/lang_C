// $Id: bidders.cc,v 3.6 2008/01/20 00:00:41 bob Exp $

#include "bidders.h"

// universal PDF 
#include "coding.h"

#include <math.h>



void
BiddingHistory::print_to (std::ostream & os) const
{
    os << "Auction State:  \n"
       << "    payoffs [" << mPayoffHistory.size()    << "] : ";
    for(std::vector<double>::const_iterator i=mPayoffHistory.begin(); i != mPayoffHistory.end(); ++i)
      os << " " << *i;
    os << std::endl
       << "    accepts [" << mAcceptedFeatures.size() << "] : " << std::endl << mAcceptedFeatures << std::endl
       << "    rejects [" << mRejectedFeatures.size() << "] : " << std::endl << mRejectedFeatures << std::endl;
  }

//  BidHistory    BidHistory    BidHistory    BidHistory    BidHistory    BidHistory    BidHistory   

void               
BidHistory::append_bid_outcome (bool success)     
{ 
  int result ((success) ? 1 : 0); 
  mResults.push_back(result); 
  if (success) 
  { mBidsSinceLastSuccess = 0;
    ++mNumSuccessfulBids;
  }
  else
    ++mBidsSinceLastSuccess;
}

